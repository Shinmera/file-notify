(in-package #:org.shirakumo.file-notify)

;; FreeBSD has Linux compatibility layer in libinotify.so available via ports.
#+freebsd
(cffi:define-foreign-library libinotify
  (:freebsd (:or "libinotify.so")))

(define-condition inotify-failure (failure)
  ((function-name :initarg :function-name :initform NIL :reader function-name)
   (code :initarg :code :reader code)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The call ~@[to~%  ~a~%~]returned with unexpected result code ~a.~@[~%  ~a~]"
                                 (function-name c) (code c) (message c)))))

(cffi:defbitfield init-flags
  (:non-block     #x800)
  (:close-on-exec #x80000))

(cffi:defbitfield (mask :uint32)
  (:access        #x00000001)
  (:modify        #x00000002)
  (:attribute     #x00000004)
  (:close-write   #x00000008)
  (:close-nowrite #x00000010)
  (:close         #x00000018)
  (:open          #x00000020)
  (:moved-from    #x00000040)
  (:moved-to      #x00000080)
  (:move          #x000000c0)
  (:create        #x00000100)
  (:delete        #x00000200)
  (:delete-self   #x00000400)
  (:move-self     #x00000800)
  (:all           #x00000FFF)
  (:unmount       #x00002000)
  (:q-overflow    #x00004000)
  (:ignored       #x00008000)
  (:only-dir      #x01000000)
  (:dont-follow   #x02000000)
  (:excl-unlink   #x04000000)
  (:mask-create   #x10000000)
  (:mask-add      #x20000000)
  (:dir           #x40000000)
  (:oneshot       #x80000000))

(cffi:defbitfield (poll-event :short)
  (:in  #x001)
  (:pri #x002)
  (:out #x004))

(cffi:defctype ssize #+64-bit :int64 #-64-bit :int32)
(cffi:defctype size #+64-bit :uint64 #-64-bit :uint32)

(cffi:defcstruct (event :conc-name event-)
  (watch :int)
  (mask mask)
  (cookie :uint32)
  (length :uint32)
  (name :char :count 256))

(cffi:defcstruct (pollfd :conc-name pollfd-)
  (fd :int)
  (events poll-event)
  (revents poll-event))

(cffi:defcfun (inotify-init "inotify_init1") :int
  (flags init-flags))

(cffi:defcfun (inotify-close "close") :int
  (fd :int))

(cffi:defcfun (inotify-read "read") ssize
  (fd :int)
  (buf :pointer)
  (count size))

(cffi:defcfun (inotify-add-watch "inotify_add_watch") :int
  (inotify :int)
  (pathname :string)
  (mask mask))

(cffi:defcfun (inotify-rm-watch "inotify_rm_watch") :int
  (inotify :int)
  (watch :int))

(cffi:defcfun (poll "poll") :int
  (pollfds :pointer)
  (n :int)
  (timeout :int))

(cffi:defcfun (error-message "strerror") :string
  (errno :int64))

(cffi:defcvar (errno "errno") :int64)

(defvar *fd* NIL)
(defvar *path->watch* (make-hash-table :test 'equal))
(defvar *watch->path* (make-hash-table :test 'eql))

(declaim (inline inotify-failure))
(defun inotify-failure (code &key function-name message)
  (error 'inotify-failure :code code :function-name function-name
                          :message (or message (error-message code))))

(defmacro check-errno (predicate &body errno-cases)
  `(unless ,predicate
     (let ((code errno))
       (case code
         ,@errno-cases
         (T (inotify-failure code))))))

(define-implementation init (&key flags)
  #+freebsd
  (unless (cffi:foreign-library-loaded-p 'libinotify)
    (cffi:load-foreign-library 'libinotify))
  (unless *fd*
    (let ((fd (inotify-init flags)))
      (check-errno (<= 0 fd))
      (setf *fd* fd))))

(define-implementation shutdown ()
  (when *fd*
    (inotify-close *fd*)
    (setf *fd* NIL)
    (clrhash *path->watch*)
    (clrhash *watch->path*))
  #+freebsd
  (when (cffi:foreign-library-loaded-p 'libinotify)
    (cffi:close-foreign-library 'libinotify)))

(define-implementation watch (file/s &key (events T))
  (init)
  (let ((events (case events
                  ((T) '(:all))
                  ((NIL) ())
                  (T events))))
    (when (member :move events)
      (setf events (union events '(:move :moved-from :moved-to :move-self))))
    (when (member :delete events)
      (setf events (union events '(:delete :delete-self :unmount :ignored))))
    (flet ((add (file)
             (let ((file (namestring (truename file))))
               (unless (gethash file *path->watch*)
                 (let ((watch (inotify-add-watch *fd* file events)))
                   (check-errno (<= 0 watch))
                   (setf (gethash file *path->watch*) watch)
                   (setf (gethash watch *watch->path*) file))))))
      (if (listp file/s)
          (mapc #'add file/s)
          (add file/s)))))

(define-implementation list-watched ()
  (loop for k being the hash-keys of *path->watch*
        collect k))

(define-implementation unwatch (file/s)
  (flet ((del (file)
           (let* ((file (namestring (truename file)))
                  (watch (or (gethash file *path->watch*)
                             (error "File was not being watched."))))
             (inotify-rm-watch *fd* watch)
             (remhash file *path->watch*)
             (remhash watch *watch->path*))))
    (if (listp file/s)
        (mapc #'del file/s)
        (del file/s))))

(defun handle-event (path watch event)
  (case event
    ((:ignored :delete-self :unmount)
     (remhash path *path->watch*)
     (remhash watch *watch->path*))))

(defun map-type (type)
  (case type
    ((:delete-self :ignored :unmount) :delete)
    (:move-self :move)
    (T type)))

(defun process (function)
  (cffi:with-foreign-object (event '(:struct event))
    (let ((read (inotify-read *fd* event (cffi:foreign-type-size '(:struct event))))
          (event event))
      (check-errno (< 0 read))
      (loop for path = (gethash (event-watch event) *watch->path*)
            for types = (event-mask event)
            for size = (- (cffi:foreign-type-size '(:struct event)) 256)
            while (<= size read)
            do (cond ((null path))
                     ((< 0 (event-length event))
                      (let ((file (string-right-trim
                                   '(#\Null)
                                   (cffi:foreign-string-to-lisp
                                    (cffi:foreign-slot-pointer event '(:struct event) 'name)
                                    :count (event-length event)))))
                        (incf size (event-length event))
                        (dolist (type types)
                          (funcall function (merge-pathnames file path) type))))
                     (T
                      (dolist (type types)
                        (handle-event path (event-watch event) type)
                        (funcall function path (map-type type)))))
               (cffi:incf-pointer event size)
               (decf read size)))))

(define-implementation process-events (function &key timeout)
  (init)
  (let* ((tsec (etypecase timeout
                 ((eql T) 0.5)
                 ((eql NIL) 0)
                 ((real 0) timeout)))
         (msec (floor (* 1000 tsec))))
    (cffi:with-foreign-objects ((pollfd '(:struct pollfd)))
      (setf (pollfd-fd pollfd) *fd*)
      (setf (pollfd-events pollfd) :in)
      (setf (pollfd-revents pollfd) 0)
      (loop for poll = (poll pollfd 1 msec)
            while *fd*
            do (unless (check-errno (<= 0 poll)
                         (4
                          ;; Syscall was interrupted.
                          ;; Wait for a bit then try again.
                          (sleep 0.01)
                          T))
                 (when (< 0 poll)
                   (process function))
                 (unless (eql T timeout)
                   (return))
                 (finish-output))))))
