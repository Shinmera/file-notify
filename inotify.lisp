#|
 This file is a part of file-notify
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-notify)

(cffi:defbitfield init-flags
  (:non-block     #x800)
  (:close-on-exec #x80000))

(cffi:defbitfield (mask :uint32)
  (:access        #x00000001)
  (:modify        #x00000002)
  (:attrib        #x00000004)
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

(cffi:defctype ssize #+64-bit :int64 #-64-bit :int32)
(cffi:defctype size #+64-bit :uint64 #-64-bit :uint32)

(cffi:defcstruct (event :conc-name event-)
  (watch :int)
  (mask :uint32)
  (cookie :uint32)
  (length :uint32)
  (name :char))

(cffi:defcstruct (pollfd :conc-name pollfd-)
  (fd fd)
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

(defvar *fd* NIL)
(defvar *watches* (make-hash-table :test 'equal))

(define-implementation init (&key flags)
  (unless *fd*
    (let ((fd (inotify-init flags)))
      (check-errno (<= 0 fd))
      (setf *fd* fd))))

(define-implementation shutdown ()
  (when *fd*
    (close *fd*)
    (setf *fd* NIL)))

(define-implementation watch (file/s &key (events T))
  (flet ((add (file)
           (let ((watch (inotify-add-watch *fd* (namestring file) events)))
             (check-errno (<= 0 watch))
             (setf (gethash file *watches*) watch))))
    (if (listp file/s)
        (mapc #'add file/s)
        (add file/s))))

(define-implementation list-watched ()
  (loop for k being the hash-keys of *watches*
        collect k))

(define-implementation unwatch (file/s)
  (flet ((del (file)
           (inotify-rm-watch *fd* (or (gethash file *watches*)
                                      (error "File was not being watched.")))
           (remhash file *watches*)))
    (if (listp file/s)
        (mapc #'del file/s)
        (del file/s))))

(defun process (function)
  (cffi:with-foreign-object (event '(:struct event))
    (let ((read (read *fd* event (cffi:foreign-type-size '(:struct event)))))
      (check-errno (< 0 read))
      ())))

(define-implementation poll (function &key timeout)
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
            do (check-errno (<= 0 poll))
               (when (< 0 poll)
                 (process function))
               (unless (eql T timeout)
                 (return))
               (finish-output)))))
