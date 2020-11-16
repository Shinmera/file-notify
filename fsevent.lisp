#|
 This file is a part of file-notify
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-notify)

(define-condition fsevent-failure (failure)
  ((function-name :initarg :function-name :initform NIL :reader function-name)
   (message :initarg :message :initform NIL :reader message))
  (:report (lambda (c s) (format s "The call ~@[to~%  ~a~%~]failed.~@[~%  ~a~]"
                                 (function-name c) (message c)))))

(cffi:define-foreign-library corefoundation
  (T (:framework "CoreFoundation")))

(cffi:define-foreign-library coreservices
  (T (:framework "CoreServices")))

(cffi:defctype size #+64-bit :uint64 #-64-bit :uint32)

(cffi:defcenum (run-loop-result :int32)
  (:finished 1)
  (:stopped 2)
  (:timed-out 3)
  (:handled-source 4))

(cffi:defbitfield (create-flags :uint32)
  (:none #x0000)
  (:use-cftypes #x0001)
  (:no-defer #x0002)
  (:watch-root #x0004)
  (:ignore-self #x0008)
  (:file-events #x0010)
  (:mark-self #x0020)
  (:use-extended-data #x0040)
  (:full-history #x0080))

(cffi:defbitfield (event-flag :uint32)
  (:none #x00000000)
  (:must-scan-subdirs #x00000001)
  (:user-dropped #x00000002)
  (:kernel-dropped #x00000004)
  (:event-ids-wrapped #x00000008)
  (:history-done #x00000010)
  (:root-changed #x00000020)
  (:mount #x00000040)
  (:unmount #x00000080)
  (:item-created #x00000100)
  (:item-removed #x00000200)
  (:item-inode-meta-mod #x00000400)
  (:item-renamed #x00000800)
  (:item-modified #x00001000)
  (:item-finder-info-mod #x00002000)
  (:item-change-owner #x00004000)
  (:item-xattr-mod #x00008000)
  (:item-is-file #x00010000)
  (:item-is-dir #x00020000)
  (:item-is-symlink #x00040000)
  (:own-event #x00080000)
  (:item-is-hardlink #x00100000)
  (:item-is-last-hardlink #x00200000)
  (:item-cloned #x00400000))

(cffi:defcenum (string-encoding :uint32)
    (:utf-8 #x08000100))

(cffi:defcfun (%release "CFRelease") :void
  (object :pointer))

(cffi:defcfun (%create-array "CFArrayCreate") :pointer
  (allocator :pointer)
  (values :pointer)
  (count :long)
  (callbacks :pointer))

(cffi:defcfun (%create-string "CFStringCreateWithCString") :pointer
  (allocator :pointer)
  (string :string)
  (encoding string-encoding))

(cffi:defcfun (string-get-length "CFStringGetLength") :long
  (string :pointer))

(cffi:defcfun (string-get-cstring "CFStringGetCString") :bool
  (string :pointer)
  (buffer :pointer)
  (length :long)
  (encoding string-encoding))

(cffi:defcfun (cfstr "__CFStringMakeConstantString") :pointer
  (string :string))

(cffi:defcfun (run-loop "CFRunLoopRunInMode") run-loop-result
  (mode :pointer)
  (seconds :double)
  (return-after-source-handled :bool))

(cffi:defcfun (current-run-loop "CFRunLoopGetCurrent") :pointer)

(cffi:defcfun (create-event-stream "FSEventStreamCreate") :pointer
  (allocator :pointer)
  (callback :pointer)
  (context :pointer)
  (paths :pointer)
  (since :uint64)
  (latency :double)
  (flags create-flags))

(cffi:defcfun (schedule-with-run-loop "FSEventStreamScheduleWithRunLoop") :void
  (stream :pointer)
  (run-loop :pointer)
  (mode :pointer))

(cffi:defcfun (start-event-stream "FSEventStreamStart") :void
  (stream :pointer))

(cffi:defcfun (stop-event-stream "FSEventStreamStop") :void
  (stream :pointer))

(cffi:defcfun (invalidate-event-stream "FSEventStreamInvalidate") :void
  (stream :pointer))

(cffi:defcfun (release-event-stream "FSEventStreamRelease") :void
  (stream :pointer))

(cffi:defcfun (flush-event-stream "FSEventStreamFlushAsync") :uint64
  (stream :pointer))

(defconstant now-id #xFFFFFFFFFFFFFFFF)

(defun release (&rest objects)
  (dolist (object objects)
    (unless (cffi:null-pointer-p object)
      (%release object))))

(defmacro check-null (form)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,form))
       (if (cffi:null-pointer-p ,value)
           (error 'fsevent-failure :function-name ',(first form))
           ,value))))

(defun create-array (entries)
  (let ((count (length entries)))
    (cffi:with-foreign-object (data :pointer count)
      (loop for i from 0
            for entry in entries
            do (setf (cffi:mem-aref data :pointer i) entry))
      (check-null (%create-array (cffi:null-pointer) data count (cffi:null-pointer))))))

(defun create-string (string)
  (check-null (%create-string (cffi:null-pointer) string :utf-8)))

(defmacro with-cf-objects (bindings &body body)
  `(let ,(loop for binding in bindings
               collect (list (first binding) `(cffi:null-pointer)))
     (unwind-protect
          (progn
            ,@(loop for (name init) in bindings
                    collect `(setf ,name ,init))
            ,@body)
       (release ,@(nreverse (mapcar #'first bindings))))))

(defmacro define-lazy-constant (name init)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let (value)
       (defun ,name ()
         (or value (setf value ,init)))
       (define-symbol-macro ,name (,name)))))

(defvar *watches* (make-hash-table :test 'equal))
(defvar *last-id*)
(defvar *callback*)

(define-lazy-constant default-run-loop-mode (cfstr "kCFRunLoopDefaultMode"))

(define-implementation init (&key)
  (unless (boundp '*last-id*)
    (cffi:use-foreign-library corefoundation)
    (cffi:use-foreign-library coreservices)
    (setf *last-id* now-id)))

(define-implementation shutdown ()
  (makunbound *last-id*))

(define-implementation watch (file/s &key events)
  (declare (ignore events))
  (flet ((add (path)
           (setf (gethash (namestring (truename path)) *watches*) T)))
    (if (listp file/s)
        (mapc #'add file/s)
        (add file/s))))

(define-implementation list-watched ()
  (loop for k being the hash-keys of *watches*
        collect k))

(define-implementation unwatch (file/s)
  (flet ((del (path)
           (remhash (namestring (truename path)) *watches*)))
    (if (listp file/s)
        (mapc #'del file/s)
        (del file/s))))

(define-implementation process-events (function &key timeout)
  (init)
  (let* ((*callback* function)
         (entries (mapcar #'create-string (list-watched)))
         (tsec (float (case timeout
                        ((T) 0.5)
                        ((NIL) 0.0)
                        (T timeout))
                      0d0)))
    (unwind-protect
         (with-cf-objects ((array (create-array entries)))
           (let ((stream (create-event-stream (cffi:null-pointer)
                                              (cffi:callback fsevent)
                                              (cffi:null-pointer)
                                              array
                                              *last-id*
                                              0.01d0
                                              '(:watch-root :file-events))))
             (unwind-protect
                  (progn
                    (schedule-with-run-loop stream (current-run-loop) default-run-loop-mode)
                    (start-event-stream stream)
                    (loop
                       (flush-event-stream stream)
                       (run-loop default-run-loop-mode tsec T)
                       (unless (eql T timeout)
                         (return))))
               (stop-event-stream stream)
               (invalidate-event-stream stream)
               (release-event-stream stream))))
      (mapc #'release entries))))

(defun flag->event (flag)
  (case flag
    (:item-created :create)
    (:item-removed :delete)
    (:item-inode-meta-mod :attribute)
    (:item-renamed :move)
    (:item-modified :modify)
    (:item-finder-info-mod :attribute)
    (:item-change-owner :attribute)
    (:item-xattr-mod :attribute)))

(cffi:defcallback fsevent :void ((stream :pointer) (data :pointer) (event-count size) (paths :pointer) (flags :pointer) (ids :pointer))
  (loop for i from 0 below event-count
        for path = (cffi:mem-aref paths :string i)
        for event = (cffi:mem-aref flags 'event-flag i)
        for id = (cffi:mem-aref ids :uint64 i)
        do (dolist (type event)
             (let ((event (flag->event type)))
               (when event
                 (funcall *callback* path event))))
           (setf *last-id* id)))
