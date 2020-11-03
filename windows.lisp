#|
 This file is a part of file-notify
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-notify)

(define-condition windows-failure (failure com:win32-error)
  ())

(cffi:defbitfield (filter :uint32)
  (:rename-file      #x00000001)
  (:rename-directory #x00000002)
  (:move             #x00000003)
  (:create           #x00000003)
  (:delete           #x00000003)
  (:attribute        #x00000004)
  (:size             #x00000008)
  (:modify           #x00000010)
  (:security         #x00000100)
  (:all              #x0000011F))

(cffi:defcenum (action :uint32 :allow-undeclared-values T)
  (:create 1)
  :delete
  :modify
  :moved-from
  :moved-to
  :stream-create
  :stream-delete
  :stream-modify
  :id-not-tunnelled
  :tunnelled-id-collision)

(cffi:defcenum (wait-result :uint32 :allow-undeclared-values T)
  (:abandoned #x80)
  (:io-completion #xC0)
  (:timeout #x102)
  (:failed #xFFFFFFFF))

(cffi:defcstruct (event :conc-name event-)
  (next-entry :uint32)
  (action action)
  (length :uint32)
  (name :uint16 :count 259))

(cffi:defcfun (first-change "FindFirstChangeNotificationW") :pointer
  (path :pointer)
  (watch-subtree :bool)
  (filter filter))

(cffi:defcfun (next-change "FindNextChangeNotification") :bool
  (handle :pointer))

(cffi:defcfun (close-change "FindCloseChangeNotification") :bool
  (handle :pointer))

(cffi:defcfun (read-changes "ReadDirectoryChangesW") :bool
  (handle :pointer)
  (buffer :pointer)
  (length :uint32)
  (watch-subtree :bool)
  (filter filter)
  (bytes-returned :pointer)
  (overlapped :pointer)
  (completion-routien :pointer))

(cffi:defcfun (wait-for-multiple-objects "WaitForMultipleObjects") wait-result
  (count :uint32)
  (handles :pointer)
  (all :bool)
  (milliseconds :uint32))

(defmacro check-last-error (predicate &body cleanup)
  `(unless ,predicate
     ,@cleanup
     (let ((errno (org.shirakumo.com-on.cffi:get-last-error)))
       (com:win32-error errno :type 'windows-failure))))

(defvar *watches* (make-hash-table :test 'equal))

(define-implementation init ())

(defstruct (watch (:constructor make-watch (handle table)))
  handle table)

(define-implementation shutdown ()
  (loop for v being the hash-values of *watches*
        do (close-change (watch-handle v)))
  (clrhash *watches*))

(define-implementation watch (file/s &key (events T))
  (let ((filter (case events
                  ((T) :all)
                  ((NIL) ())
                  (T events))))
    (flet ((add (path)
             (with-simple-restart (abort "Don't watch ~a" path)
               (let* ((path (truename path))
                      (dir (make-pathname :name NIL :type NIL :defaults path))
                      (existing (gethash dir *watches*)))
                 (unless existing
                   (com:with-wstring (path (namestring dir))
                     (let ((handle (first-change path NIL filter)))
                       (check-last-error (/= #+64-bit #xFFFFFFFFFFFFFFFF #-64-bit #xFFFFFFFF
                                             (cffi:pointer-address handle)))
                       (setf existing (make-watch handle (make-hash-table :test 'equal)))
                       (setf (gethash dir *watches*) existing))))
                 (setf (gethash path (watch-table existing)) T)))))
      (if (listp file/s)
          (mapc #'add file/s)
          (add file/s)))))

(define-implementation list-watched ()
  (let ((watched ()))
    (loop for v being the hash-values of *watches*
          do (loop for k being the hash-keys of (watch-table v)
                   do (push k watched)))
    watched))

(define-implementation unwatch (file/s)
  (flet ((del (path)
           (let* ((path (truename path))
                  (dir (make-pathname :name NIL :type NIL :defaults path))
                  (existing (gethash dir *watches*)))
             (when existing
               (remhash path (watch-table existing))
               (when (= 0 (hash-table-count (watch-table existing)))
                 (check-last-error (close-change (watch-handle existing)))
                 (remhash dir *watches*))))))
    (if (listp file/s)
        (mapc #'del file/s)
        (del file/s))))

(defun process (handle directory function)
  (cffi:with-foreign-objects ((event '(:struct event))
                              (written :uint32))
    (check-last-error (read-changes handle event (cffi:foreign-type-size '(:struct event)) NIL :all
                                    written (cffi:null-pointer) (cffi:null-pointer)))
    (let ((path (merge-pathnames (com:wstring->string (cffi:foreign-slot-pointer event '(:struct event) 'name)
                                                      (event-length event))
                                 directory))
          (action (event-action event)))
      (when (keywordp action)
        (with-simple-restart (abort "Don't notify the change to ~a" path)
          (funcall function path action))))
    (check-last-error (next-change handle))))

(define-implementation process-events (function &key timeout)
  (let* ((tsec (etypecase timeout
                 ((eql NIL) 0)
                 ((eql T) 0.5)
                 ((real 0) timeout)))
         (msec (floor (* 1000 tsec)))
         (count (hash-table-count *watches*))
         (dirs (make-array count)))
    (when (<= #x80 count)
      (error "FIXME: Too many watches. Don't know how to deal with this."))
    (cffi:with-foreign-objects ((handles :pointer count))
      (loop for i from 0
            for directory being the hash-keys of *watches*
            for watch being the hash-values of *watches*
            do (setf (cffi:mem-aref handles :pointer i) (watch-handle watch))
               (setf (aref dirs i) directory))
      (loop for ev = (wait-for-multiple-objects count handles NIL msec)
            do (case ev
                 ((:abandoned :io-completion))
                 (:timeout
                  (unless (eql T timeout)
                    (return)))
                 (:failed
                  (check-last-error NIL))
                 (T
                  (when (< ev #x80)
                    (process (cffi:mem-aref handles :pointer ev) (aref dirs ev) function))))))))
