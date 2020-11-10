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

(cffi:defbitfield (access-flag :uint32)
  (:directory #x1)
  (:read #x80000000))

(cffi:defbitfield (share-flag :uint32)
  (:none #x0)
  (:read #x1)
  (:write #x2)
  (:delete #x4))

(cffi:defcenum (disposition-flag :uint32)
  (:new 1)
  (:create 2)
  (:existing 3)
  (:always 4)
  (:truncate 5))

(cffi:defbitfield (create-flag :uint32)
  (:normal #x80)
  (:backup #x02000000)
  (:overlapped #x40000000))

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

(cffi:defcstruct (overlapped :conc-name overlapped-)
  (internal #+64-bit :uint64 #-64-bit :uint32)
  (internal-high #+64-bit :uint64 #-64-bit :uint32)
  (offset :uint32)
  (offset-high :uint32)
  (handle :pointer))

(cffi:defcfun (overlapped-result "GetOverlappedResult") :bool
  (handle :pointer)
  (overlapped :pointer)
  (bytes :pointer)
  (wait :bool))

(cffi:defcfun (cancel-io "CancelIo") :bool
  (handle :pointer))

(cffi:defcfun (first-change "FindFirstChangeNotificationW") :pointer
  (path :pointer)
  (watch-subtree :bool)
  (filter filter))

(cffi:defcfun (next-change "FindNextChangeNotification") :bool
  (handle :pointer))

(cffi:defcfun (close-change "FindCloseChangeNotification") :bool
  (handle :pointer))

(cffi:defcfun (create-event "CreateEventA") :pointer
  (security-attributes :pointer)
  (reset :bool)
  (state :bool)
  (name :string))

(cffi:defcfun (create-file "CreateFileW") :pointer
  (path :pointer)
  (access access-flag)
  (share share-flag)
  (security-attributes :pointer)
  (creation-disposition disposition-flag)
  (flags create-flag)
  (template :pointer))

(cffi:defcfun (close-handle "CloseHandle") :bool
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

(defstruct (watch (:constructor make-watch (directory handle event buffer overlapped table)))
  directory handle event buffer overlapped table)

(defun close-watch (watch)
  (cancel-io (watch-handle watch))
  (close-handle (watch-handle watch))
  (close-handle (watch-event watch))
  (cffi:foreign-free (watch-buffer watch))
  (cffi:foreign-free (watch-overlapped watch)))

(define-implementation shutdown ()
  (loop for v being the hash-values of *watches*
        do (close-watch v))
  (clrhash *watches*))

(define-implementation watch (file/s &key (events T))
  (declare (ignore events))
  (flet ((add (path)
           (with-simple-restart (abort "Don't watch ~a" path)
             (let* ((path (truename path))
                    (dir (make-pathname :name NIL :type NIL :defaults path))
                    (existing (gethash dir *watches*)))
               (unless existing
                 (com:with-wstring (path (namestring dir))
                   (let ((handle (create-file path
                                              :directory
                                              '(:read :write :delete)
                                              (cffi:null-pointer)
                                              :existing
                                              '(:backup :overlapped)
                                              (cffi:null-pointer)))
                         (event (create-event (cffi:null-pointer)
                                              NIL
                                              NIL
                                              (cffi:null-pointer))))
                     (check-last-error (/= #+64-bit #xFFFFFFFFFFFFFFFF #-64-bit #xFFFFFFFF
                                           (cffi:pointer-address handle)))
                     (check-last-error (/= #+64-bit #xFFFFFFFFFFFFFFFF #-64-bit #xFFFFFFFF
                                           (cffi:pointer-address event)))
                     (setf existing (make-watch (namestring dir)
                                                handle
                                                event
                                                (cffi:foreign-alloc '(:struct event))
                                                (cffi:foreign-alloc '(:struct overlapped))
                                                (make-hash-table :test 'equal)))
                     (start-read existing)
                     (setf (gethash dir *watches*) existing))))
               (setf (gethash path (watch-table existing)) T)))))
    (if (listp file/s)
        (mapc #'add file/s)
        (add file/s))))

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
                 (close-watch existing)
                 (remhash dir *watches*))))))
    (if (listp file/s)
        (mapc #'del file/s)
        (del file/s))))

(defun start-read (watch)
  (with-slots (handle event buffer overlapped) watch
    (setf (overlapped-internal overlapped) 0)
    (setf (overlapped-internal-high overlapped) 0)
    (setf (overlapped-offset overlapped) 0)
    (setf (overlapped-offset-high overlapped) 0)
    (setf (overlapped-handle overlapped) event)
    (cffi:foreign-funcall "memset" :pointer buffer :int 0 #+64-bit :uint64 #-64-bit :uint32
                                                          (cffi:foreign-type-size '(:struct event)) :pointer)
    (check-last-error (read-changes handle buffer (cffi:foreign-type-size '(:struct event)) NIL :all
                                    (cffi:null-pointer) overlapped (cffi:null-pointer)))))

(defun process-change (watch function)
  (let* ((buffer (watch-buffer watch))
         (path (merge-pathnames (com:wstring->string (cffi:foreign-slot-pointer buffer '(:struct event) 'name))
                                (watch-directory watch)))
         (action (event-action buffer)))
    (when (keywordp action)
      (with-simple-restart (abort "Don't notify the change to ~a" path)
        (funcall function path action)))))

(define-implementation process-events (function &key timeout)
  (let* ((tsec (etypecase timeout
                 ((eql NIL) 0)
                 ((eql T) 0.5)
                 ((real 0) timeout)))
         (msec (floor (* 1000 tsec)))
         (count (hash-table-count *watches*))
         (watches (make-array count)))
    (when (<= #x80 count)
      (error "FIXME: Too many watches. Don't know how to deal with this."))
    (when (< 0 count)
      ;; First check whether we have any pending overlaps
      (cffi:with-foreign-objects ((handles :pointer count))
        (loop for i from 0
              for watch being the hash-values of *watches*
              do (setf (cffi:mem-aref handles :pointer i) (watch-event watch))
                 (setf (aref watches i) watch))
        ;; Next watch for changes
        (loop for ev = (wait-for-multiple-objects count handles NIL msec)
              do (case ev
                   ((:abandoned :io-completion))
                   (:timeout
                    (finish-output)
                    (unless (eql T timeout)
                      (return)))
                   (:failed
                    (check-last-error NIL))
                   (T
                    (when (< ev #x80)
                      (process-change (aref watches ev) function)
                      (start-read (aref watches ev))))))))))
