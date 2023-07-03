(defpackage #:org.shirakumo.file-notify
  (:use #:cl)
  (:shadow #:byte)
  (:local-nicknames
   #+windows
   (#:com #:org.shirakumo.com-on))
  ;; protocol.lisp
  (:export
   #:failure
   #:init
   #:shutdown
   #:watch
   #:list-watched
   #:unwatch
   #:process-events
   #:with-events))
