#|
 This file is a part of file-notify
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.file-notify
  (:use #:cl)
  (:shadow #:byte)
  (:local-nicknames
   #+windows
   (#:com #:org.shirakumo.com-on))
  ;; protocol.lisp
  (:export))
