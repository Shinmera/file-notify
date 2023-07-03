(asdf:defsystem file-notify
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Access to file change and access notification."
  :homepage "https://shinmera.github.io/file-notify"
  :bug-tracker "https://github.com/Shinmera/file-notify/issues"
  :source-control (:git "https://github.com/Shinmera/file-notify.git")
  :serial T
  :defsystem-depends-on (:trivial-features)
  :components ((:file "package")
               (:file "protocol")
               (:file "inotify" :if-feature :linux)
               (:file "windows" :if-feature :windows)
               (:file "fsevent" :if-feature :darwin)
               (:file "documentation"))
  :depends-on (:documentation-utils
               :cffi
               (:feature :windows :com-on)))
