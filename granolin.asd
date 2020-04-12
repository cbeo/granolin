;;;; granolin.asd

(asdf:defsystem #:granolin
  :description "Lisp learns how to spam Matrix servers."
  :author "cbeok@protonmail.com"
  :license  "AGPLv3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma #:jonathan)
  :components ((:file "package")
               (:file "macros")
               (:file "utils")
               (:file "granolin")
               (:file "utility-apps")))
