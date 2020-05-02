;;;; granolin.asd

(asdf:defsystem #:granolin
  :description "Lisp learns how to spam Matrix servers."
  :author "cbeok@protonmail.com"
  :license  "AGPLv3.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:drakma #:jonathan #:local-time)
  :components ((:file "package")
               (:file "macros")
               (:file "utils")
               (:file "granolin")
               (:file "plugins")))
