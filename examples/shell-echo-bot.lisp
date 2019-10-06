
(defpackage :shell-echo-bot
  (:use #:cl #:granolin))

(in-package :shell-echo-bot)

(defclass shell-echo-bot (client
                          message-log
                          server-directory
                          auto-joiner)
  ())



(defun login-and-run (user pw homeserver)
  (let ((bot (make-instance 'shell-echo-bot :homeserver homeserver)))
    (login bot user pw)
    (start bot)))



