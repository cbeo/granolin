
(defclass shell-echo-bot (granolin:client granolin::message-log) ())

(defvar *bot*
  (make-instance 'shell-echo-bot
                 :hardcopy (merge-pathnames ".shell-echo-bot.conf"
                                            (user-homedir-pathname))
                 :homeserver "https://matrix.hrlo.world"
                 :output *standard-output*))


;; a script to login if necessary, and then start the bot

(unless (access-token *bot*) 
  (princ "Log in to the server:")
  (terpri)
  (granolin:login *bot*
                  (and (princ "username: ") (read-line))
                  (and (princ "password: ") (read-line))))


(start *bot*)
