
(defclass shell-echo-bot (granolin:client granolin::message-log) ())

(defvar *bot* (make-instance 'shell-echo-bot
                             :homeserver "https://matrix.hrlo.world"
                             :output *standard-output*))




