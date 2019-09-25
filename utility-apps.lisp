(in-package :granolin)

(defclass message-log ()
  ((output
    :accessor output
    :initarg :output
    :initform (error "Message Log requires an output stream")
    :type stream
    :documentation "An output stream to which messages are logged."
    )))

(defmethod handle-event :after ((log message-log) room (event timeline-event))
  (format (output log) "~a in ~a: ~a~%"
          (sender event)
          room
          (msg-body event)))

