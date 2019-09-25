(in-package :granolin)

(defclass message-log ()
  ((output
    :accessor output
    :initarg :output
    :initform (error "Message Log requires an output stream")
    :type stream
    :documentation "An output stream to which messages are logged."
    )))

(defun print-assoc (alist &optional (stream t))
  (loop :for (k . v) :in alist
        :do (format stream "~a: ~a~%" k v)))

(defmethod handle-event :after ((log message-log) room (event timeline-event))
  (let ((fields `(("sender" . ,(sender event))
                  ("event type" . ,(event-type event))
                  ("message type" . ,(msg-type event))
                  ("messge body" . ,(msg-body event)))))
    (print-assoc fields (output log))
    (terpri (output log))))


(defmethod handle-event :after ((log message-log) room (event room-state-event))
  (let ((fields `(("sender" . ,(sender event))
                  ("event type" . ,(event-type event))
                  ("state key" . ,(state-key event))
                  ("content" . ,(event-content event)))))
    (print-assoc fields (output log))
    (terpri (output log))))

