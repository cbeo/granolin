
;;;; granolin.lisp

(in-package #:granolin)

;;; Login https://matrix.org/docs/spec/client_server/r0.5.0#id242
;;; Media Upload https://matrix.org/docs/spec/client_server/r0.5.0#post-matrix-media-r0-upload



(defclass client ()
  ((access-token
    :reader login
    :initarg :login
    :initform (error "Clients need an access token."))
   (homeserver
    :reader homeserver
    :initarg :homeserver
    :initform (error "Clients require a homeserver."))
   (rooms
    :accessor rooms
    :initform nil)
   (users
    :accessor users
    :initform nil)))
;;TODO add things like transaction ids and time points


(defmethod initialize-instance :after ((client client) &key)
  ;; fetch rooms
  ;; fetch users
  ;; fetch any other state that might be useful
  )

;;; Sending Room Messages 
(defgeneric send-message (client room msg)
  (:documentation "Ends up sending an http request to the client's homeserver"))


