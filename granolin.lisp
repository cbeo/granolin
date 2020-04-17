;;;; granolin.lisp

;;;; While reading this file, any symbol, function, etc with a docstring should
;;;; be exported from the granolin package. Anything else should be considered
;;;; "private".

(in-package #:granolin)

;;; Utility class for generating a sequence of IDs

(defclass id-source ()
  ((id-source :initform 0)))

(defun txn-id (source)
  (with-slots (id-source) source
    (incf id-source)
    (format nil "~a" id-source)))

;;; The main matrix client class

(defclass client (id-source)
  ((homeserver
    :reader homeserver
    :initarg :homeserver
    :initform (error "HOMESERVER is required.")
    :type string
    :documentation "The hostname this client connects to.")
   (user-id
    :accessor user-id
    :initarg :user-id
    :initform nil
    :type string)
   (hardcopy
    :accessor hardcopy
    :initarg :hardcopy
    :initform nil
    :type pathname
    :documentation "A file path where client state is saved.")
   (running-p
    :accessor running-p
    :initform t)
   (timeout
    :accessor timeout
    :initarg :timeout
    :initform (* 1000 30)
    :type integer
    :documentation "The length of time, in ms, to wait during long-polling to /sync")
   (access-token
    :accessor access-token
    :initform nil
    :type string)
   (next-batch
    :accessor next-batch
    :initform nil
    :type string
    :documentation "Used on sync requests as the value of the SINCE parameter")
   (ssl
    :reader ssl
    :initarg :ssl
    :initform T
    :documentation "Set to nil to use http protocol."))
  (:documentation "An instance of CLIENT holds the necessary state for
  interacting with a Matrix server. If HARDCOPY is supplied, the
  INITIALIZE-INSTANCE :after auxilliary method will attempt to populate the
  following slots from a file: HOMESERVER, TIMEOUT, ACCESS-TOKEN, NEXT-BATCH."))

(defun logged-in-p (client)
  "T if the client has an access token."
  (and (access-token client) t))

(defun save-client-state (client &key fname)
  "Save a PLIST of client state to disk. Saves HOMESERVER, TIMEOUT,
   ACCESS-TOKEN, and NEXT-BATCH values to the file."

  (when (and (not fname) (hardcopy client))
    (setf fname (hardcopy client)))

  (with-open-file (out fname :direction :output :if-exists :supersede)
    (print (list
            :id-source (slot-value client 'id-source)
            :homeserver (homeserver client)
            :timeout (timeout client)
            :access-token (access-token client)
            :next-batch (next-batch client))
           out)))

(defun load-client-state (client &optional fname)
  "Load client state from a PLIST stored in a file."

  (when (and (not fname) (hardcopy client))
    (setf fname (hardcopy client)))

  (let ((conf (with-open-file (in fname) (read in))))
    (setf (slot-value client 'id-source) (getf conf :id-source))
    (setf (timeout client) (getf conf :timeout))
    (setf (access-token client) (getf conf :access-token))
    (setf (next-batch client) (getf conf :next-batch)))
  client)

;; TODO
(defun validate-homserver-url (client)
  "Ensure that the homeserver url is well formed, and makes an attempt to format it if it isnt")

(defmethod initialize-instance :after ((client client) &key)
  (validate-homserver-url client)
  (when (and (hardcopy client) (probe-file (hardcopy client)))
    (load-client-state client)))

(defgeneric handle-event (client event)
  (:documentation "Implemented on handlers that need to respond to events.")
  (:method ((client client) event) t))

(defgeneric clean-up (client)
  (:documentation "To be run before the client crashes or is killed.")
  (:method ((client client))
    (setf (running-p client) nil)
    (save-client-state client)))

;;; Dynamic variables bound during response handling. Each such variable should
;;; be bound to a value during any message handler call.

(defvar *response-body* nil
  "Dynamic variable holding a response body.")
(defvar *response-status* nil
  "Dynamic variable holding a response staus code.")
(defvar *response-headers* nil
  "Dynamic variable holding response status headers.")
(defvar *response-object* nil
  "Dynamic variable holding a RESPONSE-OBJECT struct.")
(defvar *room-id* nil
  "Dynamic variable holding id of the room whose event is being processed.")

;;; Utilities for working with parsed JSON data

(def-json-wrap login-response
  (user-id :|user_id|)
  (access-token :|access_token|))

(def-json-wrap sync-response
  (next-batch :|next_batch|)
  (rooms :|rooms|)
  (presence :|presence|)
  (joined-rooms :|rooms| :|join|)
  (invited-rooms :|rooms| :|invite|)
  (account-data-events :|account_data| :|events|))

(def-json-wrap timeline-event
  (event-content :|content|)
  (event-type :|type|)
  (event-id :|event_id|)
  (sender :|sender|)
  (msg-type :|content| :|msgtype|)
  (msg-body :|content| :|body|))

(defstruct (text-message-event (:include timeline-event)))
(def-timeline-event-pred text-message-event-p* "m.room.message" "m.text")

(defstruct (image-message-event (:include timeline-event)))
(def-timeline-event-pred image-message-event-p* "m.room.message" "m.image")

(defstruct (audio-message-event (:include timeline-event)))
(def-timeline-event-pred audio-message-event-p* "m.room.message" "m.audio")

(defstruct (file-message-event (:include timeline-event)))
(def-timeline-event-pred file-message-event-p* "m.room.message" "m.file")

(defstruct (video-message-event (:include timeline-event)))
(def-timeline-event-pred video-message-event-p* "m.room.message" "m.video")

(defstruct (emote-message-event (:include timeline-event)))
(def-timeline-event-pred emote-message-event-p* "m.room.message" "m.emote")

(defstruct (notice-message-event (:include timeline-event)))
(def-timeline-event-pred notice-message-event-p* "m.room.message" "m.notice")

(defstruct (location-message-event (:include timeline-event)))
(def-timeline-event-pred location-message-event-p* "m.room.message" "m.location")

(def-json-wrap room-state-event
  (event-content :|content|)
  (sender :|sender|)
  (event-type :|type|)
  (event-id :|event_id|)
  (state-key :|state_key|)
  (room-name :|content| :|name|)        ; only valid on m.room.name
  (room-aliases :|content| :|aliases|)  ; only valid on m.room.aliases
  (prev-content :|prev_content|))

(def-json-wrap invitation-event
  (event-content :|content|)
  (state-key :|state_key|)
  (event-type :|type|)
  (sender :|sender|))

(def-json-wrap account-data-event
  (event-content :|content|)
  (event-type :|type|))

;; the basic-json struct is used as a kind of default in some places
(def-json-wrap basic-json)

;;; URI constants (format) strings for interacting with the Matrix API

(defparameter +login-path+ "/_matrix/client/r0/login")
(defparameter +sync-path+ "/_matrix/client/r0/sync")
(defparameter +join-room-path+ "/_matrix/client/r0/rooms/~a/join")
(defparameter +text-message-path+ "/_matrix/client/r0/rooms/~a/send/m.room.message/~a")
(defparameter +create-room-path+ "/_matrix/client/r0/createRoom")
(defparameter +update-account-data-path+ "/_matrix/client/r0/user/~a/account_data/~a")
(defparameter +upload-path+ "/_matrix/media/r0/upload?filename=~a")

;;; Utility functions and macros for making HTTP requests to the MATRIX API

(defun add-auth-header (client headers)
  "If CLIENT has a non-nill ACCESS-TOKEN , adds the token to HEADERS, an ALIST,
   and returns it. Otherwise HEADERS unmodified."
  (if (access-token client)
      (cons (cons "Authorization"
                  (concatenate 'string "Bearer " (access-token client)))
            headers)
      headers))

(defmacro send ((client path body
                        &key
                        (method :put)
                        headers
                        wrap
                        (literal-body nil)
                        (content-type "application/json"))
                on-ok &optional otherwise)
  "Makes a POST request to the Matrix server and binds *RESPONSE-BODY* (a raw
  string, usually JSON formatted) *RESPONSE-STATUS* (an integer) and
  *RESPONSE-HEADERS* (an alist).

  If CLIENT has an ACCESS-TOKEN slot, this token will be added to the headers as
  the value of an authorization header, as specified in the Matrix API.

  BODY is expected to be a PLIST that will be serialized to JSON as the request
  body.

  HEADERS is an ALIST of name-value pairs.

  If *RESPONSE-STATUS* is 200, *RESPONSE-BODY* is assumed to be JSON text, and
  is parsed to a PLIST before being wrapped with a call to WRAP. WRAP must be
  the name of a constructor defined with DEF-JSON-WRAP. The result is bound to
  *RESPONSE-OBJECT*.

  When *RESPONSE-STATUS* is anything other than 200 the form in OTHERWISE is run."
  (let ((content (if literal-body body (list 'jonathan:to-json body))))

    `(multiple-value-bind
           (*response-body* *response-status* *response-headers*)
         (drakma:http-request (make-matrix-path ,client ,path)
                              :additional-headers (add-auth-header ,client ,headers)
                              :method ,method
                              :content ,content
                              :content-type ,content-type)
       (if (= 200 *response-status*)
           (let ((*response-object*
                  (,wrap
                   :data (jonathan:parse (flexi-streams:octets-to-string *response-body*)))))
             ,on-ok)
           ,otherwise))))

(defmacro fetch ((client path &key params headers wrap)
                 on-ok &optional otherwise)
  "Makes a GET request to the Matrix server and binds *RESPONSE-BODY* (see below),
  *RESPONSE-STATUS* (an integer) and *RESPONSE-HEADERS* (an alist).

  If *RESPONSE-STATUS* is 200, *RESPONSE-BODY* is assumed to be JSON text, and
  is parsed to a PLIST before being wrapped with a call to WRAP. WRAP must be
  the name of a constructor defined with DEF-JSON-WRAP. The result is bound to
  *RESPONSE-OBJECT*.

  If *RESPONSE-STATUS* is anything other than 200, then the form in OTHERWISE is
  run."
  `(multiple-value-bind
         (*response-body* *response-status* *response-headers*)
         (drakma:http-request (make-matrix-path ,client ,path)
                              :additional-headers (add-auth-header ,client ,headers)
                              :parameters ,params
                              :method :get)
     (if (= 200 *response-status*)
         (let ((*response-object*
                 (,wrap
                  :data (jonathan:parse (flexi-streams:octets-to-string *response-body*)))))
           ,on-ok)
         ,otherwise)))

(defun make-matrix-path (client path )
  (concatenate 'string
               (if (ssl client) "https://" "http://")
               (homeserver client)
               path))

;;; API Calls

(defun login (client user password)
  "Logs CLIENT into its HOMESERVER withthe provided USER and PASSWORD.

  If successful, sets the ACCESS-TOKEN of the CLIENT. Otherwise raises an
  error."
  (let ((body (list :|type| "m.login.password"
                    :|identifier| (list :|type| "m.id.user"
                                        :|user| user)
                    :|password| password
                    :|initial_device_display_name| "Granolin")))

    (send (client +login-path+ body
           :method :post
           :wrap make-login-response)

          (progn
            (setf (user-id client)
                  (user-id *response-object*))
            (setf (access-token client)
                  (access-token *response-object*)))

          (error "Attempt to login ~a : ~a failed with ~a"
                 user password *response-status*))))

(defun sync (client &key full-state extra-params)
  "Synchronize client state with server state. CLIENT should have a valid
  ACCESS-TOKEN slot value (i.e. the CLIENT should have been logged in).

  SUPPRESS-SINCE is used to ignore the NEXT-BATCH field held in the CLIENT
  instance. FULL-STATE will retrieve the full state even if SINCE is present.

  See the endpoint documentationa at:
  https://matrix.org/docs/spec/client_server/r0.5.0#get-matrix-client-r0-sync
  "
  (let (params)
    (setf params (append params extra-params))
    (when full-state
      (push (cons "full_state" "true") params))
    (push (cons "timeout" (format nil "~a" (timeout client))) params)
    (when (and (next-batch client)
               (not full-state))
      (push (cons "since" (next-batch client))  params))

    (fetch (client +sync-path+ :params params :wrap make-sync-response)
           (handle-sync-response client)
           (error "Matrix returned ~a from ~a"
                  *response-status* +sync-path+))))

(defun handle-sync-response (client)
  (setf (next-batch client)
        (next-batch *response-object*))
  (process-joined-events client)
  (process-invited-room-events client)
  (process-account-data-events client))


;;; Event Processing and Dispatch

(let ;; define some private "cache" variables to reuse
    (( %timeline-event% (make-timeline-event :data nil))
     ( %text-message-event% (make-text-message-event :data nil))
     ( %image-message-event% (make-image-message-event :data nil))
     ( %audio-message-event% (make-audio-message-event :data nil))
     ( %file-message-event% (make-file-message-event :data nil))
     ( %video-message-event% (make-video-message-event :data nil))
     ( %emote-message-event% (make-emote-message-event :data nil))
     ( %notice-message-event% (make-notice-message-event :data nil))
     ( %location-message-event% (make-location-message-event :data nil))
     ( %state-event% (make-room-state-event :data nil))
     ( %account-data-event% (make-account-data-event :data nil)))

  (defun categorize-and-set-timeline-event (ob)
    (cond
      ((text-message-event-p* ob)
       (setf (timeline-event-data %text-message-event%) ob)
       %text-message-event%)
      ((image-message-event-p* ob)
       (setf (timeline-event-data %image-message-event%) ob)
       %image-message-event%)
      ((audio-message-event-p* ob)
       (setf (timeline-event-data %audio-message-event%) ob)
       %audio-message-event%)
      ((file-message-event-p* ob)
       (setf (timeline-event-data %file-message-event%) ob)
       %file-message-event%)
      ((video-message-event-p* ob)
       (setf (timeline-event-data %video-message-event%) ob)
       %video-message-event%)
      ((emote-message-event-p* ob)
       (setf (timeline-event-data %emote-message-event%) ob)
       %emote-message-event%)
      ((notice-message-event-p* ob)
       (setf (timeline-event-data %notice-message-event%) ob)
       %notice-message-event%)
      ((location-message-event-p* ob)
       (setf (timeline-event-data %location-message-event%) ob)
       %location-message-event%)
      (t
       (setf (timeline-event-data %timeline-event%) ob)
       %timeline-event%)))

  (defun process-joined-events (client)
    (loop :for (room-id room . ignore) :on (joined-rooms *response-object*) :by #'cddr :do
      (let ((*room-id* (symbol-name room-id))) ;; room-id should be a string

        ;; handle the timeline events (aka room events)
        (dolist (ob (getob room :|timeline| :|events|))
          (handle-event client
                        (categorize-and-set-timeline-event ob)))

        ;; handle state chnage events (aka state events)
        (dolist (ob (getob room :|state| :|events|))
          (setf (room-state-event-data %state-event%) ob)
          (handle-event client %state-event%)))))

  ;; TODO add global cache variable for invite event
  (defun process-invited-room-events (client)
    (let ((invite-event (make-invitation-event :data nil)))
      (loop :for (room-id room . ignore) :on (invited-rooms *response-object*) :by #'cddr :do
        (let ((*room-id* (symbol-name room-id)))

          (dolist (ob (getob room :|invite_state| :|events|))
            (setf (invitation-event-data invite-event) ob)
            (handle-event client invite-event))))))

  (defun process-account-data-events (client)
    (dolist (ob (account-data-events *response-object*))
      (setf (account-data-event-data %account-data-event%) ob)
      (handle-event client %account-data-event%)))

)


(defun send-text-message (client room-id message &rest args)
  "Sends the MESSAGE (a string) to the room with id ROOM-ID. MESSAGE can also be
   a format string, and ARGS is "
  (let ((url (format nil +text-message-path+ room-id (txn-id client)))
        (body (list :|msgtype| "m.text"
                    :|body| (apply #'format (list* nil message args)))))
    (send (client url body :wrap make-basic-json) t)))

(defun send-image-message (client room-id alt-text mxc-uri &key info )
  "Sneds an m.image style message to the a room. 

   See https://matrix.org/docs/spec/client_server/r0.6.0#m-image for
   documentaiton aobut the :info argument."
  (let ((url (format nil +text-message-path+ room-id (txn-id client)))
        (body (list :|msgtype| "m.image"
                    :|body| alt-text
                    :|url| mxc-uri
                    :|info| info
                    )))
    (send (client url body :wrap make-basic-json) t)))

(defun send-video-message (client room-id alt-text mxc-uri &key info)
  "Sends a video message to a room using the mxc-uri that should have
  been returned from a previous upload to the server.

See https://matrix.org/docs/spec/client_server/r0.6.0#m-video for
information about the :info argument."
  (let ((url (format nil +text-message-path+ room-id (txn-id client)))
        (body (list :|msgtype| "m.video"
                    :|body| alt-text
                    :|url| mxc-uri
                    :|info| info)))
    (send (client url body :wrap make-basic-json) t)))

(defun join-room (client room-id)
  "Attempts to join the client to the room with ROOM-ID."
  (let ((body (list :|roomId| room-id))
        (url (format nil +join-room-path+ room-id)))
    (send (client url body :method :post :wrap make-basic-json)
          t ; do nothing in case of success
          (format *error-output* "FAILED to join room: ~a.~%HTTP response: ~a ~a~%~%"
                  room-id
                  *response-status*
                  (flexi-streams:octets-to-string *response-body*)))))

(defun upload (client filename content &key (content-type "application/image"))
  "Uploads a file to the homeserver's media storage. 

   It returns the MXC URI to the uploaded content.  

   FILENAME is a string passed into the query parameters of the request URI.

   CONTENT can be a string or a sequence of octets.  It is passed
   directly to the :content keyword parameter of drakma:http-request.

   See https://edicl.github.io/drakma/#http-request for more. "
  (let ((url (format nil +upload-path+ filename)))
    (send (client url content
                  :method :post
                  :content-type content-type
                  :wrap make-basic-json
                  :literal-body t)
          (getf (basic-json-data *response-object*) :|content_uri|)
          (format *error-output* "FAILED to upload content: ~a.~%HTTP response: ~a ~a~%~%"
                  filename
                  *response-status*
                  (flexi-streams:octets-to-string *response-body*)))))

(defun update-account-data (client m-type data)
  "Serializes the PLIST DATA as JSON and PUTs it in account_data at the given M-TYPE.

   E.g. M-TYPE might be the string m.direct"
  (let ((url  (format nil
                      +update-account-data-path+
                      (user-id client)
                      m-type)))
    (send (client url data :method :put :wrap make-basic-json)
          t
          (format *error-output*
                  "FAILED to update user account data.~%HTTP respponse: ~a ~a~%"
                  *response-status*
                  (flexi-streams:octets-to-string *response-body*)))))

;;; bot loop

(defun start (client)
  "Repeatedly calls SYNC with this cleint. If something goes wrong, CLEAN-UP
   will be called before the process dies."
  (setf (running-p client) t)
  (unwind-protect
       (loop :while (running-p client)
          :do (handler-case (sync client)
                (error (c) (format *error-output* "Error during Event Sync: ~a~%~%" c))))
    (clean-up client)))

(defun stop (client)
  (setf (running-p client) nil))

