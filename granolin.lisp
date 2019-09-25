
;;;; granolin.lisp

(in-package #:granolin)




(defclass id-source ()
  ((id-source :initform 0)))

(defun txn-id (source)
  (with-slots (id-source) source
    (incf id-source)
    (base64:string-to-base64-string
     (format nil "~r, ~r bats ha hah hahhh" id-source id-source))))


(defparameter +login-path+ "/_matrix/client/r0/login")
(defparameter +sync-path+ "/_matrix/client/r0/sync")

(defclass client (id-source)
  ((homeserver
    :reader homeserver
    :initarg :homeserver
    :initform (error "HOMESERVER is required.")
    :type string)
   (access-token
    :accessor access-token
    :initform nil
    :type string)
   (state
    :accessor state
    :initform nil
    :type list
    :documentation "Holds server and room meta info")
   (next-batch
    :accessor next-batch
    :initform nil
    :type string
    :documentation "Used on sync requests as the value of the SINCE parameter")))

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

;;; Utilities for working with parsed JSON data

(defmacro getob (ob key &rest keys)
  "OB should be a nested PLIST, KEYS are lists of keys into that PLIST. Gets the
  result of nested GETF calls into the list. This form is SETF-able."
  (let ((form `(getf ,ob ,key)))
    (dolist (k keys)
      (setf form `(getf ,form ,k)))
    form))

(defmacro def-json-wrap (name &rest field-specs)
  "Defines a struct called NAME with a single slot called DATA.

   The DATA slot holds a PLIST created with JONATHAN:PARSE.

   Each FIELD-SPEC is a list of the form (METHOD-NAME KEY1 ... KEYN)

   For each FIELD-SPEC, a method called METHOD-NAME will be defined as a reader
   that accesses a value, the path to which is formed of the KEY values.

   E.g. If a JSON value `ob` has a descendent at `ob.x.y.z` then the FIELD-SPEC
   could be (get-z :|x| :|y| :|z|)
  "
  `(progn
     (defstruct ,name data)
     ,@(loop :for (method . keys) :in field-specs :collect
             `(defmethod ,method ((ob ,name))
                (with-slots (data) ob
                  (getob data ,@keys))))))

(def-json-wrap login-response
  (user-id :|user_id|)
  (access-token :|access_token|))

(def-json-wrap sync-response
  (next-batch :|next_batch|)
  (rooms :|rooms|)
  (presence :|presence|)
  (joined-rooms :|rooms| :|join|)
  (invited-rooms :|rooms| :|invite|))

(def-json-wrap room-event
  (content :|content|)
  (event-type :|type|)
  (event-id :|event_id|)
  (sender :|sender|))

(defmacro  do-timeline-events ((room-var evt-var resp) &body body)
  "RESP is a SYNC-RESPONSE instance, probably passed in as *RESPONSE-OBJECT*.

  ROOM-VAR becomes bound to the id of each room as it is processed, and EVT-VAR
  to a ROOM-EVENT instance in that room."

  (let ((tmp-var (gensym)))
    `(let ((,evt-var (make-room-event :data nil)))
       (loop :for (,room-var room . ignore) :on (joined-rooms ,resp) :by #'cddr :do
         (dolist (,tmp-var (getob room :|timeline| :|events|))
           (setf (room-event-data ,evt-var) ,tmp-var)
           ,@body)))))

(defun add-auth-header (client headers)
  "If CLIENT has a non-nill ACCESS-TOKEN , adds the token to HEADERS, an ALIST,
   and returns it. Otherwise HEADERS unmodified."

  (if (access-token client)
      (cons (cons "Authorization"
                  (concatenate 'string "Bearer " (access-token client)))
            headers)
      headers))

(defmacro send ((client path body &key (method :put) headers resp-formatter)
                on-ok &optional otherwise)
  "Makes a POST request to the Matrix server and binds *RESPONSE-BODY* (a raw
  string, usually JSON formatted) *RESPONSE-STATUS* (an integer) and
  *RESPONSE-HEADERS* (an alist).

  BODY is expected to be a PLIST that will be serialized to JSON as the request
  body.

  HEADERS is an ALIST of name-value pairs.

  If *RESPONSE-STATUS* is 200, *RESPONSE-OBJECT* is bound to a PLIST containing
  the parsed JSON content returned as the response body, then the form in ON-OK
  is run.

  When *RESPONSE-STATUS* is anything other than 200 the form in OTHERWISE is run."

  `(multiple-value-bind
       (*response-body* *response-status* *response-headers*)
       (drakma:http-request (make-matrix-url ,client ,path)
                            :additional-headers ,(add-auth-header client headers)
                            :method ,method
                            :body (jonathan:to-json ,body)
                            :content-type "application/json")
     (if (= 200 *response-status*)
         (let ((*response-object*
                 (,resp-formatter
                  :data (jonathan:parse *response-body*))))
           ,on-ok)
         ,otherwise)))


(defmacro fetch ((client path &key params headers resp-formatter)
                 on-ok &optional otherwise)
  "Makes a GET request to the Matrix server and binds *RESPONSE-BODY* (see below),
  *RESPONSE-STATUS* (an integer) and *RESPONSE-HEADERS* (an alist).

  If *RESPONSE-STATUS* is 200, *RESPONSE-BODY* is bound to a PLIST containing
  the parsed JSON content returned as the response body, then the form in ON-OK
  is run.

  If *RESPONSE-STATUS* is anything other than 200, then the form in OTHERWISE is
  run."
  `(multiple-value-bind
         (*response-body* *response-status* *response-headers*)
         (drakma:http-request (make-matrix-url ,client ,path)
                              :additional-headers ,(add-auth-header client headers)
                              :parameters ,params
                              :method :get)
     (if (= 200 *response-status*)
         (let ((*response-object*
                 (,resp-formatter
                  :data (jonathan:parse *response-body*))))
           ,on-ok)
         ,otherwise)))

(defun validate-homserver-url (client)
  "ensure that hte homeserver url is well formed, and makes an attempt to format it if it isnt")

(defmethod initialize-instance :after ((client clienxt) &key)
  (validate-homserver-url client))

(defun make-matrix-path (client path)
  (concatenate 'string (homeserver client) path))

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
           :resp-formatter make-login-response)

          (setf (access-token client)
                (access-token *response-object*))

          (error "Attempt to login ~a : ~a failed with ~a"
                 user password *response-status*))))

(defun sync (client &key (full-state "false"))
  "Synchronize client state with server state. CLIENT should have a valid
  ACCESS-TOKEN slot value (i.e. the CLIENT should have been logged in).

  SUPPRESS-SINCE is used to ignore the NEXT-BATCH field held in the CLIENT
  instance. FULL-STATE will retrieve the full state even if SINCE is present.

  See the endpoint documentationa at:
  https://matrix.org/docs/spec/client_server/r0.5.0#get-matrix-client-r0-sync
  "
  (let (params)
    (when (next-batch client)
      (push (cons "since" (next-batch client))))

    (push (cons "full_state" full-state) params)

    (fetch (client +sync-path+ params :resp-formatter make-sync-response)
           (handle-sync-response client)
           (error "Matrix returned ~a from ~a~"
                  *response-status* +sync-path+))))

(defun handle-sync-response (client)
  (setf (next-batch client)
        (next-batch *response-object*))
  ;; If client has no STATE, then set the STATE to the response object
  (if (not (state client))
      (setf (state client) (copy-tree *response-object*))
      (progn
        (process-joined-room-timeline-events client)
        (process-joined-room-state-events client)
        (process-invited-room-events client))))


(defun process-joined-room-timeline-events (client)
  (let ((event (make-room-event)))
    (dolist (eob (getob ())))

(defmacro def-event-list-processor (name handler-name &rest keys)
  `(defun ,name (client)
     (dolist (event (getob (response-object-data *response-object*) ,@keys))
       (,handler-name client event))))



