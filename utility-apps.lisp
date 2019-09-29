;;;; Common utilities to help in building bots

(in-package :granolin)

;;; Logging Bot

(defclass message-log ()
  ((output
    :accessor output
    :initarg :output
    :initform *standard-output*
    :type stream
    :documentation "An output stream to which messages are logged.")
   (logging-p
    :accessor logging-p
    :initform t) ))

(defun print-assoc (alist &optional (stream t))
  (loop :for (k . v) :in alist
        :do (format stream "~a: ~a~%" k v)))

(defmethod handle-event :after ((log message-log) (event timeline-event) &optional room)
  (when (logging-p log)
    (print "Joined Room Message/Timeline Event" (output log))
    (terpri (output log))
    (let ((fields `(("room" . ,room)
                    ("sender" . ,(sender event))
                    ("event type" . ,(event-type event))
                    ("message type" . ,(msg-type event))
                    ("messge body" . ,(msg-body event))
                    ("content" . ,(event-content event)))))
      (print-assoc fields (output log))
      (terpri (output log)))))


(defmethod handle-event :after ((log message-log) (event room-state-event) &optional room)
  (when (logging-p log)
    (print "Joined Room State Event" (output log))
    (terpri (output log))
    (let ((fields `(("room" . ,room)
                    ("sender" . ,(sender event))
                    ("event type" . ,(event-type event))
                    ("state key" . ,(state-key event))
                    ("content" . ,(event-content event)))))
      (print-assoc fields (output log))
      (terpri (output log)))))

(defmethod handle-event :after ((log message-log) (event account-data-event) &optional room)
  (declare (ignore room))
  (when (logging-p log)
    (print "Account Data Event" (output log))
    (terpri (output log))
    (print-assoc `(("content" . ,(event-content event))
                   ("type" . ,(event-type event)))
                 (output log))
    (terpri (output log))))


(defmethod handle-event :after ((log message-log) (event invitation-event) &optional room)
  (when (logging-p log)
    (print "Invitation Event" (output log))
    (terpri (output log))
    (let ((fields `(("room" . ,room)
                    ("sender" . ,(sender event))
                    ("event type" . ,(event-type event))
                    ("state key" . ,(state-key event))
                    ("content" . ,(event-content event)))))
      (print-assoc fields (output log))
      (terpri (output log)))))


;;; A Room and User Directory Bot

(defclass server-room ()
  ((id :accessor room-id :initarg :id :initform (error "Must have a room-id"))
   (name :accessor room-name :initarg :name :initform "")
   (aliases :accessor room-aliases :initarg :aliases :initform nil)
   (members :accessor room-members :initarg :members :initform nil)
   (direct-p :accessor direct-p :initarg :direct-p :initform nil)))

(defclass server-directory ()
  ((directory-table
    :reader directory-table
    :initform (make-hash-table :test 'equal)
    :documentation "A table mapping room IDs to room struct instances.")
   (m-direct-event-content
    :accessor m-direct-event-content
    :initform nil
    :documentation "A cached copy of the current m.direct event")))

(defun get-room (client room-id)
  "Get the SERVER-ROOM struct keyed by ROOM-ID, or return NIL."
  (gethash room-id (directory-table client)))

(defun update-room-name (client room-id name)
  (let ((room (get-room client room-id)))
    (if room
        (setf (room-name room) name)
        (setf room (make-instance 'server-room :id room-id :name name)))
    (setf (gethash room-id (directory-table client)) room)))

(defun update-room-member (client room-id member)
  (let ((room (get-room client room-id)))
    (if room
        (pushnew member (room-members room) :test #'equal)
        (setf room (make-instance 'server-room :id room-id :members (list member))))
    (setf (gethash room-id (directory-table client)) room)))

;; TODO
(defun update-room-aliases (client room-id member)
  (declare (ignore client room-id member)))

(defmethod handle-event :after ((client server-directory)
                                (event room-state-event)
                                &optional room-id)
  (cond
    ((string= "m.room.name" (event-type event))
     (update-room-name client room-id (room-name event)))

    ((string= "m.room.member" (event-type event))
     (update-room-member client room-id (sender event)))

    ((string= "m.room.aliases" (event-type event))
     (update-room-aliases client room-id (room-aliases event)))))

(defmethod handle-event :after ((client server-directory)
                                (event account-data-event)
                                &optional room-id)
  (declare (ignore room-id))
  (when (equal "m.direct" (event-type event))
    (setf (m-direct-event-content client) (event-content event))
    (loop :for (user room-ids . more) :on (event-content event) :by #'cddr :do
      (dolist (room-id room-ids)
        (mark-as-direct client room-id)))))

(defun mark-as-direct (client room-id)
  (setf (direct-p (get-room client room-id)) t))

(defun name-of-room (client room-id)
  "Looks up the name of a room with ROOM-ID. Returns a string of NIL"
  (let ((room (get-room client room-id)))
    (when room (room-name room))))

(defun find-rooms-named (client name &key like full)
  "Looks up the room ID of rooms with the name NAME. If LIKE is T, then any room
  whose name contains the NAME as a substring is returned. If FULL is T, then
  the SERVER-ROOM structs themselves are returned."
  (with-slots (directory-table) client
    (loop :for room :being :the :hash-values :of directory-table
          :when (or (string-equal name (room-name room))
                    (and like (search name (room-name room) :test #'string-equal)))
            :collect (if full room (room-id room)))))

(defun client-contacts (client)
  "Returns a list of all users this client knows about."
  (let (contacts)
    (loop :for room :being :the :hash-values :of (directory-table client) :do
      (dolist (user (room-members room))
        (pushnew user contacts :test #'equal)))
    contacts))

(defun room-member-p (room name &key like)
  (some (lambda (memb) (or (equal name memb)
                           (and like (search name memb :test #'string-equal))))
        (members room)))

;; TODO might be too nebulous.  Could be split up into two functions.
(defun find-contact (client name &key like get-direct-room)
  "Finds a specific matrix or room ID by user name.

   If LIKE is NIL, returns a string equal to NAME if this client has a contact
   with that NAME, or NIL otherwise.

   If LIKE is not NIL, returns the first matrix ID found that contains NAME as a
   substring, or NIL if no such matrix ID is found.

   If GET-DIRECT-ROOM is not NIL, behave as above, except return a room address
   instead. The returned address is usable for direct chats with the contact."
  (if get-direct-room
      ;; return a room-id if the room is marked as direct and has name as a member
      (with-hash-table-iterator (next-room (directory-table client))
        (loop
          (multiple-value-bind (theres-more room-id room) (next-room)
            (unless theres-more (return nil))
            (when (and (direct-p room)
                       (room-member-p room name :like like))
              (return room-id)))))
      ;; return a user-id
      (find-if (lambda (contact)
                 (or (equal name contact)
                     (and like (search name contact :test #'string-equal))))
               (client-contacts client))))

(defun ensure-direct-room (client name &key like)
  "Returns the room ID of a direct chat room between the bot and the user with NAME. If
   no direct chat currently exists between the bot and the user, then an attempt will
   be made to create one before returning the room id."
  (let-cond
    (room (find-contact client name :like like :get-direct-room t)
          room)
    (room (find-contact client name :like like)
          (create-direct-message-room client room))))


(defun create-direct-message-room (client name)
  "Attempt to create a direct message room with the given name. If successful
   the room id is returned. Returns nil and prints to *error-output* if
   unsuccessful."
  (let ((body (list :|invite| (list name)
                    :|is_direct| t)))
    (send (client +create-room-path+ body :method :post :wrap make-basic-json)
          ;; if successful
          (let* ((room-id (getob (basic-json-data *response-object*) :|room_id|))
                 (user-key (string->json-key name))
                 (direct (m-direct-event-content client)))

            ;; add the room to the m.direct content for the user
            (if (getf direct user-key)
                (push (getf direct user-key) room-id)
                (setf (getf direct user-key) (list room-id)))

            (when (update-account-data client "m.direct" direct)
              room-id))
          ;; else
          (format *error-output*
                  "FAILED to create private chat with ~a~%HTTP response: ~a ~a~%"
                  name *response-status*
                  (flexi-streams:octets-to-string *response-body*)))))


;;; Basic Joiner Bot

(defclass auto-joiner () ())

(defmethod handle-event :after ((client auto-joiner) (event invitation-event) &optional room-id)
  (when (equal "invite"
               (getf (event-content event) :|join_rule|))
    (join-room client room-id)))

