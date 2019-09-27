;;;; A bot for playing rock paper scissors.

(ql:quickload :cl-ppcre)

(defpackage #:roshambot
  (:use :cl)
  (:import-from :granolin
                :handle-event
                :find-contact
                :text-message-event
                :send-text-message
                :let-cond))

(in-package :roshambot)

;;; The logic of roshambot is something like:

;;; 1. A user indicates to the bot that they will to challenge another user to a
;;; game of roshambo "hey bot, roshambo with bob"

;;; 2. If bob is known to the bot then the bot will open a private with both the
;;; initiator, say alice, and with bob and present them with a chance to make a
;;; move or to cancel.

;;; 3. The bot then waits for both users to respond with a move or for either
;;; one of them to cancel.

;;; 4. The bot then reports the outcome of the match (winner bob, winner alice,
;;; or canceled by x) in the room in which the match was initiated.

(defstruct roshambo-match
  room
  challenger
  challenger-state
  challenged
  challenged-state)

(defclass roshambot ()
  ((match-id
   :accessor match-id
   :initform 0)
   (live-matches
   :accessor live-matches
   :initform nil)))

(defparameter +challenge-regex+
  (ppcre:create-scanner "i challenge ([a-zA-Z0-9_.-]+) to roshambo"
                        :case-insensitive-mode t))

(defun you-wanna-piece-of-this!? (str)
  "If the string communicates one user's intention to challenge another to
  roshambo, returns the name of the user that has been challenged. Returns NIL otherwise."
  (nth-value 1 (ppcre:scan-to-strings +challenge-regex+ str)))

(defmethod handle-event :after ((bot roshambot) (event text-message-event) &optional room-id)
  (let ((text (msg-body event)))
    (let-cond
      (challenged (you-wanna-piece-of-this!? text)
                  (handle-new-challenge bot room-id (sender event) challenged)))))

(defun handle-new-challenge (bot room-id challenger challenged)
  (let-cond
    (direct-chat (find-contact bot challenged :like t :get-direct-room t)
                 

    )


  (let-if (full-challenged (granolin::find-contact bot challenged :like t))
          (make-new-challenge bot room-id challenger full-challenged)
          (send-text-message client room-id
                             "Hey ~a, I don't know of anybody named ~a"
                             challenger challenged)))



