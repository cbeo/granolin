;;;; A bot for playing rock paper scissors.

(ql:quickload :cl-ppcre)

(defpackage #:roshambot
  (:use :cl :granolin))

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
  challenger-room
  challenger-move
  challenged
  challenged-room
  challenged-move)

(defclass roshambot ()
  ((live-matches
   :accessor live-matches
   :initform nil)))

(defparameter +challenge-regex+
  (ppcre:create-scanner " ?i challenge ([a-zA-Z0-9_.-]+) to roshambo"
                        :case-insensitive-mode t))

(defun you-wanna-piece-of-this!? (str)
  "If the string communicates one user's intention to challenge another to
  roshambo, returns the name of the user that has been challenged. Returns NIL otherwise."
  (let-when (groups (nth-value 1 (ppcre:scan-to-strings +challenge-regex+ str)))
    (aref groups 0)))

(defparameter +roshambo-move-regex+
  (ppcre:create-scanner "(rock|paper|scissors|cancel)" :case-insensitive-mode t))

(defun roshambo-move!? (str)
  (nth-value 0 (ppcre:scan-to-strings +roshambo-move-regex+ str)))

(defmethod handle-event :after ((bot roshambot) (event text-message-event))
  (let ((text (granolin:msg-body event)))
    (let-cond
      (challenged (you-wanna-piece-of-this!? text)
                   (handle-new-challenge bot *room-id* (granolin:sender event) challenged))
      (roshambo-match (challenger-made-move!? bot *room-id* (granolin::sender event) text)
                      (handle-match-state-change bot roshambo-match))
      (roshambo-match (challenged-made-move!? bot *room-id* (granolin::sender event) text)
                      (handle-match-state-change bot roshambo-match)))))

(defun challenger-made-move!? (bot room-id sender text)
  (let-when (roshambo-match (find room-id (live-matches bot)
                                  :key #'roshambo-match-challenger-room
                                  :test #'equal))
    (unless (roshambo-match-challenger-move roshambo-match)
      (let-when (move (and (equal sender (roshambo-match-challenger roshambo-match))
                           (roshambo-move!? text)))
        (setf (roshambo-match-challenger-move roshambo-match) move)
        roshambo-match))))

(defun challenged-made-move!? (bot room-id sender text)
  (let-when (roshambo-match (find room-id (live-matches bot)
                                  :key #'roshambo-match-challenged-room
                                  :test #'equal))
    (unless (roshambo-match-challenged-move roshambo-match)
      (let-when (move (and (equal sender (roshambo-match-challenged roshambo-match))
                           (roshambo-move!? text)))
        (setf (roshambo-match-challenged-move roshambo-match) move)
        roshambo-match))))

(defun handle-match-state-change (bot roshambo-match)
  (let-cond
    (cancelled-by (roshambo-cancelled!? roshambo-match)
                  (send-text-message bot (roshambo-match-room roshambo-match)
                                     "The match between ~a and ~a was canceled by ~a."
                                     (readable-username
                                      (roshambo-match-challenger roshambo-match))
                                     (readable-username
                                      (roshambo-match-challenged roshambo-match))
                                     cancelled-by)
                  (kill-roshambo-match bot roshambo-match))
    (win-list (roshambo-has-winner!? roshambo-match)
              (destructuring-bind (winner win-move loser lose-move) win-list
                (if (eql winner :draw) ; this is a draw move
                    (send-text-message bot (roshambo-match-room roshambo-match)
                                       "It's a draw! Both ~a and ~a picked ~a."
                                       (readable-username win-move)
                                       (readable-username loser)
                                       lose-move)
                    (send-text-message bot (roshambo-match-room roshambo-match)
                                       "~a's ~a beats ~a's ~a! ~a is the winner!~%"
                                       (readable-username winner)
                                       win-move
                                       (readable-username loser)
                                       lose-move
                                       (readable-username winner))))
              (kill-roshambo-match bot roshambo-match))))


(defun roshambo-cancelled!? (rmatch)
  (cond ((and (roshambo-match-challenger-move rmatch)
              (string-equal "cancel" (roshambo-match-challenger-move rmatch)))
         (roshambo-match-challenger rmatch))

        ((and (roshambo-match-challenged-move rmatch)
             (string-equal "cancel" (roshambo-match-challenged-move rmatch)))
         (roshambo-match-challenged rmatch))))

(defun roshambo-has-winner!? (rsb)
  (with-slots (challenger challenger-move challenged challenged-move) rsb
    (when (and challenger-move challenged-move
               (not (string-equal "cancel" challenger-move))
               (not (string-equal "cancel" challenged-move)))
      (cond ((string-equal challenger-move challenged-move)
             (list :draw challenger challenged challenger-move)) ; both had same move
            ((and (string-equal "rock" challenger-move)
                  (string-equal "scissors" challenged-move))
             (list challenger challenger-move challenged challenged-move))

            ((and (string-equal "paper" challenger-move)
                  (string-equal "rock" challenged-move))
             (list challenger challenger-move challenged challenged-move))

            ((and (string-equal "scissors" challenger-move)
                  (string-equal "paper" challenged-move))
             (list challenger challenger-move challenged challenged-move))

            ((and (string-equal "rock" challenged-move)
                  (string-equal "scissors" challenger-move))
             (list challenged challenged-move challenger challenger-move))

            ((and (string-equal "paper" challenged-move)
                  (string-equal "rock" challenger-move))
             (list challenged challenged-move challenger challenger-move))

            ((and (string-equal "scissors" challenged-move)
                  (string-equal "paper" challenger-move))
             (list challenged challenged-move challenger challenger-move))))))


(defun kill-roshambo-match (bot roshambo-match)
  (setf (live-matches bot)
        (delete roshambo-match (live-matches bot))))

(defun find-match-with-user (bot user)
  (find-if (lambda (m) (or (equal user (roshambo-match-challenger m))
                           (equal user (roshambo-match-challenged m))))
           (live-matches bot)))


(defun handle-new-challenge (bot room-id challenger challenged)
  (setf challenged (find-contact bot challenged :like t))
  ;; if either user is already in a match, they shouldn't enter a second.
  (cond ((find-match-with-user bot challenger)
         (send-text-message bot room-id
                            "Sorry, ~a is already in a match."
                            (readable-username challenger)))
        ((find-match-with-user bot challenged)
         (send-text-message bot room-id
                            "Sorry, ~a is already in a match."
                            (readable-username challenged)))
    (t ;; otherwise send a direct message to each participant and make a new match instance
     (let ((challenger-room (ensure-direct-room bot challenger))
           (challenged-room (ensure-direct-room bot challenged)))
       (if (and challenger-room challenged-room
                (send-text-message
                 bot
                 challenger-room
                 "You have challenged ~a to roshambo. Reply with Rock, Paper, Scissors or Cancel."
                 (readable-username challenged))
                (send-text-message
                 bot
                 challenged-room
                 "~a has challenged you to roshambo. Reply with Rock, Paper, Scissors, or Cancel."
                 (readable-username challenger)))
           (push (make-roshambo-match :room room-id
                                      :challenger challenger
                                      :challenger-room challenger-room
                                      :challenger-move nil
                                      :challenged challenged 
                                      :challenged-room challenged-room
                                      :challenged-move nil)
                 (live-matches bot))

           (send-text-message bot room-id "Some kind of problem starting a roshambo match :("))))))


(defclass roshambo-bot (granolin:client granolin:server-directory roshambot auto-joiner) ())

(defmethod handle-event :after ((bot roshambo-bot) (ev text-message-event))
  (format t "~a - ~a:~%  ~a~%" *room-id* (granolin::sender ev) (granolin:msg-body ev)))

(defmethod handle-event :after ((bot roshambo-bot) (ev granolin::account-data-event))
  (format t "~a ~a" (event-type ev) (event-content ev)))

;; creates and authenticates the bot, returns the bot instance
(defun roshambot (homeserver user password)
  (let ((bot (make-instance 'roshambo-bot :homeserver homeserver)))
    (login bot user password)
    bot))
