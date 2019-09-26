;;;; package.lisp

(defpackage #:granolin
  (:use #:cl)
  (:export

   ;; main class
   #:client
   #:homeserver
   #:logged-in-p
   #:handle-event

   ;; utility classes
   #:message-log
   #:server-directory
   #:auto-joiner

   ;; json data utilities & accessors
   #:event-content
   #:event-id
   #:event-type
   #:getob
   #:msg-body
   #:msg-type
   #:prev-content
   #:room-aliases
   #:room-name
   #:sender
   #:state-key

   ;; event types
   #:timeline-event
   #:room-state-event
   #:invitation-event

   ;; generic response types
   #:basic-json

   ;; server interaction macros
   #:send
   #:fetch

   ;; matrix API calls
   #:login
   #:sync
   #:send-text-message
   #:join-room

   ;; bot control
   #:start
   #:stop

   ))
