;;;; package.lisp

(defpackage #:granolin
  (:use #:cl)
  (:export

   #:txn-id
   #:client
   #:homeserver
   #:handle-event

   #:getob
   #:event-content
   #:event-type
   #:event-id
   #:sender
   #:msg-type
   #:msg-body
   #:state-key
   #:prev-content

   #:timeline-event
   #:room-state-event
   #:invitation-event

   #:send
   #:fetch

   #:login
   #:sync

   #:start
   #:stop
   ))
