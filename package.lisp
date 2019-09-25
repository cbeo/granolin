;;;; package.lisp

(defpackage #:granolin
  (:use #:cl)
  (:export

   #:txn-id
   #:client
   #:homeserver
   #:handle-timeline-event
   #:handle-room-state-event
   #:handle-invitation-event

   #:getob
   #:event-content
   #:event-type
   #:event-id
   #:sender
   #:msg-type
   #:msg-body
   #:state-key
   #:prev-content

   #:send
   #:fetch

   #:login
   #:sync 







   ))
