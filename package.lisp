;;;; package.lisp

(defpackage #:granolin
  (:use #:cl)
  (:export

   ;; macros
   #:let-cond
   #:let-if
   #:let-when
   #:getob
   #:def-json-wrap

   ;; utilities
   #:string->json-key
   #:readable-username

   ;; main class
   #:client
   #:homeserver
   #:logged-in-p
   #:hardcopy
   #:timeout
   #:save-client-state
   #:load-client-state

   ;; bot generic functions
   #:handle-event
   #:clean-up

   ;; utility classes & their APIS
   #:message-log ; plugin 
   #:server-directory ; plugin
   #:server-room      ; data class
   #:room-members
   #:direct-p
   #:get-room
   #:get-direct-room
   #:find-rooms-named
   #:client-contacts
   #:room-member-p
   #:find-contact
   #:ensure-direct-room
   #:create-direct-message-room

   #:auto-joiner

   ;; json data utilities & accessors
   #:event-content
   #:event-id
   #:event-type
   #:msg-body
   #:msg-type
   #:prev-content
   #:room-aliases
   #:room-name
   #:sender
   #:state-key

   ;; event types
   #:timeline-event
   #:text-message-event
   #:image-message-event
   #:audio-message-event
   #:file-message-event
   #:video-message-event
   #:emote-message-event
   #:notice-message-event
   #:location-message-event
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
   #:create-direct-message-room

   ;; bot control
   #:start
   #:stop

   ))
