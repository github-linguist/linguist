#!/usr/bin/env newlisp

;; @module IRC
;; @description a basic irc library
;; @version early alpha! 0.1 2013-01-02 20:11:22
;; @author cormullion
;; Usage:
;; (IRC:init "newlithper") ; a username/nick (not that one obviously :-)
;; (IRC:connect "irc.freenode.net" 6667) ; irc/server
;; (IRC:join-channel {#newlisp}) ; join a room
;; either (IRC:read-irc-loop) ; loop - monitor only, no input
;; or     (IRC:session)       ; a command-line session, end with /QUIT

(context 'IRC)
    (define Inickname)
    (define Ichannels)
    (define Iserver)
    (define Iconnected)
    (define Icallbacks '())
    (define Idle-time 400) ; seconds
    (define Itime-stamp)   ; time since last message was processed

(define (register-callback callback-name callback-function)
    (println {registering callback for } callback-name { : } (sym (term callback-function) (prefix callback-function)))
    (push (list callback-name (sym (term callback-function) (prefix callback-function))) Icallbacks)) 

(define (deregister-callback callback-name)
    (println {deregistering callback for } callback-name)
    (setf (assoc "idle-event" Icallbacks) nil)
    (println {current callbacks: } Icallbacks)) 

(define (do-callback callback-name data)
   (when (set 'func (lookup callback-name Icallbacks)) ; find first callback
         (if-not (catch (apply func (list data)) 'error)
                 (println {error in callback } callback-name {: } error))))

(define (do-callbacks callback-name data)
   (dolist (rf (ref-all callback-name Icallbacks))
        (set 'callback-entry (Icallbacks (first rf)))
        (when   (set 'func (last callback-entry))
                (if-not (catch (apply func (list data)) 'error)
                (println {error in callback } callback-name {: } error)))))

(define (init str)
   (set 'Inickname str)
   (set 'Iconnected nil)
   (set 'Ichannels '())
   (set 'Itime-stamp (time-of-day)))

(define (connect server port)
    (set 'Iserver     (net-connect server port))
    (net-send Iserver (format "USER %s %s %s :%s\r\n" Inickname Inickname Inickname Inickname))
    (net-send Iserver (format "NICK %s \r\n" Inickname))
    (set 'Iconnected true)
    (do-callbacks "connect" (list (list "server" server) (list "port" port))))

(define (identify password)
    (net-send Iserver (format "PRIVMSG nickserv :identify %s\r\n" password)))

(define (join-channel channel)
    (when (net-send Iserver (format "JOIN %s \r\n" channel))
          (push channel Ichannels)
          (do-callbacks "join-channel" (list (list "channel" channel) (list "nickname" Inickname)))))

(define (part chan)
    (if-not (empty? chan)
        ; leave specified
        (begin
            (net-send Iserver (format "PART %s\r\n" chan))
            (replace channel Ichannels)
            (do-callbacks "part" (list (list "channel" channel))))
        ; leave all
        (begin
            (dolist (channel Ichannels)
                (net-send Iserver (format "PART %s\r\n" channel))
                (replace channel Ichannels)
                (do-callbacks "part" (list (list "channel" channel)))))))

(define (do-quit message)
    (do-callbacks "quit" '()) ; chance to do stuff before quit...
    (net-send Iserver (format "QUIT :%s\r\n" message))
    (sleep 1000)
    (set 'Ichannels '())
    (close Iserver)
    (set 'Iconnected nil))

(define (privmsg user message)
    (net-send Iserver (format "PRIVMSG %s :%s\r\n" user message)))

(define (notice user message)
    (net-send Iserver (format "NOTICE %s :%s\r\n" user message)))

(define (send-to-server message (channel nil))
    (cond
        ((starts-with message {/}) ; default command character
            (set 'the-message (replace "^/" (copy message) {} 0)) ; keep original
            (net-send Iserver (format "%s \r\n" the-message)) ; send it
            ; do a quit
            (if (starts-with (lower-case the-message) "quit")
                (do-quit { enough})))
        (true 
            (if (nil? channel)
                ; say to all channels
                (dolist (c Ichannels)
                        (net-send Iserver (format "PRIVMSG %s :%s\r\n" c message)))
                ; say to specified channel
                (if (find channel Ichannels)
                    (net-send Iserver (format "PRIVMSG %s :%s\r\n" channel message))))))
    (do-callbacks "send-to-server" (list (list "channel" channel) (list "message" message))))

(define (process-command sender command text)
    (cond
        ((= sender "PING")
            (net-send Iserver (format "PONG %s\r\n" command)))
        ((or (= command "NOTICE") (= command "PRIVMSG"))
            (process-message sender command text))
        ((= command "JOIN")
            (set 'username (first (clean empty? (parse sender {!|:} 0))))
            (set 'channel  (last  (clean empty? (parse sender {!|:} 0))))
            (println {username } username { joined } channel)
            (do-callbacks "join" (list (list "channel" channel) (list "username" username))))
        (true
            nil)))

(define (process-message sender command text)
    (let ((username {} target {} message {}))
        (set 'username (first (clean empty? (parse sender {!|:} 0))))
        (set 'target   (trim  (first (clean empty? (parse text {!|:} 0)))))
        (set 'message  (slice text (+ (find {:} text) 1)))
        (cond 
            ((starts-with message "\001")
                (process-ctcp username target message))
            ((find target Ichannels)
                (cond 
                    ((= command {PRIVMSG})
                        (do-callbacks "channel-message" (list (list "channel" target) (list "username" username) (list "message" message))))
                    ((= command {NOTICE})
                        (do-callbacks "channel-notice"  (list (list "channel" target) (list "username" username) (list "message" message))))))
            ((= target Inickname)
                (cond 
                    ((= command {PRIVMSG})
                        (do-callbacks "private-message" (list (list "username" username) (list "message" message))))
                    ((= command {NOTICE})
                        (do-callbacks "private-notice"  (list (list "username" username) (list "message" message))))))
            (true                
                nil))))
  
(define (process-ctcp username target message)
    (cond
        ((starts-with message "\001VERSION\001")
            (net-send Iserver (format "NOTICE %s :\001VERSION %s\001\r\n" username message)))
        ((starts-with message "\001PING")
            (set 'data (first (rest (clean empty? (parse message { } 0)))))
            (set 'data (trim data "\001" "\001"))
            (net-send Iserver  (format "NOTICE %s :\001PING %s\001\r\n" username data)))
        ((starts-with message "\001ACTION")
;            (set 'data (first (rest (clean empty? (parse message { } 0)))))
;            (set 'data (join data { }))
;            (set 'data (trim data "\001" "\001"))
            (if (find target Ichannels)
                (do-callbacks "channel-action" (list (list "username" username) (list "message" message))))
            (if (= target Inickname)
                (do-callbacks "private-action" (list (list "username" username) (list "message" message)))))
        ((starts-with message "\001TIME\001")
            (net-send Iserver (format "NOTICE %s:\001TIME :%s\001\r\n" username (date))))))

(define (parse-buffer raw-buffer)
    (let ((messages (clean empty? (parse raw-buffer "\r\n" 0)))
          (sender {} command {} text {}))
        ; check for elapsed time since last activity    
        (when (> (sub (time-of-day) Itime-stamp) (mul Idle-time 1000))
              (do-callbacks "idle-event")
              (set 'Itime-stamp (time-of-day)))
        (dolist (message messages)
            (set 'message-parts (parse message { }))           
            (unless (empty? message-parts)
                (set 'sender (first message-parts))
                (catch (set 'command (first (rest message-parts))) 'error)
                (catch (set 'text (join (rest (rest message-parts)) { })) 'error))
            (process-command sender command text))))

(define (read-irc)
    (let ((buffer {}))
        (when (!= (net-peek Iserver) 0) 
              (net-receive Iserver buffer 8192 "\n")
              (unless (empty? buffer)
                (parse-buffer buffer)))))

(define (read-irc-loop) ; monitoring
    (let ((buffer {}))       
        (while Iconnected    
            (read-irc)
            (sleep 1000))))

(define (print-raw-message data) ; example of using a callback
    (set 'raw-data (lookup "message" data))
    (set 'channel  (lookup "channel" data))
    (set 'message-text raw-data)
    (println (date (date-value) 0 {%H:%M:%S }) username {> } message-text))

(define (print-outgoing-message data)
    (set 'raw-data (lookup "message" data))
    (set 'channel  (lookup "channel" data))
    (set 'message-text raw-data)
    (println (date (date-value) 0 {%H:%M:%S }) Inickname {> } message-text))

(define (session); interactive terminal
    ; must add callbacks to display messages
    (register-callback "channel-message" 'print-raw-message)
    (register-callback "send-to-server"  'print-outgoing-message)
    (while Iconnected
        (while (zero? (peek 0))
            (read-irc)
            (sleep 1000))
        (send-to-server (string (read-line 0))))
    (println {finished session } (date))
    (exit))

; end of IRC code

[text]

simple bot code:
(load (string (env {HOME}) {/projects/programming/newlisp-projects/irc.lsp}))
(context 'BOT)
(define bot-name "bot")
(define (join-channel data)
		(println {in BOT:join-channel with data: } data))
(define (process-message data)
        ????)
(IRC:register-callback "join-channel"    'join-channel) 
(IRC:register-callback "channel-message" 'process-message)
(IRC:register-callback "idle-event"      'do-idle-event)
(IRC:register-callback "send-to-server"  'do-send-event)
(IRC:init bot-name)
(IRC:connect "irc.freenode.net" 6667)
(IRC:join-channel {#newlisp})
(IRC:read-irc-loop)
[/text]