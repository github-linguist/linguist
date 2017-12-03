(setq user-full-name "Alhadis")
(setq user-mail-address "fake.account@gmail.com")

(auto-image-file-mode)
(setq mm-inline-large-images t)
(add-to-list 'mm-attachment-override-types "image/*")

(setq gnus-select-method 
	  '(nnimap "gmail"
		(nnimap-address "imap.gmail.com")
		(nnimap-server-port 777)
		(nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 600 nil nil))
	  smtpmail-auth-credentials '(("smtp.gmail.com" 700 "me@lisp.com" nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 800
	  setq gnus-ignored-from-addresses "^from\\.Telstra[ \t\r\n]+Thanks")
