(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

; Key bindings
(define-key viper-vi-global-user-map "\C-d" 'end-of-line)

; Return to top of window
(defun my-viper-return-to-top ()
  (interactive)
  (beginning-of-buffer))
