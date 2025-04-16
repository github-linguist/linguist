;; UTF-8 support
;; (set-language-environment "UTF-8")
(setenv "LANG" "en_AU.UTF-8")
(setenv "LC_ALL" "en_AU.UTF-8")
(setq default-tab-width 4)


;;; Function to load all ".el" files in ~/.emacs.d/config
(defun load-directory (directory)
  "Recursively load all Emacs Lisp files in a directory."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;; Tell Emacs we'd like to use Hunspell for spell-checking
(setq ispell-program-name (executable-find "hunspell"))

;; Load Homebrew-installed packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(load "aggressive-indent")
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Load Git-related syntax highlighting
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "git-modes")
(load "git-commit")

;; Keybindings
(global-set-key (kbd "C-u") (lambda ()
                             (interactive)
                             (kill-line 0)))

;; Show cursor's current column number
(setq column-number-mode t)

;; Disable autosave
(setq auto-save-default nil)

;; Use a single directory for storing backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-save-list")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
