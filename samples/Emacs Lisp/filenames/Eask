;; -*- mode: eask; lexical-binding: t -*-

(package "setup-emacs-windows"
         "4.0.0"
         "A Github Action that installs a specific emacs version")

(website-url "https://github.com/jcs090218/setup-emacs-windows")
(keywords "ci" "gha")

(package-file "setup-windows.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)
(source 'jcs-elpa)

(depends-on "emacs" "26.1")
(depends-on "request")
(depends-on "s")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
