;; -*- mode: eask; lexical-binding: t -*-

(package "composer"
         "0.2.0"
         "Interface to PHP Composer")

(website-url "https://github.com/emacs-php/composer.el")
(keywords "tools" "php" "dependency" "manager")

(package-file "composer.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "25.1")
(depends-on "seq")
(depends-on "php-runtime")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
