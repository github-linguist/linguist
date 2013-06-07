;; main.nu
;;  Entry point for a Nu program.
;;
;;  Copyright (c) 2007 Tim Burks, Neon Design Technology, Inc.

(load "Nu:nu")     ;; basics
(load "Nu:cocoa")  ;; cocoa definitions
(load "Nu:menu")   ;; menu generation
(load "randomapp") ;; Aaron Hillegass' famous example

;; define the application delegate class
(class ApplicationDelegate is NSObject
     (imethod (void) applicationDidFinishLaunching: (id) sender is
          (build-menu default-application-menu "RandomApp")
          (set $random ((RandomAppWindowController alloc) init))))

;; install the delegate and keep a reference to it since 
;; the application won't retain it.
((NSApplication sharedApplication) setDelegate:
    (set delegate ((ApplicationDelegate alloc) init)))

;; this makes the application window take focus when
;;  we've started it from the terminal
((NSApplication sharedApplication) activateIgnoringOtherApps:YES)

;; run the main Cocoa event loop
(NSApplicationMain 0 nil)
