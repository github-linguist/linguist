#lang at-exp racket
(require xml)

(define xml-str
  @~a{<?xml version="1.0" ?>
       <root>
           <element>
               Some text here
           </element>
       </root>})

;; read & parse to get an xml value
(define xml (read-xml/document (open-input-string xml-str)))
;; print it out in xml form, which is identical to the input xml
(write-xml xml)
(newline)
