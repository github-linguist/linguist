include ffl/dom.fs

\ Create a dom variable 'doc' in the dictionary

dom-create doc

\ Add the document root with its version attribute

dom.document doc dom-append-node

s" version" s" 1.0" dom.attribute doc dom-append-node

\ Add root and element

doc dom-parent 2drop

s" root"    dom.element doc dom-append-node

s" element" dom.element doc dom-append-node

\ Add the text

s" Some text here" dom.text doc dom-append-node

\ Convert the document to a string and print

doc dom-write-string [IF]
  type cr
[THEN]
