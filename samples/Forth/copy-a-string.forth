\ Allocate two string buffers
create stringa 256 allot
create stringb 256 allot

\ Copy a constant string into a string buffer
s" Hello" stringa place

\ Copy the contents of one string buffer into another
stringa count  stringb place
