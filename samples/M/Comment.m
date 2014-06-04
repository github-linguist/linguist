Comment ;
  ; this is a comment block
  ; comments always start with a semicolon
  ; the next line, while not a comment, is a legal blank line
  
  ;whitespace alone is a valid line in a routine
  ;** Comments can have any graphic character, but no "control"
  ;** characters
  
  ;graphic characters such as: !@#$%^&*()_+=-{}[]|\:"?/>.<,
  ;the space character is considered a graphic character, even 
  ;though you can't see it.
  ; ASCII characters whose numeric code is above 128 and below 32
  ; are NOT allowed on a line in a routine.
  ;; multiple semicolons are okay
  ; a line that has a tag must have whitespace after the tag, bug
  ; does not have to have a comment or a command on it
Tag1  
  ;
  ;Tags can start with % or an uppercase or lowercase alphabetic
  ; or can be a series of numeric characters
%HELO ;
 ;
0123 ;
  ;
%987  ;
  ; the most common label is uppercase alphabetic
LABEL ;
  ; 
  ; Tags can be followed directly by an open parenthesis and a
  ; formal list of variables, and a close parenthesis
ANOTHER(X) ;
  ;
  ;Normally, a subroutine would be ended by a QUIT command, but we
  ; are taking advantage of the rule that the END of routine is an
  ; implicit QUIT
