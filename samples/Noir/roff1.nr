.\"##########################################################################
.\"# This program is Copyright (C) 1986-2002 by Jonathan Payne.  JOVE is    #
.\"# provided by Jonathan and Jovehacks without charge and without          #
.\"# warranty.  You may copy, modify, and/or distribute JOVE, provided that #
.\"# this notice is included in all the source files and documentation.     #
.\"##########################################################################
.\"
.\" cmds.nr: descriptions of each JOVE command and variable.
.\"
.\" This nroff/troff text is used for two purposes:
.\" it is the second portion of the JOVE manual (combine with intro.nr), and
.\" it is used to produce cmds.doc, the text used by the describe-command
.\" and describe-variable commands.
.\" Take note of the .IQ and .BQ macros used in this file to put text in
.\" italic or bold, and that their effect when producing cmds.doc is to
.\" enclose the relevant text within quotes.  Do not use any other means of
.\" italicising or emboldening in this file.
.
.dc "abort-char" "(variable)"
This variable defines \s-2JOVE\s0'S abort character.  When JOVE
reads this character from the keyboard, it stops what it is doing
(unless the character is quoted in some way).  Unfortunately, JOVE
won't notice the character until it reads from the keyboard.  The
default value is ^G.  See also
.IQ interrupt-character .
.dc "add-lisp-special" "Not Bound"
This command is to tell \s-2JOVE\s0 what identifiers require special
indentation in lisp mode.  Lisp functions like
.IQ defun
and
.IQ let
are two of
the default functions that get treated specially.  This is just a kludge
to define some of your own.  It prompts for the function name.
.dc "allow-^S-and-^Q" "(variable)"
This variable, when set, tells \s-2JOVE\s0 that your terminal will not need
to use the characters ^S and ^Q for flow control, in which case \s-2JOVE\s0
will instruct the system's tty driver to pass them through as normal
characters.  Otherwise, if the tty driver was already using these characters
for flow control, it will continue to do so.  Certain terminals and
communications systems require that this variable be set
.IQ off ;
in other circumstances it is better set
.IQ on .
.dc "allow-bad-characters-in-filenames" "(variable)"
If set, this variable permits the creation of filenames which contain
\*Qbad\*U characters
such as those from the set \(**&%!"`[]{}.  These files are harder to deal
with, because the characters mean something to the shell.  The default
value is
.IQ off .
.dc "append-region" "Not Bound"
This appends the region to a specified file.  If the file does not
already exist it is created.
.dc "apropos" "Not Bound"
This types out each command, variable and macro with the specified
string in its name (\*Q?\*U matches every name).
For each command and macro that contains the
string, the key sequence that can be used to execute the command or macro is
printed; with variables, the current value is printed.  So, to find all the
commands that are related to windows, you type
.DS
: apropos window<Return> .
.DE
.dc "auto-case-abbrev" "(variable)"
When this variable is
.IQ on
(the default), word abbreviations are adjusted for
case automatically.  If the abbreviation is typed with no uppercase letter,
the expansion is not changed; if it is typed with one or more uppercase letters,
the first character in the expansion is capitalized;
additionally, if the abbreviation is typed with more than one uppercase letter,
each letter in the expansion immediately preceded by whitespace or \-
is capitalized.
For example, if \*Qjove\*U were the abbreviation for
\*Qjonathan's own version of EMACS\*U, the following table shows how the
abbreviation would be expanded.
.DS I
.ta \w'JOVE111'u
jove	jonathan's own version of EMACS
Jove	Jonathan's own version of EMACS
JOVE	Jonathan's Own Version Of EMACS
JoVe	Jonathan's Own Version Of EMACS
.DE
When this variable is
.IQ off ,
upper and lower case are
distinguished when looking for the abbreviation, i.e., in the example above,
\*QJOVE\*U and \*QJove\*U would not be expanded unless they were defined separately.
See also the
.IQ word-abbrev-mode
command.
.dc "auto-execute-command" "Not Bound"
This tells \s-2JOVE\s0 to execute a command automatically when a file whose
name matches a specified pattern is read.  The first argument is the
command you wish to have executed.
The second argument is the pattern, a regular expression
that is matched against the start of the file name.
If you wish to match a suffix, start the pattern with \*Q.\(**\*U;
to match every file, use that as the whole pattern.
Any numeric argument
will be passed on to the command when it is executed (this is
useful when combined with commands that adjust a minor mode).
For example, if you want
to be in
.IQ show-match-mode
when you edit C source files (that is, files
that end with
.BQ .c
or
.BQ .h )
you can type
.DS
: auto-execute-command show-match-mode .\(**\^\e\^.[ch]$
.DE
Actually, this command toggles the Show Match minor mode,
but since it is initially off, it will have the desired effect.  For
more certain control, give the
.IQ auto-execute-command
a non-zero numeric
argument: this will be passed on to the
.IQ show-match-mode .
.dc "auto-execute-macro" "Not Bound"
This is like
.IQ auto-execute-command
except you use it to execute macros
automatically instead of built-in commands.
.dc "auto-fill-mode" "Not Bound"
This turns on or off the Auto Fill minor mode in the selected buffer.
Without a numeric argument, the command toggles the mode; with a
zero argument, the mode is turned off; with a non-zero argument,
the mode is turned on.  When \s-2JOVE\s0 is in Auto Fill mode it
automatically breaks lines for you when you reach the right margin
so you don't have to remember to hit Return.  \s-2JOVE\s0 uses 78 as
the right margin but you can change that by setting the variable
.IQ right-margin
to another
value.
.dc "auto-indent-mode" "Not Bound"
This turns on or off Auto Indent minor mode in the selected buffer.
Without a numeric argument, the command toggles the mode; with a
zero argument, the mode is turned off; with a non-zero argument,
the mode is turned on.  When \s-2JOVE\s0 is in Auto Indent mode,
the
.IQ newline
command (which is normally bound to Return) acts identically to
.IQ newline-and-indent :
the new line is indented to the same position as the line you
were just on.  This is useful for lining up C code (or any other
language (but what else is there besides C?)).
Furthermore, if a line is broken because of Auto Fill mode, and
Auto Indent mode is on, the new line will be indented as the old line was.
.dc "backward-character" "^B"
This moves point backward over a single character or line-separator.
Thus if point is at the
beginning of the line it moves to the end of the previous line.
.dc "backward-list" "ESC\ ^P"
This moves point backward over a list, which is any text between properly
matching (\^...\^), [\^...\^] or {\^...\^}.
It first searches backward for a \*Q)\*U and then moves to the matching \*Q(\*U.
This is useful when you are trying to find unmatched parentheses in a program.
Arguments are accepted, and negative arguments search forwards.
See also
.IQ backward-s-expression .
.dc "backward-paragraph" "Usually Not Bound"
This moves point backward to the beginning of the current or previous
paragraph.  Paragraphs are bounded by lines that match
.IQ paragraph-delimiter-pattern
(by default, those that are empty or look like troff or TeX commands).
A change in indentation may also signal a break
between paragraphs, except that \s-2JOVE\s0 allows the first line of a
paragraph to be indented differently from the other lines.
Arguments are accepted, and negative arguments search forwards.
.dc "backward-s-expression" "ESC\ ^B"
This moves point backward over an s-expression, that is over a Lisp atom or
a C identifier (depending on the major mode) ignoring punctuation and
whitespace; or, if the nearest preceding significant character is one of
\*Q)]}\*U, over a list as in
.IQ backward-list .
Arguments are accepted, and negative arguments search forwards.
.dc "backward-sentence" "ESC\ A"
This moves point backward to the beginning of the current or previous
sentence.  \s-2JOVE\s0 considers the end of a sentence to be the characters
\*Q.\*U, \*Q!\*U or \*Q?\*U followed by a Return or by one or more spaces.
Arguments are accepted, and negative arguments search forwards.
.dc "backward-up-list" "ESC\ ^U"
This is similar to
.IQ backward-list
except it backs up and OUT of the enclosing list.  In other
words, it moves backward to whichever of \*Q([{\*U would match one of
\*Q)]}\*U if you were to type it right then.
Arguments are accepted, and negative arguments search forwards as in
.IQ down-list .
.dc "backward-word" "ESC\ B"
This moves point backward to the beginning of the current or previous
word.
Arguments are accepted, and negative arguments search forwards.
.dc "bad-filename-extensions" "(variable)"
This contains a list of words separated by spaces which are to be
considered bad filename extensions, and so will not be included in
filename completion.  The default contains, amongst much else,
.BQ .o
so if you have
.BQ jove.c
and
.BQ jove.o
in the same directory, the filename completion will not complain
of an ambiguity because it will ignore
.BQ jove.o .
.dc "begin-kbd-macro" "^X\ ("
This starts defining the keyboard macro by remembering all your key
strokes until you execute
.IQ end-kbd-macro ,
by typing \*Q^X\ )\*U.  Because of a bug in \s-2JOVE\s0 you shouldn't
terminate the macro by typing \*QESC\ X end-kbd-macro\*U;
.IQ end-kbd-macro
must be bound to \*Q^X\ )\*U in order to make things work correctly.
The
.IQ execute-kbd-macro
command will execute the remembered key strokes.
Sometimes you may want a macro to accept different input each time it runs.
To see how to do this, see the
.IQ make-macro-interactive
command.
.dc "beginning-of-file" "ESC\ <"
This moves point backward to the beginning of the buffer.  This sometimes
prints the \*Q[Point pushed]\*U message to indicate that
\s-2JOVE\s0 has set the mark so you can go back to where you were
if you want.
See also the variable
.IQ mark-threshold .
.dc "beginning-of-line" "^A"
This moves point to the beginning of the current line.
.dc "beginning-of-window" "ESC\ ,"
This moves point to the beginning of the active window.  If there
is a numeric argument, point moves that many lines below the top
line.  With the default bindings, the sequence \*QESC\ ,\*U is the same as
\*QESC\ <\*U (\c
.IQ beginning-of-file )
but without the shift key on the \*Q<\*U,
and can thus easily be remembered.
.dc "bind-keymap-to-key" "Not Bound"
This is like
.IQ bind-to-key
except that you use it to attach a key sequence to a named keymap.
The only reasonable use is to bind some extra key to
.IQ ESC-map
for keyboards that make typing ESC painful.
.dc "bind-macro-to-key" "Not Bound"
This is like
.IQ bind-to-key
except you use it to attach a key sequence to a named macro.
.dc "bind-macro-to-word-abbrev" "Not Bound"
This command allows you to bind a macro to a previously defined word
abbreviation.  Whenever you type the abbreviation, it will first be
expanded as an abbreviation (which could be empty, of course), and then
the macro will be executed.
Note that if the macro moves point around, you should first
.IQ set-mark
and then
.IQ exchange-point-and-mark .
.dc "bind-to-key" "Not Bound"
This attaches a key sequence to an internal \s-2JOVE\s0 command so
that future hits on that key sequence invoke that command.  This is
called a global binding, as compared to local bindings and process
bindings.  Any previous global binding of this key sequence is
discarded.  For example, to make \*Q^W\*U erase the previous word, you
type
.DS
: bind-to-key kill-previous-word ^W .
.DE
It isn't possible to have two globally bound key sequences where one
is a prefix of the other: \s-2JOVE\s0 wouldn't know whether to obey
the shorter sequence or wait for the longer sequence.  Normally, when
the
.IQ bind-to-key
command is issued interactively, the key sequence is taken to end
one keystroke after the longest sequence matching any proper prefix
of another binding (thus no new prefix can be created).  If the command
is given a numeric argument, the key sequence is taken up to the
next Return keystroke (kludge!); bindings to any prefix of the
sequence are discarded.  When the command is issued from a
.IQ source d
file, the
key sequence is taken up to the end of the line (it is also
processed so that control characters can and should be entered using
the ^A notation).

Note that neither process nor local bindings are changed by
this command, although they can be eclipsed.  Given a choice between
bindings, the shortest is executed; if there is still a choice,
a process binding is preferred to a local binding, and a local
binding is preferred to a global binding.
.dc "buffer-position" "Not Bound"
This displays the current file name, current line number, total number
of lines, current character number, total number of characters,
percentage of the way through the file, and the position of
the cursor in the current line.
.dc "c-argument-indentation" "(variable)"
This variable describes how to indent lines which are part of nested
expressions in C.  The default is \-1, which means to indent a continued
line by lining it up with the first argument of the current expression.
Otherwise, the line will be indented by
.IQ c-argument-indentation
characters
past the indent of the first line of the expression.  For example, the
default value produces:
.DS
.nr TY \w'Typeout('
Typeout(fmt, itoa(bcount++), line_cnt(b, nbuf),
\h'\n(TYu'TypeNames[b->b_type],
\h'\n(TYu'IsModified(b) ? "\(**" : b->b_ntbf ? "+" : NullStr,
\h'\n(TYu'buf_width, b->b_name, filename(b));
.DE
.dc "c-indentation-increment" "(variable)"
This defines a set of tabstops independent of the value of
.IQ tab-width .
This value will be used in C
mode, and \s-2JOVE\s0 will insert the correct number of Spaces and Tabs to
get the right behavior.  For programmers that like to indent with 4 spaces,
set this value to 4.  Some people prefer to set this to 4 and leave
tab-width set to 8.  This will create files whose indentation steps in
4-space increments, and which look the same anywhere that tabs are expanded
to 8 spaces (i.e. in most settings).  Others prefer to have one tab
character per indentation level, then fiddle the tab expansion width to get
the appearance they like.  They should set both
.IQ c-indentation-increment
and
.IQ tab-width
to 4.  Whenever using a non-standard tab width
(\c
.IQ tab-width )
you should only use tabs for indentation, and use
spaces for all columnar alignment later in the lines.
.dc "c-mode" "Not Bound"
This turns on the C major mode in the currently selected buffer.
When in C or Lisp mode, Tab, \*Q}\*U, and \*Q)\*U behave a little differently
from usual: They are indented to the \*Qright\*U place for C (or Lisp)
programs.  In \s-2JOVE\s0, the \*Qright\*U place is simply the way the author
likes it (but I've got good taste).
.dc "case-character-capitalize" "Not Bound"
This capitalizes the character after point, i.e., the character under
the cursor.  If a negative argument is supplied that many characters
before point are upper cased.
.dc "case-ignore-search" "(variable)"
This variable, when
.IQ on ,
tells \s-2JOVE\s0 to treat upper and lower case
the same when searching.  Thus \*Qjove\*U would match \*QJOVE\*U, and
\*QJoVe\*U would match either.  The default value of this variable is
.IQ off .
.dc "case-region-lower" "Not Bound"
This changes all the upper case letters in the region to their lower
case equivalents.
.dc "case-region-upper" "Not Bound"
This changes all the lower case letters in the region to their upper
case equivalents.
.dc "case-word-capitalize" "ESC\ C"
This capitalizes the current word by making the current letter upper
case and making the rest of the word lower case.  Point is moved to
the end of the word.  If point is not positioned on a word it is first
moved forward to the beginning of the next word.  If a negative
argument is supplied that many words
before point are capitalized.
This is useful for correcting the word just typed without having to
move point to the beginning of the word yourself.
.dc "case-word-lower" "ESC\ L"
This lower-cases the current word and leaves point at the end of it.
If point is in the middle of a word the rest of the word is
converted.  If point is not in a word it is first moved forward to the
beginning of the next word.  If a negative argument is supplied that
many words before
point are converted to lower case.  This is useful
for correcting the word just typed without having to move point to the
beginning of the word yourself.
.dc "case-word-upper" "ESC\ U"
This upper-cases the current word and leaves point at the end of it.
If point is in the middle of a word the rest of the word is
converted.  If point is not in a word it is first moved forward to the
beginning of the next word.  If a negative argument is supplied that
many words before
point are converted to upper case.  This is useful
for correcting the word just typed without having to move point to the
beginning of the word yourself.
.dc "cd" "Not Bound"
This changes the current directory.
.dc "character-to-octal-insert" "Not Bound"
This inserts a Back-slash followed by the ascii value of the next
character typed.  For example, \*Q^G\*U inserts the string \*Q\^\e\^007\*U.
.dc "clear-and-redraw" "ESC\ ^L"
This clears the entire screen and redraws all the windows.  Use this
when \s-2JOVE\s0 gets confused about what's on the screen, or when the screen
gets filled with garbage characters or output from another program.
.dc "comment-format" "(variable)"
This variable tells \s-2JOVE\s0 how to format your comments when you run the
command
.IQ fill-comment .
Its format is this:
.ID
<open pattern>%!<line header>%c<line trailer>%!<close pattern>
.DE
The %!, %c, and %! must appear in the format; everything else is optional.
A newline (represented by %n) may appear in the open or close patterns.  %%
is the representation for %.  The default comment format is for C comments.
See
.IQ fill-comment
for more details.
.dc "compile-it" "^X\ ^E"
This compiles your program by running the
.UX
command
.IQ make
into a buffer,
and automatically parsing the error messages that are created (if any).  See
the
.IQ parse-errors
command.
If
.IQ compile-it
is given a numeric argument, it will prompt for a command to run in
place of the plain make and the command you enter will become the new
default.  See also
.IQ error-format-string
which makes it possible to parse errors of a different format and
see also the variable
.IQ error-window-size .
.dc "continue-process" "Not Bound"
This sends the signal SIGCONT to the interactive process in the current
buffer, IF the process is currently stopped.
.dc "copy-region" "ESC\ W"
This takes all the text in the region and copies it onto the kill ring
buffer.  This is just like running
.IQ kill-region
followed by the
.IQ yank
command.  See the
.IQ kill-region
and
.IQ yank
commands.
.dc "current-error" "Not Bound"
This moves to the current error in the list of parsed errors.  See the
.IQ next-error
and
.IQ previous-error
commands for more detailed
information.
.dc "date" "Not Bound"
This prints the date on the message line.
.dc "dbx-format-string" "(variable)"
This is the default regular-expression search string used by \s-2JOVE\s0 to
parse output from
.IQ dbx
running in a shell process (see the
.IQ dbx-mode
command).  You shouldn't have to change
this unless you are using something other than
.IQ DBX .
.dc "dbx-mode" "Not Bound"
This turns on or off the DBX minor mode in the selected
buffer.  Without a numeric argument, the command toggles the mode;
with a zero argument, the mode is turned off; with a non-zero
argument, the mode is turned on.
This mode only makes sense in a buffer running an interactive shell
process.  If you are running
.IQ dbx
in a window and and the buffer is in DBX minor mode, \s-2JOVE\s0 will
automatically track the source location in another window.
Whenever you type \*Qwhere\*U or while you're stepping through a
program, or when you reach a breakpoint, \s-2JOVE\s0 will present the
source file in another window and move to the line that is being referenced.
See also the variable
.IQ dbx-format-string .
.dc "define-global-word-abbrev" "Not Bound"
This defines a global abbreviation.
See the
.IQ word-abbrev-mode
command.
.dc "define-macro" "Not Bound"
This provides a different mechanism for defining keyboard macros.
Instead of gathering keystrokes and storing them into the
\*Qkeyboard-macro\*U (which is how
.IQ begin-kbd-macro
works),
.IQ define-macro
prompts for a macro name (terminated with Space, or Newline) and then for
the actual macro body.  If you wish to specify control characters in the
macro, you may simply insert them (using the
.IQ quoted-insert
command) or by inserting the character '^' followed by the appropriate
letter for that character (e.g., ^A would be the two characters '^'
followed by 'A').  You may use Back-slash to prevent the '^' from being
interpreted as part of a control character when you really wish to insert
one (e.g., a macro body \*Q\^\e\^^foo\*U would insert the string \*Q^foo\*U into the
buffer, whereas the body \*Q^foo\*U would be the same as typing ^F and then
inserting the string \*Qoo\*U).  See
.IQ write-macros-to-file
to see how to save macros.
.dc "define-mode-word-abbrev" "Not Bound"
This defines a mode-specific abbreviation.
See the
.IQ word-abbrev-mode
command.
.dc "delete-blank-lines" "^X\ ^O"
This deletes all the blank lines around point.  This is useful when you
previously opened many lines with the
.IQ newline-and-backup
command and now wish to delete the unused ones.
.dc "delete-buffer" "^X\ K"
This deletes a buffer and frees up all the memory associated with it.  Be
careful(!) - once a buffer has been deleted it is gone forever.  \s-2JOVE\s0
will ask you to confirm if you try to delete a buffer that needs saving.
This command is useful for when \s-2JOVE\s0 runs out of space to store
new buffers.
See also the
.IQ erase-buffer
command and the
.IQ kill-some-buffers
command.
.dc "delete-current-window" "^X\ D)"
This deletes the active window and moves point into one of the
remaining ones.  It is an error to try to delete the only remaining
window.
.dc "delete-next-character" "^D"
This deletes the character that's just after point (that is, the
character under the cursor).  If point is at the end of a line, the
line-separator is deleted and the next line is joined with the current
one.
If an argument is given, that many characters are deleted and placed on the
kill ring.  If the argument is negative the deletion is forwards.
.dc "delete-other-windows" "^X\ 1"
This deletes all the other windows except the current one.  This can be
thought of as going back into One Window mode.
.dc "delete-previous-character" "DEL and ^H"
This deletes the character that's just before point (that is, the
character before the cursor).  If point is at the beginning of the
line, the line separator is deleted and that line is joined with the
previous one.
If an argument is given, that many characters are deleted and placed on the
kill ring.  If the argument is negative the deletion is backwards.
.dc "delete-white-space" "ESC\ \e\^"
This deletes all the Tabs and Spaces around point.
.dc "describe-bindings" "Not Bound"
This types out a list containing each bound key and the command that gets
invoked every time that key is typed.  To make a wall chart of \s-2JOVE\s0
commands, set
.IQ send-typeout-to-buffer
to
.IQ on
and \s-2JOVE\s0 will
store the key bindings in a buffer which you can save to a file and then
print.
.dc "describe-command" "ESC\ ?"
This waits for you to type a command and then prints an explanation of that
command, together with its current bindings.
.dc "describe-key" "^X\ ?"
This waits for you to type a key and then tells the name of the
command that gets invoked every time that key is hit.  Once you have
the name of the command you can use the
.IQ describe-command
command
to find out exactly what it does.
.dc "describe-variable" "Not Bound"
This prints an explanation of a specified variable.
.dc "digit" "ESC\ 0 through ESC\ 9"
Starts or continues the entry of a numeric argument with the digit typed.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
.dc "digit-0" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 0.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 0 key on the numeric keypad.
.dc "digit-1" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 1.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 1 key on the numeric keypad.
.dc "digit-2" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 2.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 2 key on the numeric keypad.
.dc "digit-3" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 3.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 3 key on the numeric keypad.
.dc "digit-4" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 4.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 4 key on the numeric keypad.
.dc "digit-5" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 5.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 5 key on the numeric keypad.
.dc "digit-6" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 6.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 6 key on the numeric keypad.
.dc "digit-7" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 7.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 7 key on the numeric keypad.
.dc "digit-8" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 8.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 8 key on the numeric keypad.
.dc "digit-9" "Not Bound"
Starts or continues the entry of a numeric argument with the digit 9.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the 9 key on the numeric keypad.
.dc "digit-minus" "ESC\ \-"
Starts the entry of a numeric argument with a minus sign.
It continues reading digits until you type some other command.
Then that command is executed with the numeric argument you specified.
Sometimes it is handy to bind this to the \- key on a numeric keypad.
In the absence of further digits and unless otherwise stated (e.g.
.IQ next-page "),"
the argument \-1 is assumed.
.dc "dirs" "Not Bound"
This prints out the directory stack.  See the
.IQ cd ,
.IQ pushd ,
.IQ pushlibd
and
.IQ popd
commands for more information.
.dc "disable-biff" "(variable)"
When this is set, \s-2JOVE\s0 disables biff when you're editing and enables it
again when you get out of \s-2JOVE\s0, or when you pause to the parent shell
or push to a new shell.  (This means arrival of new mail will not be
immediately apparent but will not cause indiscriminate writing on the
display).  The default is
.IQ off ,
although it is always safe to set it
.IQ on ,
even on systems that do not provide the biff facility.  Note that
the variable
.IQ mode-line
can be set up to announce the arrival of new
mail during a \s-2JOVE\s0 session.
.dc "display-default-filenames" "(variable)"
If this is set when \s-2JOVE\s0 asks for a filename, it will display the default
(unless that would take too much of the prompt line).
.dc "display-filenames-with-bad-extensions" "(variable)"
This variable affects only filename completion, in particular, what
happens when \*Q?\*U is typed while prompting for a file.  When this variable
is
.IQ on ,
any files that end with one of the extensions defined by the
variable
.IQ bad-filename-extensions
will be displayed with an \*Q!\*U in front of their names.  When
.IQ display-filenames-with-bad-extensions
is
.IQ off
the files will not be displayed at all.  The default value
is
.IQ on .
.dc "down-list" "ESC\ ^D"
This is the opposite of
.IQ backward-up-list .
It enters the next list.  In other
words, it moves forward to whichever of \*Q([{\*U it first encounters.
Arguments are accepted, and negative arguments search backwards as in
.IQ backward-up-list .
.dc "dstop-process" "Proc: ^C\ ^Y"
Send the signal SIGTSTP to the interactive process in the selected buffer when
next it tries to read input.  This is equivalent to sending the \*Qdsusp\*U
character (which most people have set to ^Y) to the process.
This only works if you are in a buffer bound to an interactive process.
.dc "edit-word-abbrevs" "Not Bound"
This creates (if necessary) a buffer with a list of each abbreviation and
the phrase
it expands into, and enters a recursive edit to let you change the
abbreviations or add some more.  The format of this list is
\*Qabbreviation:phrase\*U so if you add some more you should follow that
format.  It's probably simplest just to copy some already existing
abbreviations and edit them.  Use the
.IQ exit-jove
command to exit the recursive edit.
.dc "end-kbd-macro" "^X\ )"
This stops the definition of the keyboard macro.  Because of a bug in
\s-2JOVE\s0, this must be bound to \*Q^X\ )\*U, or some key sequence which is
one or two characters long.  Anything else will not work properly.
See
.IQ begin-kbd-macro
for more details.
.dc "end-of-file" "ESC\ >"
This moves point forward to the end of the buffer.  This sometimes
prints the \*Q[Point pushed]\*U message to indicate that
\s-2JOVE\s0 has set the mark so you can go back to where you were
if you want.
See also the variable
.IQ mark-threshold .
.dc "end-of-line" "^E"
This moves point to the end of the current line.  If the line is too
long to fit on the screen, it will be scrolled horizontally.
This is described with the variables
.IQ scroll-width
and
.IQ scroll-all-lines .
.dc "end-of-window" "ESC\ ."
This moves point to the last character in the active window.  If there is a
numeric argument, the point moves that many lines above the bottom
line.  With the default bindings, the sequence \*QESC\ .\*U is the same as
\*QESC\ >\*U (\c
.IQ end-of-file )
but without the shift key on the \*Q>\*U,
and can thus easily be remembered.
.dc "enhanced-keyboard" "(variable)"
(IBM PC version only) This is a boolean variable which can be set to
enable the enhanced AT-style keyboard.  The enhanced keyboard contains
function keys and key combinations that are not supported on the
original IBM PCs and XTs.  The default value is determined by a bit in
the BIOS data area, but this method apparently does not work with a
few BIOS implementations.  WARNING: setting enhanced-keyboard
.IQ on
on systems without an enhanced keyboard will lock up your
system and require you to reboot.
.dc "eof-process" "^C\ ^D"
Sends EOF to the current interactive process.  This only works on
versions of \s-2JOVE\s0 running under versions of
.UX
with pty's.
.dc "erase-buffer" "Not Bound"
This erases the contents of the specified buffer.  This is like
.IQ delete-buffer
except it only erases the contents of the buffer, not
the buffer itself.  If you try to erase a buffer that needs saving you
will be asked to confirm it.
.dc "error-format-string" "(variable)"
This is the error format string that is used by
.IQ parse-errors
to find the error messages in a buffer.  The way it works is by using
this string as a \s-2JOVE\s0 regular expression search string, where the
\e\^(\^...\^\e) regular expression feature is used to pick out the
file name and line number from the line containing an error message.  For
instance, a typical error message might look like this:
.sp 1
	"file.c", line 540: missing semi-colon
.sp 1
For strings of this format, an appropriate value for
.IQ error-format-string
would be something like this:
.sp 1
	^"\^\e\^([^"]\(**\^\e)", line \e\^([0-9]\(**\^\e):
.sp 1
What this means is, to find an error message, search for a line beginning
with a double-quote.  Then it says that all the following characters up
to another double-quote should be remembered as one unit, namely the
filename that the error is in (that is why the first set of parentheses is
surrounding it).  Then it says that after the filename there will be the
string \*Q, line \*U followed by a line number, which should be remembered as
a single unit (which is why the second set of parentheses is around that).
The only constraint on the error messages is that the file name and line
number appear on the same line.  Most compilers seem to do this anyway, so
this is not an unreasonable restriction.
.sp 1
If you do not know how to use regular expressions then this variable will
be hard for you to use.  Also note that you can look at the default
value of this variable by printing it out, but it is a really complicated
string because it is trying to accommodate the outputs of more than one
compiler.
.dc "error-window-size" "(variable)"
This is the percentage of the screen to use for the error-window on the
screen.  When you execute
.IQ compile-it
or
.IQ spell-buffer ,
.IQ error-window-size
percent of the screen will go to the error window.  If the window already
exists and is a different size, it is made to be this size.  The default
value is 20%.
.dc "exchange-point-and-mark" "^X\ ^X"
This moves point to mark and makes mark the old point.  This is for
quickly moving from one end of the region to the other.
.dc "execute-kbd-macro" "^X\ E"
This executes the keyboard macro.  If you supply a numeric argument the
macro is executed that many times.
See the
.IQ begin-kbd-macro
command for more details.
.dc "execute-macro" "Not Bound"
This executes a specified macro.  If you supply a numeric argument the
macro is executed that many times.
.dc "execute-named-command" "ESC\ X"
This is the way to execute a command that isn't bound to any key.  When you
are prompted with \*Q:\ \*U you can type the name of the command.  You don't
have to type the entire name.  After typing a few characters, Tab will fill
in as many more as it can (as will Space, but that will also obey the
command if it is now unambiguous).  If you are not sure of the name of the
command, type \*Q?\*U and \s-2JOVE\s0 will print a list of all the commands
that you could possibly match given what you've already typed.  Once the
command is unambiguous, typing Return will cause it to be obeyed.

If you don't have any idea what the command's name
is but you know it has something to do with windows (for example), you
can do \*QESC\ X apropos window\*U and \s-2JOVE\s0 will print a list of all
the commands that are related to windows.  If you find yourself
constantly executing the same commands this way you probably want to bind
them to keys so that you can execute them more quickly.  See the
.IQ bind-to-key
command.
.dc "exit-jove" "^X\ ^C"
This exits \s-2JOVE\s0.  If any buffers need saving \s-2JOVE\s0 will print a warning
message and ask for confirmation.  If you leave without saving your
buffers all your work will be lost.  If you made a mistake and really
do want to exit then you can.
If there are any interactive processes running, \s-2JOVE\s0 will also ask
whether they should be terminated.

If you are in a recursive editing level
.IQ exit-jove
will return you from that.  The selected buffer will be set back to
the buffer that was current when the recursive edit was entered.
Normally, point will be returned to its position at the time of
entry, but if the
.IQ exit-jove
command is given a numeric argument,
point is left at its most recent position within that buffer.
.dc "expand-environment-variables" "(variable)"
When this variable is
.IQ on
\s-2JOVE\s0 will try to expand any strings of the form
\*Q$var\*U into the value of the environment variable \*Qvar\*U when asking for
a filename.  For example, if you type
.BQ $HOME/.joverc ,
\*Q$HOME\*U will be
replaced with your home directory.  The default value is
.IQ on .
.dc "file-creation-mode" "(variable)"
This variable has an octal value.  It contains the mode (see
.IQ chmod (1))
with which files should be created.  This mode gets
modified by your current umask setting (see
.IQ umask (1)).
The default value is usually
0666 or 0644.
.dc "files-should-end-with-newline" "(variable)"
This variable indicates that all files should always have a newline
at the end.  This is often necessary for line printers and the like.
When set, if \s-2JOVE\s0 is writing a file whose last character is not a
newline, it will add one automatically.
The default value is
.IQ on .
.dc "fill-comment" "Not Bound"
This command fills in your C comments to make them pretty and readable.
This filling is done according the variable
.IQ comment-format .
.DS
/\(**
 \(** the default format makes comments like this.
 \(**/
.DE
This can be changed by changing the
.IQ comment-format
variable.  Other languages
may be supported by changing the format variable appropriately.  The
formatter looks backwards from point for an open comment symbol.  If
found, all indentation is done relative to the position of the first character
of the open symbol.  If there is a matching close symbol, the entire
comment is formatted.  If not, the region between the open symbol and point
is reformatted.  The original text is saved in the kill ring; a
.IQ yank-pop
command will undo the formatting.
.dc "fill-paragraph" "ESC\ J"
This rearranges words between lines so that all the lines in the current
paragraph extend as close to the right margin as possible, ensuring that
none of the lines will be greater than the right margin.  The default value
for
.IQ right-margin
is 78, but can be changed with the
.IQ set
and
.IQ right-margin-here
commands.

The rearrangement may cause an end of line to be replaced by whitespace.
Normally, this whitespace is a single space character.
If the variable
.IQ space-sentence-2
is
.IQ on ,
and the end of the line was apparently the end of a sentence
or the line ended with a colon, two spaces will be used.
However, a sentence or colon followed by a single space already within
a line will not be altered.

\s-2JOVE\s0 has a complicated algorithm
for determining the beginning and end of the paragraph.  In the normal case
\s-2JOVE\s0 will give all the lines the same indent as they currently have,
but if you wish to force a new indent you can supply a numeric argument to
.IQ fill-paragraph
and \s-2JOVE\s0 will indent each line to the column
specified by the
.IQ left-margin
variable.  See also the
.IQ left-margin
variable and
.IQ left-margin-here
command.

Filling a paragraph can do something that you didn't intend.  For that
reason the original text is saved on the kill ring and can be yanked
back.  Deleting the rubble is up to you.
.dc "fill-region" "Not Bound"
This is like
.IQ fill-paragraph ,
except it operates on a region instead of
just a paragraph.
.dc "filter-region" "Not Bound"
This sends the text in the region to a
.UX
command, and replaces the
region with the output from that command.  For example, if you are
lazy and don't like to take the time to write properly indented C
code, you can put the region around your C file and
.IQ filter-region
it through
.IQ cb ,
the
.UX
C beautifier.  If you have a file that contains
a bunch of lines that need to be sorted you can do that from inside
\s-2JOVE\s0 too, by filtering the region through the
.UX
.IQ sort
command.
Before output from the command replaces the region \s-2JOVE\s0 stores the old
text in the kill ring.  If you are unhappy with the results a
.IQ yank-pop
command will get back the old text.
.dc "find-file" "^X\ ^F"
This reads a specified file into its own buffer and then selects that buffer.
If you've already read this file into a buffer, that buffer is simply
selected.  If the file doesn't yet exist, \s-2JOVE\s0 will print \*Q(New file)\*U
so that you know.
If possible, the buffer is named after the filename (ignoring any directory
part).
.dc "find-tag" "^X\ T"
This finds the file that contains the specified tag.  \s-2JOVE\s0 looks up
tags by default in the
.BQ tags
file in the current directory, as created
by the
.UX
command
.IQ ctags(1) .
You can change
the default tag name by setting the
.IQ tag-file
variable to another
name.  If you specify a numeric argument to this command, you will be
prompted for a tag file.  This is a good way to specify another tag file
without changing the default.
.dc "find-tag-at-point" "Not Bound"
This finds the file that contains the tag that point is currently in.
See
.IQ find-tag .
.dc "first-non-blank" "ESC\ M"
This moves point (backwards or forwards) to the indent of the current line.
.dc "forward-character" "^F"
This moves point forward over a single character or line-separator.
Thus if point is at the end of
the line it moves to the beginning of the next one.
.dc "forward-list" "ESC\ ^N"
This moves point forward over a list, which is any text between properly
matching (\^...\^), [\^...\^] or {\^...\^}.
It first searches forward for a \*Q(\*U and then moves to the matching \*Q)\*U.
This is useful when you are trying to find unmatched parentheses in a program.
Arguments are accepted, and negative arguments search backwards.
See also
.IQ forward-s-expression .
.dc "forward-paragraph" "ESC\ ]"
This moves point forward to the end of the current or next paragraph.
Paragraphs are bounded by lines that match
.IQ paragraph-delimiter-pattern
(by default, those that are empty or look like troff or TeX commands).
A change in indentation may also signal a break
between paragraphs, except that \s-2JOVE\s0 allows the first line of a
paragraph to be indented differently from the other lines.
Arguments are accepted, and negative arguments search backwards.
.dc "forward-s-expression" "ESC\ ^F"
This moves point forward over an s-expression, that is over a Lisp atom or
a C identifier (depending on the major mode) ignoring punctuation and
whitespace; or, if the nearest succeeding significant character is one of
\*Q([{\*U, over a list as in
.IQ forward-list .
Arguments are accepted, and negative arguments search backwards.
.dc "forward-sentence" "ESC\ E"
This moves point forward to the end of the current or next sentence.
\s-2JOVE\s0 considers the end of a sentence to be the characters \*Q.\*U, \*Q!\*U or
\*Q?\*U, followed possibly by \*Q'\*U, \*Q''\*U, or \*Q"\*U, followed by a
Return or whitespace.
Arguments are accepted, and negative arguments search backwards.
.dc "forward-word" "ESC\ F"
This moves point forward to the end of the current or next word.
.dc "fundamental-mode" "Not Bound"
This sets the major mode to Fundamental.
Fundamental mode is the mode of the Minibuf, and hence of anything typed in
the message line.
.dc "gather-numeric-argument" "^U"
This command is one of two ways to specify a numeric argument to a
command.  Typing this command once means, Do the next
command 4 times.  Typing it twice will do the next command 16 times, and
so on.  If at any point you type a number, then that number will be used
instead of 4.  For instance, ^U 3 5 means do the next command 35 times
(assuming
.IQ gather-numeric-argument
is bound to ^U).
.dc "goto-line" "ESC\ G"
If a positive numeric argument is supplied, point moves to the
beginning of that line.  If the argument is negative, it indicates
how many lines from the end of the buffer to move point to.  If no
argument is supplied one is prompted for.
.dc "goto-window-with-buffer" "Not Bound"
This command prompts for a buffer name and then selects that buffer.  If
the buffer is currently being displayed in one of the windows, that
window is selected instead.
.dc "grind-s-expr" "Not Bound"
When point is positioned on a \*Q(\*U, this re-indents that LISP expression.
.dc "grow-window" "^X\ ^\^"
This makes the active window one line bigger.  This only works when
there is more than one window and provided there is room to change the
size.
See also
.IQ shrink-window .
.dc "handle-tab" "Tab"
This handles indenting to the \*Qright\*U place in C and Lisp mode, and
just inserts itself in Text mode.
.dc "highlight-attribute" "(variable)"
(IBM PC version only) This specifies how the attribute (color) of a
character is to be changed when it is highlighted.  Highlighting is indicated
by exclusive oring this value with the normal attribute for the character.
The default is 16.
.dc "highlight-mark" "(variable)"
When this is on, jove will highlight the mark if currently
visible.  The mark is highlighted with an underscore.
.dc "i-search-forward" "Not Bound"
Incremental search.  Like search-forward except that instead of prompting
for a string and searching for that string all at once, it accepts the string
one character at a time.  After each character you type as part of the search
string, it searches for the entire string so far.  When you like what it
found, type Return to finish the search.  You can take back a
character with DEL and the search will back up to the position before
that character was typed.  ^G aborts the search.
.dc "i-search-reverse" "Not Bound"
Incremental search.  Like search-reverse except that instead of prompting
for a string and searching for that string all at once, it accepts the string
one character at a time.  After each character you type as part of the search
string, it searches for the entire string so far.  When you like what it
found, type Return to finish the search.  You can take back a
character with DEL and the search will back up to the position before
that character was typed.  ^G aborts the search.
.dc "i-shell-command" "Not Bound"
This is like
.IQ shell-command
except that it launches an interactive process and so lets you continue with
your editing while the command is running.  This is really useful for long
running commands with sporadic output.
See also the variable
.IQ wrap-process-lines .
.dc "insert-file" "^X\ ^I"
This inserts a specified file into the selected buffer at point.  Point
is positioned at the beginning of the inserted file.
.dc "interrupt-character" "(variable)"
This specifies what character should be used as the operating
system's tty driver interrupt character.  When this character is
typed, the tty driver generates SIGINT signal.  This will
interrupt a non-interactive process.  If no such process is
running, \s-2JOVE'S\s0 will offer you the option of continuing, or
crashing \s-2JOVE'S\s0 (trying to save your work).  This is a crude and
desperate way to stop \s-2JOVE'S\s0.  Unfortunately there is no way to
turn off the interrupt character.  The default is ^].  See also
.IQ abort-char .
.dc "insert-variable" "Not Bound"
This inserts the value of the specified variable
at the current point in the current buffer.
.dc "interrupt-process" "Proc: ^C\ ^C"
This sends the signal SIGINT to the interactive process
in the selected buffer.  This only works if you are inside a buffer
bound to an interactive process.
.dc "iproc-env-export" "Not Bound"
This takes an argument of the form VARNAME=VALUE and replaces or
sets VARNAME to that VALUE in the list of environment variables that
are passed to interactive shell commands i.e.
.IQ shell
and
.IQ i-shell-command .
.dc "iproc-env-show" "Not Bound"
This shows (as temporary screen output or in a buffer, depending on the variable
.IQ send-typeout-to-buffer )
the environment that will be exported to interactive processes.
.dc "iproc-env-unset" "Not Bound"
This takes an argument that is the name of an environment variable and
removes it from the list of environment variables that will be passed
to interactive processes.
.dc "jove-compiled-with" "(variable)"
This is a special, read-only variable, containing the compiler, flags and
options used to build Jove (on Unix-style machines)
.dc "jove-features" "(variable)"
This is a special, read-only variable, set with a colon-delimited list of 
all the optional capabilities compiled into \s-2JOVE\s0.  The value will
be something like
.br
.IQ :unix:abbr:bak:biff:cmtfmt:fcomp:iproc:pty:lisp:proc:spell:rec:job:idchar:hl:tcap:ctype: 
.br
.IQ unix
specifies a variety of traditional UNIX/POSIX capabilities (case-sensitive filenames,
a Unix-style filesystem), as opposed to 
.br
.IQ msdos
or
.IQ win32
which have case-insensitive filesystems, for example.  The platform
.IQ ibmpcdos
indicates specialization for the IBM-PC platform, which in reality, is
the only MS-DOS form remaining in existence (other non-IBM-PC platforms,
like Heath/Zenith, DEC Rainbow, etc probably do not exist anymore,
as is probably the case for the pre-OSX classic
.IQ mac
platform).  Other tokens are
.br
.IQ abbr 
(abbrev-mode exists), 
.br
.IQ bak 
(backup-files can be created),
.br
.IQ biff 
(ability to disable screen-disruptions from old Unix-style mail notification),
.br
.IQ cmtfmt
(format C comments),
.br
.IQ fcomp
(filename completion),
.br
.IQ iproc
(interactive shell processes),
.br
.IQ pipe
(indicating that 
.IQ iproc
is implemented using pipes and the 
.IQ portsrv
helper program ), or
.br
.IQ pty
(indicating that 
.IQ iproc
is implemented using pseudo-terminals, available on any modern
post-4.2BSD platform),
.br
.IQ lisp
(lisp mode is available),
.br
.IQ proc
(the ability to run non-interactive subshell processes with output 
to buffers, or switch entirely from \s-2JOVE\s0 to a child shell
via 
.IQ push-shell
and then return to \s-2JOVE\s0 when that child shell exits),
.br
.IQ spell
(the ability to run a spell checker on the contents of a buffer,
parse and modify the checker output and then step through
any misspelled words in the original buffer with ^X^N and ^X^P)
.br
.IQ rec
(periodically generate a recovery file so that one can restore unsaved changes
from \s-2JOVE\s0 editors after a machine or program crash)
.br
.IQ job
(BSD job control is supported, so one can suspend \s-2JOVE\s0
with the pause-jove command and return to the shell that \s-2JOVE\s0
was started from, and later resume \s-2JOVE\s0 using a
shell command like 
.IQ fg ),
.br
.IQ jsmall
(\s-2JOVE\s0 was built with smaller limits on the number of
lines it can support, probably only 32000)
.br
.IQ idchar
(\s-2JOVE\s0 will attempt to optimize screen display
using insert/delete character modes and terminal control sequences),
.IQ hl
(enable highlight-mark capabiltiy to display a small underscore for the position of the mark,
and enable the scroll-bar capability in the buffer mode line),
.br
.IQ jtc
(uses builtin ANSI/VT1xx/XTERM driver rather than 
.IQ termcap/terminfo/curses ),
.br
.IQ tcap
(termcap database code is enabled)
.br
.IQ tinfo
(uses SystemV-style terminfo rather than BSD-style termcap calls),
.br
.IQ ctype
(uses the system ctype character classification rather than \s-2JOVE\s0 builtin
capability)
.br
.IQ iso88591
(uses builtin \s-2JOVE\s0 tables for ISO-8859-1 character classification
rather than the system ctype capability).
.dc "jove-linked-with" "(variable)"
This is a special, read-only variable, containing the linker, flags and
libraries used to build Jove (on Unix-style machines)
.dc "kill-next-word" "ESC\ D"
This kills the text from point to the end of the current or next word.
.IQ 
The killed text is sent to the kill ring.
.dc "kill-previous-word" "ESC\ DEL"
This kills the text from point to the beginning of the current or
previous word.
The killed text is sent to the kill ring.
.dc "kill-process" "Not Bound"
This command prompts for a buffer name or buffer number (just as
.IQ select-buffer
does) and then sends the process in that buffer the
signal SIGKILL.
.dc "kill-region" "^W"
This deletes the text in the region and saves it on the kill ring.
Commands that delete text but save it on the kill ring all have the
word \*Qkill\*U in their names.  Use the
.IQ yank
command to get back the most recent
kill.
.dc "kill-s-expression" "ESC\ ^K"
This kills the text from point to the end of the current or next
s-expression.
The killed text is sent to the kill ring.
.dc "kill-some-buffers" "Not Bound"
This goes through all the existing buffers and asks whether or not to delete
each one.  If you decide to delete a buffer, and it turns out that the buffer is
modified, \s-2JOVE\s0 will offer to save it first.  This is useful for when \s-2JOVE\s0
runs out of memory to store lines (this only happens on PDP-11's) and you
have lots of buffers that you are no longer using.
See also the
.IQ delete-buffer
command.
.dc "kill-to-beginning-of-sentence" "^X\ DEL"
This kills from point to the beginning of the current or previous
sentence.  If a
negative numeric argument is supplied it kills from point to the
end of the current or next sentence.
The killed text is sent to the kill ring.
.dc "kill-to-end-of-line" "^K"
This kills from point to the end of the current line.  When point is
at the end of the line (discounting any white space) the line-separator
is also deleted and the next line
is joined with current one.  If a numeric argument is supplied that
many lines are killed; if the argument is negative that many lines
before point are killed; if the argument is zero the text from point
to the beginning of the line is killed.
The killed text is sent to the kill ring.
.dc "kill-to-end-of-sentence" "ESC\ K"
This kills from point to the end of the current or next sentence.  If a
negative numeric argument is supplied it kills from point to the
beginning of the current or previous sentence.
The killed text is sent to the kill ring.
.dc "lc-ctype" "(variable)"
This string variable determines how non-ASCII characters are displayed, and
which characters are to be considered as upper-case, lower-case, printable,
etc.  The default is the implementation-defined native environment; under
POSIX, it is determined by whichever of the environment variables
LC_ALL, LC_CTYPE or LANG is first found to be set, and is otherwise "C".
Some useful values of
.IQ lc-ctype
might be:
.DS
.ta \w'"iso_8859_111" 'u
""	Default: the native environment.
"C"	Strict ASCII.  All other characters greater than \e\^177
	rendered in octal.
"iso_8859_1"	Latin-1 alphabet.
.DE
.dc "left-margin" "(variable)"
This is how far lines should be indented when Auto Indent mode is on,
or when the
.IQ newline-and-indent
command is run (usually by typing
Linefeed).  It is also used by
.IQ fill-paragraph
and Auto Fill mode.
If the value is zero (the default) then the left margin is determined
from the surrounding lines.
.dc "left-margin-here" "Not Bound"
This sets the
.IQ left-margin
variable to the current position of
point.  This is an easy way to say, \*QMake the left margin begin here,\*U
without having to count the number of spaces over it actually is.
.dc "lib-dir-pathname" "(variable)"
This tells JOVE where to find the machine-dependent helper
programs: the
.IQ recover
program, run when 
.IQ "jove -r"
is run after a crash to try to recover unsaved files,
and on some older machines without pseudo-terminal support, the 
.IQ portsrv
program, which handles multiplexed communication with the
keyboard and interactive shells (if JOVE is compiled with SUBSHELL
and PIPEPROCS configured)
.dc "lisp-mode" "Not Bound"
This turns on the Lisp major mode.  In Lisp mode, the characters Tab
and \*Q)\*U are treated specially, similar to the way they are treated in C mode.
Also, Auto Indent mode is affected, and handled specially.
See also the
.IQ c-mode
command.
.dc "list-buffers" "^X\ ^B"
This types out a list containing various information about each buffer.
The list looks like this:
.DS
.ta \w'NO111'u +\w'Lines1'u +\w'Scratch111'u +\w'\(**1'u +\w'commands.doc111'u
\ (\(** means the buffer needs saving)
\ NO	Lines	Type		Name	File
\ --	-----	----		----	----
\ 1	1	File		Main	[No file]
\ 2	1	Scratch	\(**	Minibuf	[No file]
\ 3	519	File	\(**	commands.doc	commands.doc
.DE
The first column lists the buffer's number.  When \s-2JOVE\s0 prompts for a
buffer name you can either type in the full name, or you can simply
type the buffer's number.  The second column is the number of lines in
the buffer.  The third says what type of buffer.  There are four
types: File, Scratch, Process and I-Process.  \*QFile\*U is simply a
buffer that holds a file; \*QScratch\*U is for buffers that \s-2JOVE\s0 uses
internally; \*QProcess\*U is one that holds the output from a
.UX
command; \*QI-Process\*U is one that has an interactive process attached to
it.  The next column contains the name of the buffer.  And the last
column is the name of the file that's attached to the buffer.  In this
case, both Minibuf and commands.doc have been changed but not yet
saved.  In fact Minibuf won't be saved since it's a Scratch buffer.
.dc "list-processes" "Not Bound"
This makes a list somewhat like \*Qlist-buffers\*U does, except its
list consists of the current interactive processes.  The list
looks like this:
.DS
.ta \w'shell-111111111111'u +\w'Running1111111111'u +\w'1851211111'u
\ Buffer	Status	Pid	Command
\ ------	------	---	-------
\ \(**shell\(**	Running	18415	shell
\ fgrep	Done	18512	fgrep -n Buffer \(**.c
.DE
The first column has the name of the buffer to which the process is
attached.  The second has the status of the process; if a process has
exited normally the status is \*QDone\*U as in fgrep; if the process
exited with an error the status is \*QExit N\*U where N is the value of
the exit code; if the process was killed by some signal the status is
the name of the signal that was used; otherwise the process is
running.  The last column is the name of the command that is being run.
.dc "local-bind-keymap-to-key" "Not Bound"
This is like
.IQ local-bind-to-key
except that you use it to attach a key sequence to a named keymap.
The only reasonable use is to bind some extra key to
.IQ ESC-map
for keyboards that make typing ESC painful.
.dc "local-bind-macro-to-key" "Not Bound"
This is like
.IQ local-bind-to-key
except you use it to attach a key sequence to a named macro.
.dc "local-bind-to-key" "Not Bound"
This is like
.IQ bind-to-key ,
except that the binding is only enabled when the selected buffer is
the buffer that was current when the command was executed.  In other
words, the binding only applies to the selected buffer.
.dc "macify" "(variable)"
(Mac version only)
When this variable is on, \s-2JOVE\s0 will use the standard Macintosh file-selector
dialog in place of the traditional \s-2JOVE\s0 Minibuffer.
.dc "mail-check-frequency" "(variable)"
This is how often (in seconds) \s-2JOVE\s0 should check your mailbox for
incoming mail.  If you set this to zero \s-2JOVE\s0 won't check for new
mail.  See also the
.IQ mode-line ,
.IQ mailbox
and
.IQ disable-biff
variables.
The default is 60.
.dc "mailbox" "(variable)"
Set this to the full pathname of your mailbox.  \s-2JOVE\s0 will look here to
decide whether or not you have any unread mail.  This defaults to
.BQ /usr/spool/mail/$USER ,
where \*Q$USER\*U is set to your login name.
.dc "make-backup-files" "(variable)"
If this variable is set, then whenever \s-2JOVE\s0 writes out a file, it will
move the previous version of the file (if there was one) to \*Q#filename~\*U.
This is often convenient if you save a file by accident.  The default
value of this variable is
.IQ off .
.dc "make-buffer-unmodified" "ESC\ ~\^"
This makes \s-2JOVE\s0 think the selected buffer hasn't been changed even if
it has.  Use this when you accidentally change the buffer but don't
want it considered changed.  Watch the mode line to see the \(** disappear
when you use this command.
.dc "make-macro-interactive" "ESC\ I"
This command is meaningful only while you are defining a keyboard macro,
and when you are expecting input in the message line.
Ordinarily, when a command in a macro
definition requires a trailing text argument (file name, search string,
etc.), the argument you supply becomes part of the macro definition.  If
you want to be able to supply a different argument each time the macro is
used, then while you are defining it, you should give the
.IQ make-macro-interactive
command just before typing the argument which will
be used during the definition process.  Note: you must bind this command
to a key in order to use it; you can't say \*QESC\ X make-macro-interactive\*U.
.dc "mark-threshold" "(variable)"
This variable contains the number of lines point may move by before
the mark is set.  If, in a search or some other command that may move point,
point moves by more
than this many lines, the mark is set so that you may return easily.
The default value of this variable is 22 (one screenful, on most
terminals).
See also the commands
.IQ search-forward ,
.IQ search-reverse ,
.IQ beginning-of-file
and
.IQ end-of-file .
.dc "match-regular-expressions" "(variable)"
When set, \s-2JOVE\s0 will match regular expressions in search patterns.
This makes special the characters ., \(**, [ and ].
See the \s-2JOVE\s0 Manual for a full discussion of regular-expressions.
.dc "meta-key" "(variable)"
You should set this variable to
.IQ on
if your terminal has a real Meta key
which forces the 8th bit of each character.
If your terminal has such a key, then a key sequence like ESC\ Y can be
entered by holding down Meta and typing Y.
On the IBM PC, this variable affects how ALT is interpreted.
On the Macintosh, it affects how Option is interpreted.
NOTE: In some older
.UX
systems, \s-2JOVE\s0 must switch the tty to
raw mode to accept the 8-bit characters generated by a meta key.
Unfortunately, the
.IQ interrupt-character
does not generate an interrupt in raw mode.
.dc "mode-line" "(variable)"
The format of the mode line can be determined by setting this variable.
The items in the line are specified using a format similar to that used by
.IQ printf(3) ,
with the special things being marked as \*Q%x\*U.  Digits may be used
between the '%' and the 'x' to mean repeat that many times.
\&'x' may be:
.DS I
.ta .5i 1i 1.5i
	C	checks for new mail, and displays \*Q[New mail]\*U if there is any
		(see also the \c
.IQ mail-check-frequency " \c"
and \c
.IQ mailbox " \c"
variables)
	F	the current file name, with leading path stripped
	M	the current list of major and minor modes
	b	the selected buffer name
	c	the fill character (-)
	d	the current directory
	e	extra space in mode line is distributed evenly
		among the places %e is used (used for justifying,
		separating, or centering parts of the mode line)
	f	the current file name
	ixy	x, when the buffer's file has been changed behind JOVE's back,
		y, when not
	mxy	x, when the buffer is modified or y, when not
	n	the selected buffer number
	p	interactive process status for process windows
	s	space, but only if previous character is not a space
	t	the current time (updated automatically)
	w	a '>' for windows which are scrolled left
	[ ]	the square brackets printed when in a recursive edit
	( )	items enclosed in %( ... %) will only be printed on
		the bottom mode line, rather than copied when the
		window is split
.DE
In addition, any other character is simply copied into the mode line.
Characters may be escaped with a backslash.  To get a feel for all
this, try typing \*QESC\ X print mode-line\*U and compare the result with
your current mode line.
.dc "mode-line-attribute" "(variable)"
(IBM PC version only) This specifies the screen attribute (color) for
characters in the mode line.  The default is 112 (black on white).
.dc "mode-line-should-standout" "(variable)"
If set, the mode line will be printed in reverse video, if your
terminal supports it.  The default for this variable is
.IQ on .
.dc "name-kbd-macro" "Not Bound"
This copies the keyboard macro and gives it a name freeing up the
keyboard macro so you can define some more.  Keyboard macros with
their own names can be bound to keys just like built in commands
can.  See the
.IQ define-macro ,
.IQ source
and
.IQ write-macros-to-file
commands.
.dc "newline" "Return"
This divides the current line at point moving all the text to the
right of point down onto the newly created line.  Point moves down to
the beginning of the new line.  In Auto Indent mode, the new line will
be indented to match the old line.
.dc "newline-and-backup" "^O"
This divides the current line at point moving all the text to the
right of point down onto the newly created line.  The difference
between this and
.IQ newline
is that point does not move down to the beginning of the new line.
.dc "newline-and-indent" "Linefeed"
This behaves in any mode the same way as
.IQ newline
does in Auto Indent mode.
.dc "next-error" "^X\ ^N"
This moves to the next error in the list of errors that were parsed
with
.IQ parse-errors .
In one window the list of errors is shown with the current one always at
the top.  If the file that contains the error is not already in a buffer, it
is read in.  Its buffer is displayed in another window and point
is positioned in this window on the line where the error occurred.
.dc "next-line" "^N"
This moves point down to the corresponding position on the next line (or the
end of that line if it does not extend so far).
.dc "next-page" "^V"
This displays the next page of the selected buffer by taking the bottom line of
the window and redrawing the window with it at the top.  If there isn't
another page in the buffer \s-2JOVE\s0 rings the bell.  If a numeric argument
of only \- (with no digits) is supplied, the previous page
is displayed.  Otherwise, if a numeric argument
is supplied the screen is scrolled up that many lines, exactly as in the
.IQ scroll-up
command; if the argument
is negative the screen is scrolled down.
.dc "next-window" "^X\ N"
This moves into the next window.  Windows live in a circular list so
when you're in the bottom window and you try to move to the next one
you are moved to the top window.  It is an error to use this command
with only one window.
.dc "number-lines-in-window" "Not Bound"
This displays the line numbers for each line in the buffer being
displayed.  The number isn't actually part of the text; it's just
printed before the actual buffer line is.  To turn this off you run
the command again; it toggles.
.dc "one-key-confirmation" "(variable)"
If this variable is set, a single keystroke of y or n is expected
in answer to yes/no questions.
Normally, a yes/no question must be answered with any non-empty
prefix of yes or no, followed by a Return
.dc "over-write-mode" "Not Bound"
This turns Over Write minor mode on in the selected buffer.  Without
a numeric argument, the command toggles the mode; with a zero
argument, the mode is turned off; with a non-zero argument, the
mode is turned on.  When on, this mode changes the way the
self-inserting characters work.  Instead of inserting themselves and
pushing the rest of the line over to the right, they replace or
over-write the existing character.  Also, DEL replaces the character
before point with a space instead of deleting it.  When Over Write
mode is on \*QOvrWt\*U is displayed in the mode line.
.dc "page-next-window" "ESC\ ^V"
This displays the next page in the next window.
It switches to the next window, performs a
.IQ next-page
command (with any numeric argument), and switches back to the original window.
Note that an argument of just \*Q\-\*U will thus display the previous page.
.dc "paren-flash" "\^) } ]\^"
This command causes the characters bound to it to be inserted, and then to
partake in C mode curly brace indentation, Lisp mode parenthesis
indentation, and the Show Match mode paren/curly-brace/square-bracket
flashing.
.dc "paragraph-delimiter-pattern" "(variable)"
When \s-2JOVE\s0 is searching for a paragraph boundary, if this
pattern (a regular expression) matches the start of a line, that
line is treated as a paragraph delimiter.  The default pattern
recognizes blank lines, troff control lines, and lines starting with
a TeX control sequence.

There is a special provision for TeX: if a line is matched by the
pattern, and the match is of exactly an initial \e, that line is
only treated as a delimiter if the next line also starts with \e.
.dc "paren-flash-delay" "(variable)"
How long, in tenths of a second, \s-2JOVE\s0 should pause on a matching
parenthesis in Show Match mode.  The default is 5.
.dc "parse-errors" "Not Bound"
This takes the list of C compilation errors (or the output from another
program in an acceptable format) in the selected buffer and parses them for
use with the
.IQ next-error ,
.IQ previous-error
and
.IQ current-error
commands.
This is a very useful tool and helps with compiling C programs or, when used
in conjunction with the
.UX
.IQ grep
command, with making changes to a bunch of files.
\s-2JOVE\s0 finds each file that has an error and remembers each line that
contains an error.  It doesn't matter if later you insert or delete
some lines in the buffers containing errors; \s-2JOVE\s0 remembers where
they are regardless.
.IQ current-error
is automatically executed after one of the parse commands, so you end up
at the first error.  The variable
.IQ error-format-string
specifies, by means of regular-expressions, the format of errors to be
recognized.  Its default value can handle messages from
.IQ cc ,
.IQ cpp ,
.IQ lint
and
.IQ "grep\ \-n" .
.dc "parse-spelling-errors-in-buffer" "Not Bound"
This parses a list of words in the selected buffer and looks them up in
another buffer that you specify.
It is invoked automatically by the
.IQ spell-buffer
command.
.dc "pause-jove" "ESC\ S"
This stops \s-2JOVE\s0 and returns control to the parent shell.  This
only works on systems that have the
job control facility.  To return to \s-2JOVE\s0 you type \*Qfg\*U to the shell.
.dc "pop-mark" "Not Bound"
\s-2JOVE\s0 remembers the last eight marks and you use
.IQ pop-mark
to go
backward through the ring of marks.  If you execute
.IQ pop-mark
enough
times you will eventually get back to where you started.
This command is also executed when you run
.IQ set-mark
with a numeric argument.
.dc "popd" "Not Bound"
This pops one entry off the directory stack.  Entries are pushed with
the
.IQ pushd
or
.IQ pushlibd
commands.  The names were stolen from the C-shell and the
behavior is the same.
.dc "previous-error" "^X\ ^P"
This is the same as
.IQ next-error
except it goes to the previous error.
See
.IQ next-error
for documentation.
.dc "previous-line" "^P"
This moves point up to the corresponding position on the previous line (or the
end of that line if it does not extend so far).
.dc "previous-page" "ESC\ V"
This displays the previous page of the selected buffer by taking the top
line and redrawing the window with it at the bottom.  If a numeric
argument of only \- (with no digits) is supplied, the next page
is displayed.  Otherwise, if a numeric
argument is supplied the screen is scrolled down that many lines, exactly as
in the
.IQ scroll-down
command; if
the argument is negative the screen is scrolled up.
.dc "previous-window" "^X\ P or ^X\ O"
This moves into the previous window.  Windows live in a circular list so
when you're in the top window and you try to move to the previous one
you are moved to the bottom window.  It is an error to use this command
with only one window.
.dc "print" "Not Bound"
This displays the value of a \s-2JOVE\s0 variable in the message line.
.dc "proc-env-export" "Not Bound"
This takes an argument of the form VARNAME=VALUE and replaces or
sets VARNAME to that VALUE in the list of environment variables that
are passed to subshell commands i.e.
.IQ shell-command ,
.IQ compile-it ,
.IQ spell-buffer ,
etc.
.dc "proc-env-show" "Not Bound"
This shows (as temporary screen output or in a buffer, depending on the variable
.IQ send-typeout-to-buffer )
the environment that will be exported to subshell commands.
.dc "proc-env-unset" "Not Bound"
This takes an argument that is the name of an environment variable and
removes it from the list of environment variables that will be passed
to subshell commands.
.dc "process-bind-keymap-to-key" "Not Bound"
This is like
.IQ process-bind-to-key
except that you use it to attach a key sequence to named keymap.
The only reasonable use is to bind some extra key to
.IQ ESC-map
for keyboards that make typing ESC\ painful.
.dc "process-bind-macro-to-key" "Not Bound"
This is like
.IQ process-bind-to-key
except you use it to attach a key sequence to a named macro.
.dc "process-bind-to-key" "Not Bound"
This command is identical to
.IQ bind-to-key ,
except that it only affects
your bindings when you are in a buffer attached to an interactive process.
When you enter the process buffer, any keys bound with this command will
automatically take their new values.  When you switch to a non-process
buffer, the old bindings for those keys will be restored.  For example,
you might want to execute
.DS I
process-bind-to-key stop-process ^C\ ^Z
process-bind-to-key interrupt-process ^C\ ^C
.DE
Then, when you start up an interactive process and switch into that
buffer, ^C\ ^Z will execute
.IQ stop-process
and ^C\ ^C will execute
.IQ interrupt-process .
Bindings effective only in process windows are shown with a \*QProc:\*U
prefix in this manual and by the
.IQ apropos
and
.IQ describe-bindings
commands.
.dc "process-newline" "Proc: Return"
This command is normally bound to Return as if by a
.IQ process-bind-to-key
so that it will only be bound in a process window.
\s-2JOVE\s0 does two different things depending on where you are when you
hit Return.  When you're in the last line of the interactive process buffer,
point moves to the end of the line, the line is terminated, and the line is
made available as input to the process.  When point is positioned in some
other line, that line is copied to the end of the buffer (with the prompt
stripped) and point is moved there with it, so you can then edit that line
before sending it to the process.  This command must be bound to the key you
usually use to enter shell commands (Return), or else you won't be able to
enter any.  See the variable
.IQ process-prompt .
.dc "process-prompt" (variable)
What a prompt looks like from the
.IQ shell
and
.IQ i-shell-command
processes.  The default is \*Q% \*U, the default C-shell prompt.  This is
actually a regular expression search string.  So you can set it to be
more than one thing at once using the \e| operator.  For instance, for
LISP hackers, the prompt can be
.DS
"% \e|-> \e|<[0-9]>: ".
.DE
.dc "process-send-data-no-return" "Not Bound"
This is like
.IQ process-newline
except it sends everything to the process without the newline.  Normally,
when you type return in a process buffer it sends everything you typed
including the Return.  This command just provides a way to send data to
the process without having to send a newline as well.
.dc "push-shell" "Not Bound"
This spawns a child shell and relinquishes control to it.
Within this shell, $1 can be used to refer to the filename (if any)
of the selected buffer.
This works on any version of
.UX ,
but this isn't as good as
.IQ pause-jove
because
it takes time to start up the new shell and you get a brand new
environment every time.  To return to \s-2JOVE\s0, simply exit the shell.
.dc "pushd" "Not Bound"
This pushes a directory onto the directory stack and cd's into it.  It
asks for the directory name but if you don't specify one it switches
the top two entries on the stack.  It purposely behaves the same as
C-shell's
.IQ pushd .
.dc "pushlibd" "Not Bound"
Performs same function as
.IQ pushd
except that it pushes the Jove
sharable library directory.  This directory holds the system-wide
.BQ jove.rc
and the text used by the
.IQ describe-command
and
.IQ describe-variable
commands.  It is mainly intended for use with the
.BQ jove.rc
file.
.dc "pwd" "Not Bound"
This prints the pathname of the working directory, as in the
.UX
.IQ pwd
command.
.dc "query-replace-string" "ESC\ Q"
This replaces strings matching a specified regular-expression
with a specified replacement string.
When a match is found, point is moved to it
and then \s-2JOVE\s0 asks what to do.  The options are:
.DS I
.ta \w'DEL, BS, or N or n11'u
Space or Y or y	to replace this match and go on to the next one.
Period	to replace this match and then stop.
DEL, BS, or N or n	to skip this match and go on to the next one.
^R or R or r	to enter a recursive edit.  This lets you temporarily
	suspend the replace, do some editing, and then return
	to continue where you left off.  To continue with the
	\c
.IQ query-replace-string ,\c
 use the \c
.IQ exit-jove " command."
^W	to delete the match and then enter a recursive edit.
^U or U or u	to undo all changes to the last modified line and
	continue the search from the start of that line.
! or P or p	to go ahead and replace the remaining matches without
	asking, as in \c
.IQ replace-string .
Return or Q or q	to stop the \c
.IQ query-replace-string .
^L	to redraw the screen
.DE
It is often useful to include a piece of the matched string in the
replacement, especially if the piece was not matched by literal text.
To select which part of the matched string is to be used, the
corresponding part of the pattern is bracketed with \e\^( and \e).
More than one set of brackets may be used, as long as they are
properly nested.  The matching substring is selected in the
replacement string using \e followed by a digit: \e1 for the first,
\e\^2 for the second, and so on.  Conveniently, \e\^0 always stands for
the complete matched string, as if the whole regular expression were
bracketed.  For example, the following command will reverse pairs of
comma-separated numbers:
.ID
: query-replace-string \e\^([0-9]\(**\^\e),\e\^([0-9]\(**\^\e) with \e\^2,\e1
.DE
The search for a match starts at point and goes to the end of the
buffer, so to replace in the entire buffer you must first go to the
beginning.  Each subsequent search starts at the position after the
previous match; if the previous match was an empty string, the search
is first advanced one character to prevent unbounded repetition.
.dc "quit-process" "Proc: ^C\ ^\^\e\^"
Send the signal SIGQUIT to the interactive process in the selected buffer.
This is equivalent to sending the \*Qquit\*U character (which most people
have bound to ^\^\e) to the process.  This only works if you are in a buffer
bound to an interactive process.
.dc "quoted-insert" "^Q or ^^\^"
This lets you insert characters that normally would be executed as
other \s-2JOVE\s0 commands.  For example,
to insert \*Q^F\*U you type \*Q^Q ^F\*U
(assuming
.IQ quoted-insert
is bound to ^Q).
NUL cannot be represented in the buffer, so
.IQ quoted-insert
will insert \*Q^@\*U in its stead.
On the IBM PC under DOS, non-ASCII keystrokes are seen by \s-2JOVE\s0
as a hex FF character followed by another character;
.IQ quoted-insert
will quote both characters.
.dc "read-only-mode" "Not Bound"
This turns on or off the Read-only minor mode.  Without a numeric
argument, the command toggles the mode; with a zero argument, the
mode is turned off; with a non-zero argument, the mode is turned
on.  When a buffer is in Read-only mode, any attempt to modify the
buffer will fail.  When a file is found, and it's not got write
permission, \s-2JOVE\s0 automatically puts the buffer in read-only mode.
This is very helpful when you are in environments which use source
control programs like RCS and SCCS.  It prevents accidents like
making a bunch of changes and only THEN discovering that you haven't
checked the file out for making changes.
.dc "read-word-abbrev-file" "Not Bound"
This reads a specified file that contains a bunch of abbreviation
definitions, and makes those abbreviations available.
See the
.IQ word-abbrev-mode
command.
.dc "recursive-edit" "Not Bound"
This enters a recursive editing level.  This isn't really very
useful.  I don't know why it's available for public use.  I think I'll
delete it some day.
.dc "redraw-display" "^L"
This vertically centers the line containing point within the window.  If that
line is already in place, the screen is first cleared and then redrawn.
If a numeric argument is supplied, the line is positioned at that
offset from the top of the window.  For example, \*QESC\ 0 ^L\*U positions
the line containing point at the top of the window
(assuming
.IQ redraw-display
is bound to ^L).
.dc "rename-buffer" "Not Bound"
This lets you rename the selected buffer.
.dc "replace-in-region" "Not Bound"
This is the same as
.IQ replace-string
except that it is restricted
to occurrences between point and the mark.
.dc "replace-string" "ESC\ R"
This replaces all occurrences of a specified string with a specified
replacement string.  This is just like
.IQ query-replace-string
except that
it replaces without asking.
.dc "right-margin" "(variable)"
Where the right margin is for Auto Fill mode and the
.IQ fill-paragraph
and
.IQ fill-region
commands.  The default is 78.
.dc "right-margin-here" "Not Bound"
This sets the
.IQ right-margin
variable to the current position of
point.  This is an easy way to say, \*QMake the right margin begin here,\*U
without having to count the number of spaces over it actually is.
.dc "save-file" "^X\ ^S or ^X\ S or ^X\ \e\^"
This saves the selected buffer to the associated file.  This makes your
changes permanent so you should be sure you really want to do it.  If the
buffer has not been modified
.IQ save-file
refuses to do the save.  If
you really do want to write the file you must use
.IQ write-file .
.dc "save-on-exit" "(variable)"
If this is
.IQ on 
when \s-2JOVE\s0 is about to exit, it will ask, for each modified buffer,
whether you wish it to be saved.
See
.IQ write-modified-files .
.dc "scroll-all-lines" "(variable)"
When this is
.IQ off ,
(the default) horizontal scrolling will only affect the line
containing point.
When it is
.IQ on ,
horizontal scrolling will affect the whole window.  See also the
.IQ scroll-width
variable.
.dc "scroll-bar" "(variable)"
When this is turned
.IQ on ,
a section of the mode line at the foot of each window is
left in not-reverse-video, to show the position of the window relative to
the whole of the file represented by that buffer (however, if the whole of
the buffer is within the window, the whole mode line remains inverted).
.dc "scroll-down" "ESC\ Z"
This scrolls the screen one line down.  If the line containing point
moves past the bottom of the window, point is moved up to the top of
the window.  If a numeric argument is supplied that many lines are
scrolled; if the argument is negative the screen is scrolled up
instead.
See the
.IQ previous-page
command.
.dc "scroll-left" "Not Bound"
This scrolls the text in the active window to the
left.  If a numeric argument is specified then the text is scrolled that
number of columns.  Otherwise, the text is scrolled by
the number of columns specified by the variable
.IQ scroll-width .
If the variable
.IQ scroll-all-lines
is ON then
.IQ scroll-left
may actually do nothing if the scrolling would cause point not to be
visible.  A negative argument scrolls right.  If the
.IQ mode-line
variable
is suitably set, an indication that the text is scrolled will be given in
the mode line.
.dc "scroll-right" "Not Bound"
This scrolls the text in the active window to the
right.  If a numeric argument is specified then the text is scrolled that
number of columns.  Otherwise, the text is scrolled by
the number of columns specified by the variable
.IQ scroll-width .
If the variable
.IQ scroll-all-lines
is ON then
.IQ scroll-right
may actually do nothing if the scrolling would cause point not to be
visible.  A negative argument scrolls left.
.dc "scroll-step" "(variable)"
How many lines should be scrolled if the
.IQ previous-line
or
.IQ next-line
commands move you off the top or bottom of the screen.  You may wish to
decrease this variable if you are on a slow terminal.  The default value
is 0, which means to center the current line in the window.  If the value
is negative, the behavior is slightly different.  If you move off the top
of the window, and
.IQ scroll-step
is, say, \-5 then the new line will be displayed 5 lines from the bottom
of the window.  If you move off the bottom of the window, the new line
will be positioned 5 lines from the top of the window.
.dc "scroll-up" "^Z"
This scrolls the screen one line up.  If the line containing point
moves past the top of the window, point is moved down to the top of
the window.  If a numeric argument is supplied that many lines are
scrolled; if the argument is negative the screen is scrolled down
instead.
See also the
.IQ next-page
command.
.dc "scroll-width" "(variable)"
Just as a buffer may be too long to be completely displayed in a
window, a line may be too wide.  \s-2JOVE\s0 handles wide lines
through horizontal scrolling, displaying only a portion of the
line.  This variable affects horizontal scrolling.  If point is
outside the displayed portion of its line, but is within the
specified number of columns beyond either side, the line is scrolled
that much.  Otherwise, the line will be scrolled to center point.
The default value is 10.  If the variable is 0, centering will
always be used.  See also the
.IQ scroll-all-lines
variable.
.dc "search-exit-char" "(variable)"
Set this to the character you want to use to exit incremental search.  The
default is Newline, which makes
.IQ i-search
commands compatible with
normal string search.
.dc "search-forward" "^S or ^\^\e\^"
This searches forward for a specified search string and positions
point at the end of the string if it's found.  If the string is not
found point remains unchanged.  This searches from point to the end of
the buffer, so any matches before point will be missed.
If point is moved by more than the variable
.IQ mark-threshold ,
the old
point will be pushed.
.dc "search-forward-nd" "Not Bound"
This is just like
.IQ search-forward
except that it doesn't assume a default search string, and it doesn't set
the default search string.  This is useful for defining macros, when you
want to search for something, but you don't want it to affect the current
default search string.
.dc "search-reverse" "^R"
This searches backward for a specified search string and positions
point at the beginning if the string if it's found.  If the string is
not found point remains unchanged.  This searches from point to the
beginning of the buffer, so any matches after point will be missed.
If point is moved by more than the variable
.IQ mark-threshold ,
the old
point will be pushed.
.dc "search-reverse-nd" "Not Bound"
This is just like
.IQ search-reverse
except that it doesn't assume a default search string, and it doesn't set
the default search string.  This is useful for defining macros, when you
want to search for something, but you don't want it to affect the current
default search string.
.dc "select-buffer" "^X\ B"
This selects a new or already existing buffer making it the current
one.  You can type either the buffer name or number.  If you type in
the name you need only type the name until it is unambiguous, at which
point typing Tab or Space will complete it for you.  If you want to
create a new buffer you can type Return instead of Space, and a new
empty buffer will be created.
.dc "select-buffer-1" "Not Bound"
This selects buffer number 1, if it exists.
.dc "select-buffer-10" "Not Bound"
This selects buffer number 10, if it exists.
.dc "select-buffer-2" "Not Bound"
This selects buffer number 2, if it exists.
.dc "select-buffer-3" "Not Bound"
This selects buffer number 3, if it exists.
.dc "select-buffer-4" "Not Bound"
This selects buffer number 4, if it exists.
.dc "select-buffer-5" "Not Bound"
This selects buffer number 5, if it exists.
.dc "select-buffer-6" "Not Bound"
This selects buffer number 6, if it exists.
.dc "select-buffer-7" "Not Bound"
This selects buffer number 7, if it exists.
.dc "select-buffer-8" "Not Bound"
This selects buffer number 8, if it exists.
.dc "select-buffer-9" "Not Bound"
This selects buffer number 9, if it exists.
.dc "self-insert" "Most Printing Characters"
This inserts the character that invoked it into the buffer at point.
Initially all but a few of the printing characters are bound to
.IQ self-insert .
See also
.IQ paren-flash .
.dc "send-typeout-to-buffer" "(variable)"
When this is
.IQ on
\s-2JOVE\s0 will send output that normally overwrites the
screen (temporarily) to a buffer instead.  This affects commands like
.IQ list-buffers ,
.IQ list-processes ,
.IQ shell-command-with-typeout ,
and commands that use completion.  The default value is
.IQ off .
.dc "set" "Not Bound"
This sets a specified variable to a new value.
.dc "set-mark" "^@"
This sets the mark at the current position in the buffer.  It prints
the message \*Q[Point pushed]\*U on the message line.  It says that instead
of \*Q[Mark set]\*U because when you set the mark the previous mark is still
remembered on a ring of eight marks.  So \*Q[Point pushed]\*U means point is
pushed onto the ring of marks and becomes the value of \*Qthe mark\*U.
To go through the ring of marks, use the
.IQ pop-mark
command.  If you type this enough times you will get back
to where you started.
If a
.IQ set-mark
command is given a numeric argument, it acts like a
.IQ pop-mark
command.
.dc "share-dir-pathname" "(variable)"
This tells JOVE where to find the machine-independent configuration
files that it uses, typically some system-wide
.IQ jove.rc
configuration files read on startup, 
the formatted help information for the 
.IQ describe-*
commands and the tutorial.  There is usually a compiled-in path that
should be correct if Jove was properly installed.  This path
can also be overridden with the 
.IQ -s
command-line option.
.dc "shell" "(variable)"
The shell to be used with all the shell-\(** commands command.  If your SHELL
environment variable is set, it is used as the default value of
.IQ shell ;
otherwise \*Q/bin/csh\*U is the default.
See also the description of the
.IQ shell-flags
variable to see how to change the flags passed to this shell.
.dc "shell" "Not Bound"
This starts up an interactive shell in a window; if there is already
an interactive shell, it just selects that buffer.
\s-2JOVE\s0 uses \*Q\(**shell-n\(**\*U (where
.BQ n
is the argument of the command)
as the name of the buffer in which the interacting takes place.
Thus different argument values refer to different interactive shells.
See the \s-2JOVE\s0 manual for information on how to use interactive processes.
See also the variable
.IQ wrap-process-lines .
.dc "shell-command" "^X\ !"
This runs a
.UX
command and places the output from that command in a
buffer.
Within the command, $1 can be used to refer the the filename (if any)
of the selected buffer.
\s-2JOVE\s0 creates a buffer that matches the name of the command
you specify and then attaches that buffer to a window.  So, when you
have only one window running, this command will cause \s-2JOVE\s0 to split the
window and attach the new buffer to that window.  Otherwise, \s-2JOVE\s0
finds the most convenient of the available windows and uses that one
instead.  If the buffer already exists it is first emptied (unless a
numeric argument is specified).  If it's already holding a file, not
some output from a previous command, \s-2JOVE\s0 asks permission before
emptying the buffer.  Beware that if you go ahead, not only do you
lose any unsaved changes that you made to the buffer, but the buffer's file
name remains set, making it easy to later accidentally overwrite the
original file.
See also the variable
.IQ wrap-process-lines .
.dc "shell-command-no-buffer" "Not Bound"
This is just like
.IQ shell-command
except it just runs the command without saving the output to any buffer.
It will report the success of the command in the usual way.
.dc "shell-command-to-buffer" "Not Bound"
This is just like
.IQ shell-command
except it lets you specify the
buffer to use.
.dc "shell-command-with-typeout" "Not Bound"
This is just like
.IQ shell-command
except that instead of saving the output to a buffer, and displaying
it in a window, this just types out the output in the same way that
.IQ list-buffers
does.  Actually, how this behaves depends on the value of the variable
.IQ send-typeout-to-buffer .
If it is
.IQ on
then
.IQ shell-command-with-typeout
will behave just like
.IQ shell-command .
If a numeric argument is given, the \*Qcompleted successfully\*U
message at the end is suppressed.
.dc "shell-flags" "(variable)"
This specifies a flag argument that directs the shell to
take the next argument as a command to be executed.
The default is \*Q\-c\*U (suitable for all known
.UX
shells).
Under MSDOS, the default is \*Q/c\*U (suitable for command.com and
similar MSDOS shells).
Other MSDOS shells, such as MKS KSH require that this be changed to \*Q\-c\*U.
Under MSDOS, \s-2JOVE\s0 puts quotes around the command argument if
.IQ shell-flags
starts with \*Q\-\*U.
See the
.IQ shell
variable to change the default shell.
.dc "shift-region-left" "Not Bound"
This shifts the region left by
.IQ c-indentation-increment
OR by the numeric
argument, if one is supplied.  If a negative argument is supplied the
region is shifted the other way.
.dc "shift-region-right" "Not Bound"
This shifts the region right by
.IQ c-indentation-increment
OR by the numeric
argument, if one is supplied.  If a negative argument is supplied the
region is shifted the other way.
.dc "show-match-mode" "Not Bound"
This turns on or off the Show Match minor mode in the selected
buffer.  Without a numeric argument, the command toggles the mode;
with a zero argument, the mode is turned off; with a non-zero
argument, the mode is turned on.  This mode changes \*Q}\*U, \*Q)\*U and \*Q]\*U
so that when they are typed they are inserted as usual, and then the
cursor flashes back to the matching \*Q{\*U, \*Q(\*U or \*Q[\*U (depending on
what was typed) for about half a second, and then goes back to just
after the \*Q}\*U, \*Q)\*U or \*Q]\*U that invoked the command.  This is useful
for typing in complicated expressions in a program.  You can change
how long the cursor sits on the matching parenthesis by setting the
.IQ paren-flash-delay
variable in tenths of a second.  If the matching
\*Q{\*U, \*Q(\*U or \*Q[\*U isn't visible, the line containing the match is
displayed on the message line.
.dc "shrink-window" "Not Bound"
This makes the active window one line shorter, if possible.  Windows
must be at least 2 lines high, one for the text and the other for the
mode line.
See also
.IQ grow-window .
.dc "source" "Not Bound"
This reads a bunch of \s-2JOVE\s0 commands from a file.
If a numeric argument is supplied to the
.IQ source
command, it will quietly do nothing if it cannot read the file.

The format of the file is the same as that in the
.BQ jove.rc
file, or
your private
.BQ \&.joverc
in your home directory.  There should be one
command per line and it should be as though you were responding to an
.IQ execute-named-command
command while in \s-2JOVE\s0.
A command can be optionally preceded by a numeric argument.
Lines commencing with a # are treated as comments.  Control characters such
as ^R may be represented as themselves, or as \*Q^\*U followed by \*QR\*U.
ESC should be represented by ^[.

Sometimes it is useful to do different things in
different circumstances.
To make this possible, there are three conditional commands:
.IQ if ,
.IQ ifenv ,
and
.IQ ifvar .
The
.IQ if
command takes as an operand a shell command, which it runs.
If the command succeeds, the commands after the
.IQ if ,
until a line
containing
.IQ else
or
.IQ endif ,
are performed.
Otherwise, these commands are suppressed and
the commands after any
.IQ else ,
up until an
.IQ endif ,
are executed.
Conditionals nest in the normal way.
The
.IQ ifenv
command takes as operands the name of an environment variable
and a pattern.
If the environment variable is defined and its value matches the pattern,
the
.IQ ifenv
succeeds.  Note that patterns are anchored matches from the left.
The
.IQ ifvar
command takes as operands the name of a \s-2JOVE\s0 variable
and a pattern.
If the \s-2JOVE\s0 variable is defined and its value matches the pattern,
the
.IQ ifvar
succeeds.

For example, here are some lines from the file
.BQ jove.rc .
.DS I
bind-to-key pause-jove ^[S
bind-to-key pause-jove ^[s
set process-prompt ^[^%$#]*[%$#]
# source any TERMinal-specific rc file
1 source jove.rc.$TERM
ifvar jove-features .*:iproc:
  process-bind-to-key interrupt-process ^C^C
endif
.DE
What they do is to provide two alternative key bindings for
.IQ pause-jove ,
set the variable
.IQ process-prompt ,
and attempt to call
the
.IQ source
command on the file
.BQ jove.rc.$TERM .
Because of the
numeric argument 1, there will be no complaint if this file cannot be found.
If \s-2JOVE\s0 was compiled with interactive-processes enabled (in which
case the string
.IQ :iproc:
will be part of the read-only \s-2JOVE\s0 variable 
.IQ jove-features ),
the
.iQ process-bind-to-key
command binds the key sequence Control-C Control-C to the 
.IQ interrupt-process
command, which sends the SIGINT signal to interactive shell.
.dc "space-sentence-2" (variable)
If set
.IQ on ,
two spaces are left after each sentence by commands such as
.IQ fill-paragraph ;
otherwise, one space is left.  The default is
.IQ on .
.dc "spell-command-format" "(variable)"
This is the format specification string for a command which will run 
a spell checker on a specified file and produce
a list of misspelled words on the output. This variable (and related
.IQ spell-buffer
capability) is only available
if Jove is built with support for subshells.  The string must contain one
instance of the specifier %s which will be replaced with
the pathname of the file to be checked.
.dc "spell-buffer" "Not Bound"
This saves the current buffer and runs it through the
.UX
.IQ spell
command (specified by the variable
.IQ spell-command-format
) and places
the output in buffer \*QSpell\*U.  Then \s-2JOVE\s0 lets you edit the list of
words, expecting you to delete the ones that you don't care about, i.e., the
ones you know are spelled correctly.  Then the
.IQ parse-spelling-errors-in-buffer
command comes along and finds all the
misspelled words and sets things up so the error commands
.IQ next-error ,
.IQ previous-error
and
.IQ current-error
work.  See also the variable
.IQ error-window-size .
.dc "split-current-window" "^X\ 2"
This splits the active window into two equal parts (providing the
resulting windows would be big enough) and displays the selected buffer
in both windows.  Use
.IQ delete-other-windows
to go back to 1 window mode.  If a numeric
argument is supplied, the window is split \*Qevenly\*U that many times (when
possible).
.dc "start-remembering" "Not Bound"
This is just another name for the
.IQ begin-kbd-macro
command.  It is included for backward compatibility.
.dc "stop-process" "Not Bound"
Send the signal SIGTSTP to the interactive process in the selected buffer.
This is equivalent to sending the \*Qstop\*U character (which most people
have bound to ^Z) to the process.  This only works if you are in a buffer
bound to an interactive process.
.dc "stop-remembering" "Not Bound"
This is just another name for the
.IQ end-kbd-macro
command.  It is included for backward compatibility.
.dc "string-length" "Not Bound"
This prints the number of characters in the string that point sits in.
Strings are surrounded by double quotes.  \s-2JOVE\s0 knows that
\*Q\^\e\^007\*U is considered a single character, namely \*Q^G\*U, and also
knows about other common ones, like \*Q\^\e\^r\*U (Return) and \*Q\^\e\^n\*U
(Linefeed).  This is mostly useful only for C programmers.
.dc "suspend-jove" "Not Bound"
This is a synonym for
.IQ pause-jove .
.dc "sync-frequency" "(variable)"
The temporary files used by \s-2JOVE\s0 are forced out to disk every
.IQ sync-frequency
modifications.  The default is 50, which really makes
good sense.  Unless your system is very unstable, you probably
shouldn't fool with this.
.dc "tab-width" "(variable)"
When \s-2JOVE\s0 displays a Tab character, it moves point forward to the
next multiple of this variable.  If the value is 0, tab is displayed as ^I,
not whitespace.
The default value is 8.
.dc "tag-file" "(variable)"
This is the name of the file in which \s-2JOVE\s0 should look up tag
definitions.  The default value is \*Q.\^/tags\*U.
.dc "text-attribute" "(variable)"
(IBM PC version only) This specifies the screen attribute (color) for
normal text characters.  The default is 7 (white on black).
.dc "teach-jove" "Not Bound"
This loads the Jove tutorial from from
the
.BQ ~/teach-jove
file if it was already saved there, else it loads the reference copy
from 
.BQ SHAREDIR/teach-jove
into a buffer that will save to
.BQ ~/teach-jove,
replacing the old standalone
.I teachjove
command.  The 
.IQ -T
option, if given to JOVE on the command line, will start JOVE up with
the tutorial loaded.
.dc "text-mode" "Not Bound"
This sets the major mode to Text.  This affects what \s-2JOVE\s0 considers
as characters that make up words.  For instance, Single-quote is not part of
a word in Fundamental mode, but is in Text mode.
.dc "tmp-file-pathname" "(variable)"
This tells \s-2JOVE\s0 where to put the tmp files, which is where \s-2JOVE\s0 stores
buffers internally.  The default is in
.BQ /tmp ,
or as set up when your system
was compiled, but if you want to
store them somewhere else, you can set this variable.  If your system
crashes a lot it might be a good idea to set this variable to somewhere
other than
.BQ /tmp
because the system removes all the files in
.BQ /tmp
upon
reboot, and so you would not be able to recover editor buffers using the
.IQ "jove -r"
command.

NOTE: In order for this to work correctly you must set this variable
BEFORE \s-2JOVE\s0 creates the tmp file.  You can set this in your
.BQ \&.joverc
(the closer to the beginning the better), or as soon as you
start up \s-2JOVE\s0 before you read any files.
.dc "transpose-characters" "^T"
This switches the character before point with the one after point, and
then moves forward one.  This doesn't work at the beginning of the
line, and at the end of the line it switches the two characters before
point.  Since point is moved forward, so that the character that was
before point is still before point, you can use
.IQ transpose-characters
to drag a character down the length of a line.
.dc "transpose-lines" "^X\ ^T"
This switches the current line with the one above it, and then moves
down one so that the line that was above point is still above point.
This, like
.IQ transpose-characters ,
can be used to drag a line down a page.
.dc "unbound" "Not Bound"
This command acts as if an unbound key sequence were typed.
In fact, that is its use: if you wish to unbind a key sequence,
simply bind it to this command.
.dc "update-time-frequency" "(variable)"
How often the mode line is updated (and thus the time).  The
default is 30 seconds.
.dc "use-i/d-char" "(variable)"
If your terminal has insert/delete character capability you can tell \s-2JOVE\s0
not to use it by setting this to
.IQ off .
In my opinion it is only worth using
insert/delete character at low baud rates.  WARNING: if you set this to
.IQ on
when your terminal doesn't have insert/delete character capability,
you will get weird (perhaps fatal) results.
.dc "version" "Not Bound"
Displays the version number of this \s-2JOVE\s0.
.dc "visible-bell" "(variable)"
If the terminal has a visible bell, use it instead of beeping.
.dc "visible-spaces-in-window" "Not Bound"
This displays an underscore character instead of each Space in the
window and displays a greater-than followed by spaces for each Tab
in the window.  The actual text in the buffer is not changed; only
the screen display is affected.  To turn this off you run the command
again; it toggles.
.dc "visit-file" "^X\ ^V or ^X\ ^R"
This reads a specified file into the selected buffer replacing the old
text.  If the buffer needs saving \s-2JOVE\s0 will offer to save it for you.
Sometimes you use this to start over, say if you make lots of changes
and then change your mind.  If that's the case you don't want \s-2JOVE\s0 to
save your buffer and you answer \*QNO\*U to the question.
.dc "window-find" "^X\ 4"
This lets you select another buffer in another window three
different ways.  This waits for another character which can be one of
the following:
.DS I
.ta .5i 1i 1.5i
T	Finds a tag in the other window.
^T	Finds the tag at point in the other window
F	Finds a file in the other window.
B	Selects a buffer in the other window.
.DE
This is just a convenient short hand for
.IQ split-current-window
(or
.IQ previous-window
if there are
already two windows) followed by the appropriate sequence for invoking each
command.  With this, though, there isn't the extra overhead of having to
redisplay.  In addition, you don't have to decide whether to use
.IQ split-current-window
or
.IQ previous-window
since
.IQ window-find
does the right thing.
.dc "word-abbrev-mode" "Not Bound"
This turns on or off Word Abbrev minor mode in the selected buffer.
Without a numeric argument, the command toggles the mode; with a
zero argument, the mode is turned off; with a non-zero argument,
the mode is turned on.  Word Abbrev mode lets you specify a word (an
abbreviation) and a phrase with which \s-2JOVE\s0 should substitute
the abbreviation.  You can use this to define words to expand into
long phrases, e.g., \*Qjove\*U can expand into \*QJonathan's Own Version of Emacs\*U;
another common use is defining words that you often misspell
in the same way, e.g., \*Qthier\*U => \*Qtheir\*U or \*Qteh\*U => \*Qthe\*U.  See
the information on the
.IQ auto-case-abbrev
variable.
.sp 1
There are two kinds of abbreviations: mode specific and global.  If you
define a Mode specific abbreviation in C mode, it will expand only in
buffers that are in C mode.  This is so you can have the same
abbreviation expand to different things depending on your context.
Global abbreviations expand regardless of the major mode of the buffer.
The way it works is this: \s-2JOVE\s0 looks first in the mode specific
table, and then in the global table.  Whichever it finds it in first is
the one that's used in the expansion.  If it doesn't find the word it is
left untouched.  \s-2JOVE\s0 tries to expand words when
you type a punctuation character or Space or Return.  If you are in Auto
Fill mode the expansion will be filled as if you typed it yourself.
.dc "wrap-process-lines" "(variable)"
If this variable is
.IQ on ,
the process output that is captured in a
buffer is wrapped just before the line would have as many characters as
there are columns on the screen.
This introduces extra newlines, but it makes the output more
readable.
Note that the folding does not take into account that some
characters (notably tabs) occupy more than one column of the
display.
The output of the
.IQ filter-region
command is not processed in this way
because the extra newlines are presumed to be undesired in this case.
.dc "wrap-search" "(variable)"
If set, searches will \*Qwrap around\*U the ends of the buffer instead
of stopping at the bottom or top.  The default is
.IQ off .
.dc "write-file" "^X\ ^W"
This saves the selected buffer to a specified file, and then makes that
file the default file name for this buffer.  If you specify a file
that already exists you are asked to confirm over-writing it.
.dc "write-files-on-make" "(variable)"
When set, all modified files will be written out before calling
make when the
.IQ compile-it
command is executed.  The default is
.IQ on .
.dc "write-macros-to-file" "Not Bound"
This writes the currently defined macros to a specified file in a format
appropriate for reading them back in with the
.IQ source
command.  The purpose of this command is to allow you to define macros
once and use them in other instances of \s-2JOVE\s0.
See also the
.IQ define-macro
command.
.dc "write-modified-files" "^X\ ^M"
This saves all the buffers that need saving.  If you supply a numeric
argument it asks, for each buffer, whether you really want to save it.
.dc "write-region" "Not Bound"
This writes the text in the region to a specified file.  If the file
already exists you are asked to confirm over-writing it.
.dc "write-word-abbrev-file" "Not Bound"
This writes the currently defined abbreviations to a specified file.
They can be read back in and automatically defined with
.IQ read-word-abbrev-file .
.dc "xj-mouse-commands" "^X\ m\(**"
Programs such as XJove and JoveTool generate these commands whenever a mouse button
is pressed or released, or the mouse is moved while the button is
pressed.  They are followed by parameters giving parameters for the
button pressed, the coordinates of the mouse, etc.  They are not
intended for direct use by the normal user.
.sp 1
The individual commands will now be described.
.dc "xj-mouse-copy-cut" "^X\ m8"
Performs a
.IQ copy-region
if the CTRL key was down, or a
.IQ kill-region
if
both CTRL and SHIFT were down.
This command is normally bound to the release of button 2.
.dc "xj-mouse-line" "^X\ m7"
Sets the region to be the whole line containing the cursor.
This command is normally bound to a triple down click of button 2, and
the presumed effects of the preceding double click are first undone.
.dc "xj-mouse-mark" "^X\ m5"
Both point and mark are set to the cursor.
This command is normally bound to the pressing of button 2.
.dc "xj-mouse-point" "^X\ m[01249]"
Point is set to the cursor.
This command is normally bound to the single, double, and triple
down-click and the dragging of button 1; also the dragging of button 2.
.dc "xj-mouse-word" "^X\ m6"
Sets the region to be the word (or the gap between two words)
containing the cursor.
This command is normally bound to a double down click of button 2, and
the presumed effects of the preceding single click are first undone.
.dc "xj-mouse-yank" "^X\ m3"
Performs a
.IQ yank
if the CTRL key was down.
This command is normally bound to the release of button 1.
.dc "xt-mouse" "(variable)"
When set, \s-2JOVE\s0 sends XTerm escape sequences to enable and
disable the mouse messages at appropriate times.
Warning: due to the way XTerm encodes mouse events, if
.IQ meta-key
is set, mouse actions beyond column 95 or row 95 will be misunderstood;
in any case, mouse actions beyond column 223 or row 223 will be misunderstood.
.dc "xt-mouse-commands" "ESC\ [ M\(**"
Programs such as XTerm generate these commands whenever a mouse button
is pressed or released.
XTerm does not give the user as much power as XJove.
They are followed by parameters specifying the
button pressed, the coordinates of the mouse, etc.
They are not intended for direct use by the normal user.
Set the variable
.IQ xt-mouse
on to enable XTerm mouse mode.
.sp 1
The individual commands will now be described.
.dc "xt-mouse-mark" "^X\ m5"
Both point and mark are set to the cursor.
This command is normally bound to the pressing of button 2.
.dc "xt-mouse-point" "^X\ m[01249]"
Point is set to the cursor.
This command is normally bound to the down-click of button 1.
.dc "xt-mouse-up" "^X\ m6"
As the name implies, this command is normally bound to the release
of any button (XTerm does not specify which button was released).
Note that a normally configured XTerm will not pass on mouse events
if the CTRL or SHIFT keys are pressed.  Point is set to the cursor.
If the most recently pressed button was button 1 and the CTRL key
was down (and not the SHIFT key), this command performs a
.IQ yank .
If
the most recently pressed button was button 2 and the CTRL key was
down, this command performs a
.IQ copy-region .
If the most recently pressed
button was button 2 and the CTRL and SHIFT keys were down, this
command performs a
.IQ kill-region .
.dc "yank" "^Y"
This inserts the text at the front of the kill ring (as set by an earlier
.IQ copy-region ,
.IQ kill-region ,
etc.) at point.
When you do multiple kill commands in a row, they are
merged so that the
.IQ yank
command yanks back all of them.
.dc "yank-pop" "ESC\ Y"
\s-2JOVE\s0 has a kill ring on which the last sixteen kills are stored.
This command yanks back previous texts from the kill ring.
.IQ yank
yanks a copy of the text at the
front of the ring.  If you want one of the last sixteen kills you then use
.IQ yank-pop
which rotates the ring so another different entry is now at the
front.  You can use
.IQ yank-pop
only immediately following a
.IQ yank
or
another
.IQ yank-pop .
If you supply a negative numeric argument the ring
is rotated the other way.  If you use this command enough times in a
row you will eventually get back to where you started.