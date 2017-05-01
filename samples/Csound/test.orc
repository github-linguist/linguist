// Csound single-line comments can be preceded by a pair of forward slashes...
; ...or a semicolon.

/* Block comments begin with /* and end with */

// Orchestras begin with a header of audio parameters.
nchnls = 1
nchnls_i = 1
sr = 44100
0dbfs = 1
ksmps = 10

// The control rate kr = sr / ksmps can be omitted when the number of audio
// samples in a control period (ksmps) is set, but kr may appear in older
// orchestras.
kr = 4410

// Orchestras contain instruments. These begin with the keyword instr followed
// by a comma-separated list of numbers or names of the instrument. Instruments
// end at the endin keyword and cannot be nested.
instr 1, N_a_M_e_, +Name
  // Instruments contain statements. Here is a typical statement:
  aSignal oscil 0dbfs, 440, 1
  // Statements are terminated with a newline (possibly preceded by a comment).
  // To write a statement on several lines, precede the newline with a
  // backslash.
  prints \
    "hello, world\n";comment

  // Csound 6 introduced function syntax for opcodes with one or zero outputs.
  // The oscil statement above is the same as
  aSignal = oscil(0dbfs, 440, 1)

  // Instruments can contain control structures.
  kNote = p3
  if (kNote == 0) then
    kFrequency = 220
  elseif kNote == 1 then // Parentheses around binary expressions are optional.
    kFrequency = 440
  endif

  // Csound 6 introduced looping structures.
  iIndex = 0
  while iIndex < 5 do
    print iIndex
    iIndex += 1
  od
  iIndex = 0
  until iIndex >= 5 do
    print iIndex
    iIndex += 1
  enduntil
  // Both kinds of loops can be terminated by either od or enduntil.

  // Single-line strings are enclosed in double-quotes.
  prints "string\\\r\n\t\""
  // Multi-line strings are enclosed in pairs of curly braces.
  prints {{
    hello,

    world
  }}

  // Instruments often end with a statement containing an output opcode.
  outc aSignal
endin

// Orchestras can also contain user-defined opcodes (UDOs). Here is an
// oscillator with one audio-rate output and two control-rate inputs:
opcode anOscillator, a, kk
  kAmplitude, kFrequency xin
  aSignal vco2 kAmplitude, kFrequency
  xout aSignal
endop
instr TestOscillator
  outc(anOscillator(0dbfs, 110))
endin

// Python can be executed in Csound
// <https://csound.github.io/docs/manual/pyrun.html>. So can Lua
// <https://csound.github.io/docs/manual/lua.html>.
pyruni {{
import random

pool = [(1 + i / 10.0) ** 1.2 for i in range(100)]

def get_number_from_pool(n, p):
  if random.random() < p:
    i = int(random.random() * len(pool))
    pool[i] = n;
  return random.choice(pool)
}}

// The Csound preprocessor supports conditional compilation and including files.
#ifdef DEBUG
#undef DEBUG
#include "filename.orc"
#endif

// The preprocessor also supports object- and function-like macros. This is an
// object-like macro that defines a number:
#define A_HZ #440#

// This is a function-like macro:
#define OSCIL_MACRO(VOLUME'FREQUENCY'TABLE) #oscil $VOLUME, $FREQUENCY, $TABLE#

// Bodies of macros are enclosed in # and can contain newlines. The arguments of
// function-like macros are separated by single-quotes. Uses of macros are
// prefixed with a dollar sign.
instr TestMacro
  aSignal $OSCIL_MACRO(1'$A_HZ'1)
  // Not unlike PHP, macros expand in double-quoted strings.
  prints "The frequency of the oscillator is $A_HZ Hz.\n"
  out aSignal
endin

// Here are other things to note about Csound.

// There are two bitwise NOT operators, ~ and ¬ (U+00AC). The latter is common
// on keyboards in the United Kingdom
// <https://en.wikipedia.org/wiki/British_and_American_keyboards>.
instr TestBitwiseNOT
  print ~42
  print ¬42
endin

// Csound uses # for bitwise XOR, which the Csound manual calls bitwise
// non-equivalence <https://csound.github.io/docs/manual/opnonequiv.html>.
instr TestBitwiseXOR
  print 0 # 0
  print 0 # 1
  print 1 # 0
  print 1 # 1
endin

// Loops and if-then statements are relatively recent additions to Csound. There
// are many flow-control opcodes that involve goto and labels.
instr TestGoto
  // This...
  if p3 > 0 goto if_label
  goto else_label
if_label:
  prints "if branch\n"
  goto endif_label
else_label:
  prints "else branch\n"
endif_label:

  // ...is the same as this.
  if p3 > 0 then
    prints "if branch\n"
  else
    prints "else branch\n"
  endif

  // This...
  iIndex = 0
loop_label:
  print iIndex
  iIndex += 1
  if iIndex < 10 goto loop_label

  // ...is the same as this...
  iIndex = 0
loop_lt_label:
  print iIndex
  loop_lt iIndex, 1, 10, loop_lt_label

  // ...and this.
  iIndex = 0
  while iIndex < 10 do
    print iIndex
    iIndex += 1
  od
endin

// The prints and printks opcodes
// <https://github.com/csound/csound/blob/develop/OOps/ugrw1.c#L831>, arguably
// the primary methods of logging output, treat certain sequences of characters
// different from printf in C.
instr TestPrints
  // ^ prints an ESCAPE character (U+001B), not a CIRCUMFLEX ACCENT character
  // (U+005E). ^^ prints a CIRCUMFLEX ACCENT.
  prints "^^\n"
  // ~ prints an ESCAPE character (U+001B) followed by a [, not a TILDE
  // character (U+007E). ~~ prints a TILDE.
  prints "~~\n"
  // \A, \B, \N, \R, and \T correspond to the escaped lowercase characters (that
  // is, BELL (U+0007), BACKSPACE (U+0008), new line (U+000A), CARRIAGE RETURN
  // (U+000D), and tab (U+0009)).
  prints "\T\R\N"
  // %n, %r, and %t are the same as \n, \r, and \t, as are %N, %R, and %T.
  prints "%t%r%n"
  // %! prints a semicolon. This is a hold-over from old versions of Csound that
  // allowed comments to begin in strings.
  prints "; %!\n"
endin

// The arguments of function-like macros can be separated by # instead of '.
// These two lines define the same macro.
#define OSCIL_MACRO(VOLUME'FREQUENCY'TABLE) #oscil $VOLUME, $FREQUENCY, $TABLE#
#define OSCIL_MACRO(VOLUME#FREQUENCY#TABLE) #oscil $VOLUME, $FREQUENCY, $TABLE#

// Uses of macros can optionally be suffixed with a period.
instr TestMacroPeriodSuffix
  aSignal $OSCIL_MACRO.(1'$A_HZ'1)
  prints "The frequency of the oscillator is $A_HZ.Hz.\n"
  out aSignal
endin

// Csound has @ and @@ operator-like macros that, when followed by a literal
// non-negative integer, expand to the next power of 2 and the next power of 2
// plus 1:
//    @x = 2^(ceil(log2(x + 1))), x >= 0
//   @@0 = 2
//   @@x = 2^(ceil(log2(x))) + 1, x > 0
instr TestAt
  prints "%d  %2d  %2d\n", 0, @0, @@0
  prints "%d  %2d  %2d\n", 1, @1, @@1
  prints "%d  %2d  %2d\n", 2, @2, @@2
  prints "%d  %2d  %2d\n", 3, @3, @@3
  prints "%d  %2d  %2d\n", 4, @4, @@4
  prints "%d  %2d  %2d\n", 5, @5, @@5
  prints "%d  %2d  %2d\n", 6, @6, @@6
  prints "%d  %2d  %2d\n", 7, @7, @@7
  prints "%d  %2d  %2d\n", 8, @8, @@8
  prints "%d  %2d  %2d\n", 9, @9, @@9
endin

// Including newlines in macros can lead to confusing code.
instr MacroAbuse
  if 1 == 1 then
    prints "on\n"
#define FOO#
BAR
#endif // This ends the if block. It is not a preprocessor directive.
endin

scoreline_i {{
f 1 0 16384 10 1
i "N_a_M_e_" 0 2
i "TestOscillator" 2 2
i "TestBitwiseNOT" 0 1
i "TestBitwiseXOR" 0 1
i "TestGoto" 0 1
i "TestMacroPeriodSuffix" 4 1
i "TestAt" 0 1
i "MacroAbuse" 0 1
e
}}
