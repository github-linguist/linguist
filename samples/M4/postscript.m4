divert(-1)
   postscript.m4                  Initialization for Postscript output.

* Circuit_macros Version 9.3, copyright (c) 2020 J. D. Aplevich under      *
* the LaTeX Project Public Licence in file Licence.txt. The files of       *
* this distribution may be redistributed or modified provided that this    *
* copyright notice is included and provided that modifications are clearly *
* marked to distinguish them from this distribution.  There is no warranty *
* whatsoever for these files.                                              *

define(`m4picprocessor',dpic)
define(`m4postprocessor',postscript)

ifdef(`libgen_',,`include(libgen.m4)divert(-1)')dnl

                                Color utilities
define(`setrgb',`pushdef(`r_',`$1')pushdef(`g_',`$2')pushdef(`b_',`$3')dnl
pushdef(`m4cl_',ifelse(`$4',,lcspec,`$4'))dnl
 command sprintf(" /m4cl_ {%7.5f %7.5f %7.5f} def",r_,g_,b_)
 command " m4cl_ setrgbcolor"')

define(`resetrgb',`popdef(`m4cl_')popdef(`r_')popdef(`g_')popdef(`b_')dnl
 ifdef(`r_',
  `command sprintf(" %7.5f %7.5f %7.5f setrgbcolor",r_,g_,b_)',
  `command " 0 0 0 setrgbcolor"') ')

                                `rgbdraw(color triple, drawing commands)'
define(`rgbdraw',`setrgb(`$1',`$2',`$3')
  shift(shift(shift($@)))
  resetrgb')
                                `rgbfill(color triple, closed path)'
define(`rgbfill',
 `command " npath /npath {} def /endstroke {} def"
  ifm4_rgbtestcomma(`$1',
   `shift($@)
    command "gsave `$1' setrgbcolor"',
   `shift(shift(shift($@)))
    command sprintf("gsave %7.5f %7.5f %7.5f setrgbcolor", `$1',`$2',`$3')')
  command " fill grestore ostroke"
  command " /endstroke {ostroke} def /npath {newpath} def"')
                                `Top-level test for comma'
define(`ifm4_rgbtestcomma',`ifinstr(`$1',`,',`$2',`$3')')

                                Define some primary colors
define(`defineRGBprimaries',`
command "/white {1 1 1} def
/lightgrey {0.75 0.75 0.75} def
/lightgray {0.75 0.75 0.75} def
/grey {0.5 0.5 0.5} def
/gray {0.5 0.5 0.5} def
/darkgrey {0.25 0.25 0.25} def
/darkgray {0.25 0.25 0.25} def
/black {0 0 0} def
/red {1 0 0} def
/green {0 1 0} def
/blue {0 0 1} def
/cyan {0 1 1} def
/magenta {1 0 1} def
/yellow {1 1 0} def"')

define(`thinlines_',`linethick = 0.4
 arrowwid = 0.04*scale; arrowht = 0.2/3*scale
 command " 0.4 setlinewidth";')
define(`thicklines_',`linethick = 0.8
 arrowwid = 0.05*scale; arrowht = 0.1*scale
 command " 0.8 setlinewidth";')
                                `linethick_(x)
                                 set line width to x pt (default 0.8)
                                 and scale arrowhead parameters'
define(`linethick_',`linethick = ifelse(`$1',,`0.8',`$1'); dnl
 arrowwid = ifelse(`$1',,`0.05',linethick/16)*scale; dnl
 arrowht = ifelse(`$1',,`0.1',linethick/8)*scale;')

divert(0)dnl
