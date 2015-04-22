# Highlighting

GitHub uses Linguist to power the syntax highlighting of code blocks. You know, the difference between this

```
var util = require('util');
var net = require('net');
var stream = require('stream');
var url = require('url');
```

and this

``` js
var util = require('util');
var net = require('net');
var stream = require('stream');
var url = require('url');
```

In order to syntax highlight a code block, you must provide a "language name" to the code block syntax of your Markup of choice.

For example, for Markdown, this would look like:

    ``` javascript

Whereas for AsciiDoc, this might be:

    [source,javascript]

Below is a comprehensive list of of the language codes necessary for syntax highlighting a code block. Generally speaking, the slugified version of a language name, as well as any of its extensions, defined the language code.

| Languages | Codes
|-----------|-------
| ABAP | abap
| AGS Script | ags-script, asc, ash
| AMPL | ampl
| ANTLR | antlr, g4
| APL | apl, dyalog
| ASP | asp, asax, ascx, ashx, asmx, aspx, axd
| ATS | ats, dats, hats, sats
| ActionScript | actionscript, as
| Ada | ada, adb, ads
| Agda | agda
| Alloy | alloy, als
| Ant Build System | ant-build-system
| ApacheConf | apacheconf
| Apex | apex, cls
| AppleScript | applescript, scpt
| Arc | arc
| Arduino | arduino, ino
| AsciiDoc | asciidoc, adoc, asc
| AspectJ | aspectj, aj
| Assembly | assembly, asm, a51, nasm
| Augeas | augeas, aug
| AutoHotkey | autohotkey, ahk, ahkl
| AutoIt | autoit, au3
| Awk | awk, auk, gawk, mawk, nawk
| Batchfile | batchfile, bat, cmd
| Befunge | befunge
| Bison | bison, y
| BitBake | bitbake, bb
| BlitzBasic | blitzbasic, bb, decls
| BlitzMax | blitzmax, bmx
| Bluespec | bluespec, bsv
| Boo | boo
| Brainfuck | brainfuck, b, bf
| Brightscript | brightscript, brs
| Bro | bro
| C | c, cats, h, idc, w
| C# | c#, cs, cshtml, csx
| C++ | c++, cpp, cc, cp, cxx, h, h++, hh, hpp, hxx, inl, ipp, tcc, tpp
| C-ObjDump | c-objdump
| C2hs Haskell | c2hs-haskell, chs
| CLIPS | clips, clp
| CMake | cmake, cmakein
| COBOL | cobol, cob, cbl, ccp, cpy
| CSS | css
| Cap'n Proto | cap'n-proto, capnp
| CartoCSS | cartocss, mss
| Ceylon | ceylon
| Chapel | chapel, chpl
| ChucK | chuck, ck
| Cirru | cirru
| Clarion | clarion, clw
| Clean | clean, icl, dcl
| Clojure | clojure, clj, boot, cl2, cljc, cljs, cljshl, cljscm, cljx, hic
| CoffeeScript | coffeescript, coffee, _coffee, cjsx, cson, iced
| ColdFusion | coldfusion, cfm, cfml
| ColdFusion CFC | coldfusion-cfc, cfc
| Common Lisp | common-lisp, lisp, asd, cl, lsp, ny, podsl
| Component Pascal | component-pascal, cp, cps
| Cool | cool, cl
| Coq | coq, v
| Cpp-ObjDump | cpp-objdump, cppobjdump, c++-objdump, c++objdump, cxx-objdump
| Creole | creole
| Crystal | crystal, cr
| Cucumber | cucumber, feature
| Cuda | cuda, cu, cuh
| Cycript | cycript, cy
| Cython | cython, pyx, pxd, pxi
| D | d, di
| D-ObjDump | d-objdump
| DIGITAL Command Language | digital-command-language, com
| DM | dm
| DTrace | dtrace, d
| Darcs Patch | darcs-patch, darcspatch, dpatch
| Dart | dart
| Diff | diff, patch
| Dockerfile | dockerfile
| Dogescript | dogescript, djs
| Dylan | dylan, dyl, intr, lid
| E | e, E
| ECL | ecl, eclxml
| Eagle | eagle, sch, brd
| Ecere Projects | ecere-projects, epj
| Eiffel | eiffel, e
| Elixir | elixir, ex, exs
| Elm | elm
| Emacs Lisp | emacs-lisp, el, emacs, emacsdesktop
| EmberScript | emberscript, em
| Erlang | erlang, erl, es, escript, hrl
| F# | f#, fs, fsi, fsx
| FLUX | flux, fx
| FORTRAN | fortran, f90, f, f03, f08, f77, f95, for, fpp
| Factor | factor
| Fancy | fancy, fy, fancypack
| Fantom | fantom, fan
| Filterscript | filterscript, fs
| Formatted | formatted, for
| Forth | forth, fth, 4th, f, for, fr, frt, fs
| Frege | frege, fr
| G-code | g-code, g, gco, gcode
| GAMS | gams, gms
| GAP | gap, g, gd, gi, tst
| GAS | gas, s
| GDScript | gdscript, gd
| GLSL | glsl, fp, frag, frg, fs, fshader, geo, geom, glslv, gshader, shader, vert, vrx, vshader
| Game Maker Language | game-maker-language, gml
| Genshi | genshi, kid
| Gentoo Ebuild | gentoo-ebuild, ebuild
| Gentoo Eclass | gentoo-eclass, eclass
| Gettext Catalog | gettext-catalog, po, pot
| Glyph | glyph, glf
| Gnuplot | gnuplot, gp, gnu, plot, plt
| Go | go
| Golo | golo
| Gosu | gosu, gs, gst, gsx, vark
| Grace | grace
| Gradle | gradle
| Grammatical Framework | grammatical-framework, gf
| Graph Modeling Language | graph-modeling-language, gml
| Graphviz (DOT) | graphviz-(dot), dot, gv
| Groff | groff, man, 1, 2, 3, 4, 5, 6, 7
| Groovy | groovy, grt, gtpl, gvy
| Groovy Server Pages | groovy-server-pages, gsp
| HTML | html, htm, htmlhl, st, xht, xhtml
| HTML+Django | html+django, mustache, jinja
| HTML+ERB | html+erb, erb, erbdeface
| HTML+PHP | html+php, phtml
| HTTP | http
| Hack | hack, hh, php
| Haml | haml, hamldeface
| Handlebars | handlebars, hbs
| Harbour | harbour, hb
| Haskell | haskell, hs, hsc
| Haxe | haxe, hx, hxsl
| Hy | hy
| IDL | idl, pro, dlm
| IGOR Pro | igor-pro, ipf
| INI | ini, cfg, prefs, pro, properties
| IRC log | irc-log, irclog, weechatlog
| Idris | idris, idr, lidr
| Inform 7 | inform-7, ni, i7x
| Inno Setup | inno-setup, iss
| Io | io
| Ioke | ioke, ik
| Isabelle | isabelle, thy
| J | j, ijs
| JFlex | jflex, flex
| JSON | json, lock
| JSON5 | json5
| JSONLD | jsonld
| JSONiq | jsoniq, jq
| Jade | jade
| Jasmin | jasmin, j
| Java | java
| Java Server Pages | java-server-pages, jsp
| JavaScript | javascript, js, _js, bones, es6, frag, gs, jake, jsb, jsfl, jsm, jss, jsx, njs, pac, sjs, ssjs, sublime-build, sublime-commands, sublime-completions, sublime-keymap, sublime-macro, sublime-menu, sublime-mousemap, sublime-project, sublime-settings, sublime-theme, sublime-workspace, sublime_metrics, sublime_session, xsjs, xsjslib
| Julia | julia, jl
| KRL | krl
| KiCad | kicad, sch
| Kit | kit
| Kotlin | kotlin, kt, ktm, kts
| LFE | lfe
| LLVM | llvm, ll
| LOLCODE | lolcode, lol
| LSL | lsl
| LabVIEW | labview, lvproj
| Lasso | lasso, las, lasso8, lasso9, ldml
| Latte | latte
| Lean | lean, hlean
| Less | less
| LilyPond | lilypond, ly, ily
| Limbo | limbo, b, m
| Linker Script | linker-script, ld, lds
| Liquid | liquid
| Literate Agda | literate-agda, lagda
| Literate CoffeeScript | literate-coffeescript, litcoffee
| Literate Haskell | literate-haskell, lhs
| LiveScript | livescript, ls, _ls
| Logos | logos, xm, x, xi
| Logtalk | logtalk, lgt
| LookML | lookml
| LoomScript | loomscript, ls
| Lua | lua, fcgi, nse, pd_lua, rbxs, wlua
| M | m, mumps
| MTML | mtml
| MUF | muf, m
| Makefile | makefile, mak, d, mk
| Mako | mako, mao
| Markdown | markdown, md, mkd, mkdn, mkdown, ron
| Mask | mask
| Mathematica | mathematica, cdf, m, ma, nb, nbp, wl, wlt
| Matlab | matlab, m
| Maven POM | maven-pom
| Max | max, maxpat, maxhelp, maxproj, mxt, pat
| MediaWiki | mediawiki
| Mercury | mercury, m, moo
| MiniD | minid
| Mirah | mirah, druby, duby, mir
| Modelica | modelica, mo
| Module Management System | module-management-system, mms, mmk
| Monkey | monkey
| Moocode | moocode, moo
| MoonScript | moonscript, moon
| Myghty | myghty, myt
| NL | nl
| NSIS | nsis, nsi, nsh
| Nemerle | nemerle, n
| NetLinx | netlinx, axs, axi
| NetLinx+ERB | netlinx+erb, axserb, axierb
| NetLogo | netlogo, nlogo
| NewLisp | newlisp, nl, lisp, lsp
| Nginx | nginx, nginxconf
| Nimrod | nimrod, nim
| Ninja | ninja
| Nit | nit
| Nix | nix
| Nu | nu
| NumPy | numpy, numpyw, numsc
| OCaml | ocaml, ml, eliom, eliomi, ml4, mli, mll, mly
| ObjDump | objdump
| Objective-C | objective-c, m, h
| Objective-C++ | objective-c++, mm
| Objective-J | objective-j, j, sj
| Omgrofl | omgrofl
| Opa | opa
| Opal | opal
| OpenCL | opencl, cl
| OpenEdge ABL | openedge-abl, p, cls
| OpenSCAD | openscad, scad
| Org | org
| Ox | ox, oxh, oxo
| Oxygene | oxygene
| Oz | oz
| PAWN | pawn, pwn
| PHP | php, aw, ctp, fcgi, php3, php4, php5, phpt
| PLSQL | plsql, pls, pkb, pks, plb, sql
| PLpgSQL | plpgsql, sql
| Pan | pan
| Papyrus | papyrus, psc
| Parrot | parrot
| Parrot Assembly | parrot-assembly, pasm
| Parrot Internal Representation | parrot-internal-representation, pir
| Pascal | pascal, pas, dfm, dpr, lpr, pp
| Perl | perl, pl, cgi, fcgi, ph, plx, pm, pod, psgi, t
| Perl6 | perl6, 6pl, 6pm, nqp, p6, p6l, p6m, pl, pl6, pm, pm6, t
| PigLatin | piglatin, pig
| Pike | pike, pmod
| Pod | pod
| PogoScript | pogoscript, pogo
| PostScript | postscript, ps, eps
| PowerShell | powershell, ps1, psd1, psm1
| Processing | processing, pde
| Prolog | prolog, pl, ecl, pro
| Propeller Spin | propeller-spin, spin
| Protocol Buffer | protocol-buffer, proto
| Public Key | public-key, asc, pub
| Puppet | puppet, pp
| Pure Data | pure-data, pd
| PureBasic | purebasic, pb, pbi
| PureScript | purescript, purs
| Python | python, py, cgi, fcgi, gyp, lmi, pyde, pyp, pyt, pyw, tac, wsgi, xpy
| Python traceback | python-traceback, pytb
| QML | qml
| QMake | qmake, pro, pri
| R | r, rd, rsx
| RAML | raml
| RDoc | rdoc
| REALbasic | realbasic, rbbas, rbfrm, rbmnu, rbres, rbtbar, rbuistate
| RHTML | rhtml
| RMarkdown | rmarkdown, rmd
| Racket | racket, rkt, rktd, rktl, scrbl
| Ragel in Ruby Host | ragel-in-ruby-host, rl
| Raw token data | raw-token-data, raw
| Rebol | rebol, reb, r, r2, r3
| Red | red, reds
| Redcode | redcode, cw
| RenderScript | renderscript, rs, rsh
| RobotFramework | robotframework, robot
| Rouge | rouge, rg
| Ruby | ruby, rb, builder, fcgi, gemspec, god, irbrc, jbuilder, mspec, pluginspec, podspec, rabl, rake, rbuild, rbw, rbx, ru, thor, watchr
| Rust | rust, rs
| SAS | sas
| SCSS | scss
| SPARQL | sparql, rq
| SQF | sqf, hqf
| SQL | sql, cql, ddl, prc, tab, udf, viw
| SQLPL | sqlpl, sql, db2
| STON | ston
| SVG | svg
| Sage | sage, sagews
| SaltStack | saltstack, sls
| Sass | sass
| Scala | scala, sbt, sc
| Scaml | scaml
| Scheme | scheme, scm, sld, sls, sps, ss
| Scilab | scilab, sci, sce, tst
| Self | self
| Shell | shell, sh, bash, bats, cgi, command, fcgi, ksh, tmux, tool, zsh
| ShellSession | shellsession, sh-session
| Shen | shen
| Slash | slash, sl
| Slim | slim
| Smalltalk | smalltalk, st, cs
| Smarty | smarty, tpl
| SourcePawn | sourcepawn, sp, sma
| Squirrel | squirrel, nut
| Standard ML | standard-ml, ML, fun, sig, sml
| Stata | stata, do, ado, doh, ihlp, mata, matah, sthlp
| Stylus | stylus, styl
| SuperCollider | supercollider, scd, sc
| Swift | swift
| SystemVerilog | systemverilog, sv, svh, vh
| TOML | toml
| TXL | txl
| Tcl | tcl, adp, tm
| Tcsh | tcsh, csh
| TeX | tex, aux, bbx, bib, cbx, cls, dtx, ins, lbx, ltx, mkii, mkiv, mkvi, sty, toc
| Tea | tea
| Text | text, txt, fr
| Textile | textile
| Thrift | thrift
| Turing | turing, t, tu
| Turtle | turtle, ttl
| Twig | twig
| TypeScript | typescript, ts
| Unified Parallel C | unified-parallel-c, upc
| UnrealScript | unrealscript, uc
| VCL | vcl
| VHDL | vhdl, vhd, vhf, vhi, vho, vhs, vht, vhw
| Vala | vala, vapi
| Verilog | verilog, v, veo
| VimL | viml, vim
| Visual Basic | visual-basic, vb, bas, cls, frm, frx, vba, vbhtml, vbs
| Volt | volt
| Web Ontology Language | web-ontology-language, owl
| WebIDL | webidl
| XC | xc
| XML | xml, ant, axml, ccxml, clixml, cproject, csproj, ct, dita, ditamap, ditaval, dllconfig, filters, fsproj, fxml, glade, grxml, ivy, jelly, kml, launch, mm, mxml, nproj, nuspec, odd, osm, plist, pluginspec, ps1xml, psc1, pt, rdf, rss, scxml, srdf, storyboard, stTheme, sublime-snippet, targets, tmCommand, tml, tmLanguage, tmPreferences, tmSnippet, tmTheme, ts, ui, urdf, vbproj, vcxproj, vxml, wsdl, wsf, wxi, wxl, wxs, x3d, xacro, xaml, xib, xlf, xliff, xmi, xmldist, xsd, xul, zcml
| XProc | xproc, xpl
| XQuery | xquery, xq, xql, xqm, xqy
| XS | xs
| XSLT | xslt, xsl
| Xojo | xojo, xojo_code, xojo_menu, xojo_report, xojo_script, xojo_toolbar, xojo_window
| Xtend | xtend
| YAML | yaml, yml, reek, rviz
| Zephir | zephir, zep
| Zimpl | zimpl, zmpl, zpl
| desktop | desktop, desktopin
| eC | ec, eh
| edn | edn
| fish | fish
| mupad | mupad, mu
| nesC | nesc, nc
| ooc | ooc
| reStructuredText | restructuredtext, rst, rest
| wisp | wisp
| xBase | xbase, prg
