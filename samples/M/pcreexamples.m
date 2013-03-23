;
;  GT.M PCRE Extension
;  Copyright (C) 2012 Piotr Koper <piotr.koper@gmail.com>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU Affero General Public License as
;  published by the Free Software Foundation, either version 3 of the
;  License, or (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU Affero General Public License for more details.
;
;  You should have received a copy of the GNU Affero General Public License
;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;



; GT.M PCRE Extension Examples
;
; see pcre.m for comments on routines parameters and all possible values for
; the options
;
pcreexamples ;GT.M PCRE Extension Examples
	;1.0;Initial release;pkoper
	d routines
	q


; GT.M PCRE Extension API
; The shining examples
;

test
	; Test the subject for the match
	w $$test^pcre("The quick brown fox jumps over the lazy dog","fox"),!
	w $$test^pcre("The quick brown fox jumps over the lazy dog","FoX","caseless"),!
	q

match
	n n
	; Simple match
	w $$match^pcre("The quick brown fox jumps over the lazy dog"," (\w+) ",.n),! zwr

	; Match with a named groups
	w $$match^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+) (?<second>\w+)",.n),! zwr

	; Match with a named group and limit the output to only the "second"
	w $$match^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+) (?<second>\w+)",.n,"second"),! zwr

	; Match with a named group with only named patterns
	w $$match^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+) (?<second>\w+)",.n,,"named_only"),! zwr
	q

global
	n n
	; Global match
	w $$global^pcre("The quick brown fox jumps over the lazy dog","(\w+)",.n),! zwr

	; Global match with a named groups
	w $$global^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+)\s+(?<second>\w+)",.n),! zwr

	; Global match with grouped captured data
	w $$global^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+)\s+(?<second>\w+)",.n,,"grouped"),! zwr

	; Global match with grouped captured data and only named patterns
	w $$global^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+)\s+(?<second>\w+)",.n,,"grouped|named_only"),! zwr
	q

replace
	; Just the replace
	w $$replace^pcre("The quick brown fox jumps over the lazy dog","brown","yellow"),!

	; Change the word order
	w $$replace^pcre("The quick brown fox jumps over the lazy dog","(\w+)\s+(\w+)","\2 \1"),!

	; Change the word order with named groups
	w $$replace^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+)\s+(?<second>\w+)","\{second} \{first}"),!

	; Escape the \ sequence
	w $$replace^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+)\s+(?<second>\w+)","\{second} \\{first}"),!

	; More \ chars
	w $$replace^pcre("The quick brown fox jumps over the lazy dog","(?<first>\w+)\s+(?<second>\w+)","\\\{second} \\\\{first}"),!
	q



; PCRE API
; Low level PCRE API examples
;
api
	n subject,pattern,options,offset,ref,count,i,begin,end,s,name,n

	; Setup exception trap as in myexception2^pcreexamples
	s $zt="d trap^pcre("_$st_") zg "_$zl_":apitrap^pcreexamples"

	s subject="The quick brown fox "_$c(10)_"jumps over the lazy dog"
	s pattern="(?<all>(.*?(?<red>F\S+).*?)(?<high>\w+))"

	; options are case insensitive, as well as all stringified option
	; names for all functions in this extension
	s options="CASELESS|multiLINE|NL_CrLf|NO_AUTO_CAPTURE|dotall"
	s offset=5 ; start the match with "quick"

	; Compile the pattern
	s ref=$$compile^pcre(.pattern,.options) ; pass by the reference

	; Run the match
	s count=$$exec^pcre(.ref,.subject,,.offset)
	w "count: ",count,!

	; To access the ovector array $$ovector^pcre and $$ovecsize^pcre can
	; be used.
	;
	; ovector array size is always (n + 1) * 3, where n is a number of
	; possible capture strings in the submitted pattern for the
	; $$compile^pcre(). The exact number of usable pairs of integers in
	; ovector array is by the $$exec^pcre().
	;
	w "ovecsize: ",$$ovecsize^pcre(.ref),!

	; Get the captured data in an old way
	f i=0:1:count-1 d
	. s begin=$$ovector^pcre(.ref,i*2)+1
	. s end=$$ovector^pcre(.ref,i*2+1)
	. s s=$ze(subject,begin,end)
	. w i,": ",s,!

	; See what's in the nametable
	;
	; $$nametable^pcre returns i-th element of nametable array, where the
	; index of the ovector array is passed by the reference in n, and the
	; return value is a name.
	;
	f i=1:1 s name=$$nametable^pcre(.ref,.i,.n) q:name=""  d
	. s begin=$$ovector^pcre(.ref,n*2)+1 ; the returned subject index in n
	. s end=$$ovector^pcre(.ref,n*2+1)
	. s s=$ze(subject,begin,end)
	. w name,": ",s,!

	; Use $$substring^pcre() to get the captured string instead of playing
	; with $$ovector^pcre().
	f i=0:1:count-1 w i,": ",$$substring^pcre(.ref,.i),!

	; .. and get the begin and the end index of the captured data in the
	; subject, as a side effect.
	f i=0:1:count-1 d
	. w i,": ",$$substring^pcre(.ref,.i,.begin,.end),!
	. w "begin: ",begin,!
	. w "end: ",end,!

	; Get some details on compiled pattern
	w "options: ",$$fullinfo^pcre(.ref,"OPTIONS"),!
	w "capture count: ",$$fullinfo^pcre(.ref,"CAPTURECOUNT"),!
	w "jit: ",$$fullinfo^pcre(.ref,"JIT"),!
	w "min length: ",$$fullinfo^pcre(.ref,"MINLENGTH"),!

	; Free the data internally allocated for the PCRE structures
	;
	d free^pcre(.ref)

	; Finally, raise an example exception
	;
	; see "Exception Handler Examples"
	; 
	w $t(api+4^pcreexamples),!
	w $$compile^pcre("aa)bb"),!
	w "should never be written, the %PCRE-E-COMPILE should be raised",!
	q

apitrap
	w "apitrap^pcreexamples",!
	q




; Perl5 Global Match Compatibility
;
; Global match as with /g switch on regular expressions in Perl5 is supported.
;
; See $$global^pcre and $$replace^pcre examples.


; Compatibility Case: Empty Matches
;

; Global Match
;
p5global
	w "$ perl -e '$_ = ""aa""; print ""1: $1\n"" while /(b*|aa)/mg'",!
	zsy "perl -e ""\$_ = \""aa\""; print \""1: \$1\n\"" while /(b*|aa)/mg"""
	d global^pcre("aa","b*|aa",.n) zwr
	q

; Global Replace
;
p5replace
	w "$ perl -e '$_ = ""aa""; s/(b*|a)/Xy/g; print ""$_\n""'",!
	zsy "perl -e ""\$_ = \""aa\""; s/(b*|a)/Xy/g; print \""\$_\n\"""""
	w $$replace^pcre("aa","(b*|a)","Xy"),!

	w "$ perl -e '$_ = ""aa""; s/(b*|aa)/Xy/g; print ""$_\n""'",!
	zsy "perl -e ""\$_ = \""aa\""; s/(b*|aa)/Xy/g; print \""\$_\n\"""""
	w $$replace^pcre("aa","(b*|aa)","Xy"),!

	w "$ perl -e '$_ = ""aaa""; s/(b*|aa)/Xy/g; print ""$_\n""'",!
	zsy "perl -e ""\$_ = \""aaa\""; s/(b*|aa)/Xy/g; print \""\$_\n\"""""
	w $$replace^pcre("aaa","(b*|aa)","Xy"),!
	q


; Compatibility Case: New Line Characters
;

; Multi-line with LF
;
p5lf
	w "perl -e '$_ = ""aa\nbb""; print ""1: $1\n"" while /(.*)/mg'",!
	zsy "perl -e ""\$_ = \""aa\nbb\""; print \""1: \$1\n\"" while /(.*)/mg"""
	d global^pcre("aa"_$c(10)_"bb",".*",.n,,"multiline|nl_lf") zwr
	q

; Various New Line Specs
;
p5nl
	d global^pcre("aa"_$c(13)_$c(10)_"bb",".*",.n,,"multiline|nl_lf") zwr
	d global^pcre("aa"_$c(13)_$c(10)_"bb",".*",.n,,"multiline|nl_cr") zwr
	d global^pcre("aa"_$c(13)_$c(10)_"bb",".*",.n,,"multiline|nl_crlf") zwr
	q



; PCRE library version
;
version
	w $$version^pcre,!
	q

; PCRE compile time defaults
;
newline
	w $$config^pcre("NEWLINE"),!
	q

utf8support
	w $$config^pcre("UTF8"),!
	q

; Stack Usage
;
; PCRE's stack usage discover procedure
;
stackusage
 	w $$stackusage^pcre,!
	q



; Locale Support Examples
;
; Polish language has been used as an example for I18N support in PCRE.
;

; The example word "dąb" (encoded here in UTF-8) is an "oak" in Polish.
;
; The second letter in "dąb" is <aogonek> (I18N) which is:
;   $c(177) in ISO8859-2,
;   $c(261) in UTF-8,
; see http://en.wikipedia.org/wiki/Polish_code_pages for complete listing
;

; Note of $CHAR(n) in different GT.M character modes:
;
;	In UTF-8 mode $c(177) will return two octet encoded UTF-8 char is
;	probably not an expected result when working with single octet ISO
;	encoded chars.
;
;	Use $zch(177) to create single octet ISO char, but be prepared for
;	%GTM-E-BADCHAR errors. Also the result of $l(), $a() and others might
;	be not what is expected.
;


; Locale: C or POSIX (i.e. no localization)
;
nolocale
	w $zchset,!
	w $$match^pcre("d"_$zch(177)_"b","\w{3}",.n,,,),! zwr
	q

; Locale: ISO
;
isolocale
	w $zchset,!
	w $$match^pcre("d"_$zch(177)_"b","\w{3}",.n,,,"pl_PL"),! zwr
	q

; Locale: UTF-8
;
utflocale
	; M and UTF-8 mode
	w $$match^pcre("d"_$zch(196)_$zch(133)_"b","\w{3}",.n,,"UTF8|UCP","pl_PL.UTF8"),! zwr

	; UTF-8 mode only
	w $$match^pcre("d"_$c(261)_"b","\w{3}",.n,,"UTF8|UCP","pl_PL.UTF8"),! zwr
	q

; Locale: environment ($LANG, $LC_CTYPE)
;
; Set the GT.M environment for LANG="pl_PL" or LANG="pl_PL.UTF8" to obtain
; different results.
;
envlocale
	w $ztrnlnm("LANG"),!
	w $ztrnlnm("LC_CTYPE"),!
	w $$match^pcre("d"_$c(177)_"b","\w{3}",.n,,,"env"),! zwr
	w $$match^pcre("d"_$zch(196)_$zch(133)_"b","\w{3}",.n,,"UTF8|UCP","pl_PL.UTF8"),! zwr
	q


; Notes on GT.M in UTF-8
;
;	Enabling native support for UTF-8 in GT.M requires:
;	1) libicu
;	2) environment:
;	   gtm_chset=UTF-8
;	   gtm_icu_version=4.8
;	3) recompiled object files for UTF-8
;
;
; Instructions for UTF-8 in Debian 6
;
;	1) Install libicu (libicu48)
;	   $ apt-get install libicu48
;	2) append environment setup to GT.M's user .bash_profile
;	   export gtm_chset=UTF-8
;	   export gtm_icu_version=4.8
;	3) remove *.o files from the GT.M installation directory
;	   $ rm /opt/gtm/*.o
;	4) allow GT.M's user to write new object files
;          $ chown gtm /opt/gtm
;
;
; Startup errors in UTF-8 mode
;
; %GTM-E-INVOBJ, Cannot ZLINK object file due to unexpected format
; %GTM-I-TEXT, Object compiled with CHSET=M which is different from $ZCHSET
;
; The above errors are written by the GT.M at the startup when the environment
; has the correct setup for the UTF-8, but GT.M can't use already existing
; object files for execution, because they were compiled for the M charset.
; Remove all GT.M's object files like in step 3) in the "Instructions for
; UTF-8 in Debian 6" above.
;



; Match Limits
;
;	PCRE has built-in limits on internal matching and recursion.
;
;	Those limits prevent the PCRE engine from a very long runs, especially
;	when there would be no matches and all possible	paths in the match
;	tree must be checked.
;
;	Functions using $$compile^pcre and the $$compile^pcre itself allows
;	setting MATCH_LIMIT and MATCH_LIMIT_RECURSION in optional arguments
;	named mlimit and reclimit:
;
;	  $$compile^pcre(pattern,options,locale,mlimit,reclimit)
;	  $$match^pcre(subject,pattern,match,capture,options,locale,mlimit,reclimit)
;	  $$global^pcre(subject,pattern,match,capture,options,locale,mlimit,reclimit)
;	  $$replace^pcre(subject,pattern,subst,first,last,options,locale,mlimit,reclimit)
;
;	If the mlimit or reclimit are not specified, the PCRE library
;	compilation time defaults are used.
;
limits
	w "Compile time (default) MATCH_LIMIT is: ",$$config^pcre("MATCH_LIMIT"),!
	w "Compile time (default) MATCH_LIMIT_RECURSION is: ",$$config^pcre("MATCH_LIMIT"),!
	q

; Example pattern with a very long run time
;
longrun
	w $$match^pcre("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","(\D+|<\d+>)*[!?]",.n),! zwr
	w "should never be written, the %PCRE-E-MATCHLIMIT should be raised",!
	q

; Equal to the longrun^pcreexamples, but corrected pattern
;
shortrun
	w $$match^pcre("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","((?>\D+)|<\d+>)*[!?]",.n),! zwr
	q

; Enforced mlimit
;
enforcedlimit
	w $$match^pcre("aaaaaa","(.)(.)",.n,,,,2),! zwr
	w "should never be written, the %PCRE-E-MATCHLIMIT should be raised",!
	q



; Exception Handling
;
; Error conditions are handled by setting the $zc to user codes, see labels at
; the end of this file. When neither $zt nor $et are set by the user, the
; default handler (trap^pcre) is used within $zt mechanism.
;
; The default handler will write out the details of the exception, and
; depending on the caller type, it will re raise the exception. This will lead
; to:
; a) writing the exception details, when called from the GT.M prompt,
; b) writing the exception details, the M code place when the pcre routine
;    was called, and terminating the GT.M image.
;
; The user should define own exception handler using $zt or $et, see
; pcreexamples.m for example exception handlers.
;


; Exception Handler Examples
;

; No handler
;
nohandler
	s ($ec,$et,$zt)=""
	s x=$$compile^pcre("a)b")
	w "will never be written",!
	q
;
; GTM>d nohandler^pcreexamples
; %PCRE-E-COMPILE, Pattern compilation failed, unmatched parentheses in a <-- HERE
; %PCRE-I-RTSLOC,          At M source location nohandler+2^pcreexamples
; %GTM-E-SETECODE, Non-empty value assigned to $ECODE (user-defined error trap)
; %GTM-I-RTSLOC,          At M source location trap+32^pcre
; $ (GT.M image has been terminated)
;


; Simple handler
;
myexception1
	s $zt="zg "_$zl_":mytrap1^pcreexamples"
	s x=$$compile^pcre("a)b")
	w "will never be written",!
	q

mytrap1
	w "it's a trap",!
	w $ec,!
	s $ec=""
	q
;
; GTM>d myexception1^pcreexamples
; it's a trap
; ,U16392,
; GTM>
;


; Simple handler with pcre exception details
;
myexception2
	s $zt="d trap^pcre("_$st_") zg "_$zl_":mytrap2^pcreexamples"
	s x=$$compile^pcre("a)b")
	w "will never be written",!
	q

mytrap2
	w "it's a trap",!
	w $ec,!
	s $ec=""
	q
;
; GTM>d myexception2^pcreexamples
; %PCRE-E-COMPILE, Pattern compilation failed, unmatched parentheses in a <-- HERE
; %PCRE-I-RTSLOC,          At M source location myexception2+2^pcreexamples
; it's a trap
; ,U16392,
; GTM>
;
; In this example the trap^pcre is called with optional argument (level
; of M execution stack), for which trap^pcre will produce the
; %PCRE-I-RTSLOC details.
;
; DETAILS:
;	The trap^pcre is executed in the stack frame where the error condition
;	occurred, that gives the trap^pcre routine an access to the local
;	variables like locale (locale name) or err (PCRE error message).
;	The following zg command drops stack frames up to the current frame
;	(the frame where the s $zt=.. is used), and executes the mytrap label,
;	where locale or err is not available.
;


; Simple handler with limited pcre exception details
;
myexception3
	s $zt="zg "_$zl_":mytrap3^pcreexamples"
	s x=$$compile^pcre("a)b")
	w "will never be written",!
	q

mytrap3
	d trap^pcre($st)
	w "it's a trap",!
	w $ec,!
	s $ec=""
	q
;
; GTM>d myexception3^pcreexamples
; %PCRE-E-COMPILE, Pattern compilation failed, unknown reason
; %PCRE-I-RTSLOC,          At M source location myexception3+2^pcreexamples
; it's a trap
; ,U16392,
; GTM>
;
; DETAILS:
;	The trap^pcre is executed in the stack frame where the compile^pcre
;	was called. The deeper stack frames has already	been dropped by the
;	zg command, so the err local variable is not available in this
;	context. Thats why trap^pcre doesn't know the exact reason why the
;	%PCRE-E-COMPILE was raised.
;


; Note on $st() and repeated exceptions
;
;	The $st() function returns information connected with $ec codes	in
;	a stack manner. That means that when once the $ec was set at n-th
;	execution level, any future exceptions at that level won't change
;	the $st() output for that level unless $ec is cleared.
;
;	Always clear $ec when the exception handling is done.
;


; Execute all of the routines in this file
;
routines
	w ">> test^pcreexamples",!
	d test^pcreexamples

	w !,">> match^pcreexamples",!
	d match^pcreexamples

	w !,">> global^pcreexamples",!
	d global^pcreexamples

	w !,">> replace^pcreexamples",!
	d replace^pcreexamples

	w !,">> p5global^pcreexamples",!
	d p5global^pcreexamples

	w !,">> p5replace^pcreexamples",!
	d p5replace^pcreexamples

	w !,">> p5lf^pcreexamples",!
	d p5lf^pcreexamples

	w !,">> p5nl^pcreexamples",!
	d p5nl^pcreexamples

	w !,">> version^pcreexamples",!
	d version^pcreexamples

	w !,">> newline^pcreexamples",!
	d newline^pcreexamples

	w !,">> utf8support^pcreexamples",!
	d utf8support^pcreexamples

	w !,">> stackusage^pcreexamples",!
	d stackusage^pcreexamples

	w !,">> nolocale^pcreexamples",!
	d nolocale^pcreexamples

	w !,">> isolocale^pcreexamples",!
	d isolocale^pcreexamples

	w !,">> utflocale^pcreexamples",!
	d utflocale^pcreexamples

	w !,">> envlocale^pcreexamples",!
	d envlocale^pcreexamples

	w !,">> limits^pcreexamples",!
	d limits^pcreexamples

	w !,">> longrun^pcreexamples",!
	w "(skipped, uncomment to raise the exception)",!
	; d longrun^pcreexamples

	w !,">> shortrun^pcreexamples",!
	d shortrun^pcreexamples

	w !,">> enforcedlimit^pcreexamples",!
	w "(skipped, uncomment to raise the exception)",!
	; d enforcedlimit^pcreexamples

	w !,">> nohandler^pcreexamples",!
	w "(skipped, uncomment to raise the exception)",!
	; d nohandler^pcreexamples

	w !,">> myexception1^pcreexamples",!
	d myexception1^pcreexamples

	w !,">> myexception2^pcreexamples",!
	d myexception2^pcreexamples

	w !,">> myexception3^pcreexamples",!
	d myexception3^pcreexamples
	q
