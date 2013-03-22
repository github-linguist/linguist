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

;  GT.M™ is a trademark of Fidelity Information Services, Inc.

; "GT.M™ is a vetted industrial strength, transaction processing application
;  platform consisting of a key-value database engine optimized for extreme
;  transaction processing throughput & business continuity."
;                                -- http://sourceforge.net/projects/fis-gtm/



; GT.M PCRE Extension
;
; This extension tries to deliver the best possible PCRE interface for the
; M world by providing a support for PCRE with M arrays, stringified parameter
; names, simplified API, locales, exceptions and Perl5 Global Match.
;
; See pcreexamples.m for comprehensive examples on ^pcre routines usage and
; beginner level tips on match limits, exception handling and UTF-8 in GT.M.
;
; Try out the best known book on regular expressions: http://regex.info/
; For more information on PCRE see: http://pcre.org/
;
; Please feel free to contact me if you have any questions or comments,
; Piotr Koper <piotr.koper@gmail.com>
;


pcre	;GT.M PCRE Extension
	;1.0;Initial release;pkoper
	q


version()
	q $&pcre.version()

config(name)
	; name is one of: (case insensitive)
	;	"UTF8", "NEWLINE", "LINK_SIZE", "POSIX_MALLOC_THRESHOLD",
	;	"MATCH_LIMIT", "MATCH_LIMIT_RECURSION", "STACKRECURSE",
	;	"BSR", "UNICODE_PROPERTIES", "JIT", "JITTARGET"
	;
	d protect
	;
	n erropt,isstring,s,n,code
	s code=$&pcre.config(.name,.erropt,.isstring,.s,.n)
	s:code $ec=",U"_(-code)_","
	q $s(isstring:s,1:n)

compile(pattern,options,locale,mlimit,reclimit)
	; options is case insensitive and optional string with "|" joined:
	;	"ANCHORED", "CASELESS", "DOLLAR_ENDONLY", "DOTALL", "EXTENDED",
	;	"FIRSTLINE", "MULTILINE", "NO_AUTO_CAPTURE", "DUPNAMES",
	;	"UNGREEDY", "BSR_ANYCRLF", "BSR_UNICODE", "JAVASCRIPT_COMPAT",
	;	"NL_ANY", "NL_ANYCRLF", "NL_CR", "NL_CRLF","NL_LF",
	;	"UTF8", "UCP", "NO_UTF8_CHECK"
	;
	; locale is an optional Unix locale name used for pcre_maketables(),
	; cases:
	;	undefined or "":
	;		pcre_maketables() will not be called
	;	"ENV" (case insensitive):
	;		use locale in program environment defined by the
	;		environment variables LANG or LC_*
	;	specified:
	;		"pl_PL.iso88592", "pl_PL.utf8", "C", ...
	;		see locale(1), locale(2) and the output of command:
	;			$ locale -a
	;		Debian tip: use
	;			$ dpkg-reconfigure locales
	;		to enable or set system-wide locale
	;
	; mlimit (optional) limits the number of internal matching function
	;	calls in pcre_exec() execution, see PCRE manual for details
	;
	; reclimit (optional) limit for the depth of recursion when calling
	;	the internal matching function in a pcre_exec() execution,
	;	see PCRE manual for details
	;
	d protect
	;
	n erropt,ref,err,erroffset,code
	s code=$&pcre.compile(.pattern,$g(options),.erropt,.ref,.err,.erroffset,$g(locale),$g(mlimit,0),$g(reclimit,0))
	s:code $ec=",U"_(-code)_","
	q ref

exec(ref,subject,options,startoffset,length)
	; options is case insensitive and optional string with "|" joined:
	;	"ANCHORED", "BSR_ANYCRLF", "BSR_UNICODE",
	;	"NL_ANY", "NL_ANYCRLF", "NL_CR", "NL_CRLF", "NL_LF",
	;	"NOTBOL", "NOTEOL", "NOTEMPTY", "NOTEMPTY_ATSTART",
	;	"NO_START_OPTIMIZE", "NO_UTF8_CHECK",
	;	"PARTIAL_SOFT", "PARTIAL_HARD"
	;
	; startoffset is in octets, starts with 1 (like in M) (optional)
	;
	; length is subject length in octets, not chars (optional)
	;
	d protect
	;
	n erropt,code,start
	s start=$g(startoffset,1)-1
	s code=$&pcre.exec(.ref,.subject,$g(length,$zl(subject)),start,$g(options),.erropt)
	s:code<0 $ec=",U"_(-code)_","
	q code

ovector(ref,i) ; return i-element from ovector
	d protect
	;
	n n,code
	s code=$&pcre.ovector(.ref,.i,.n)
	s:code $ec=",U"_(-code)_","
	;s $ec=",U123,"
	q n

ovecsize(ref) ; return ovecsize
	d protect
	;
	n n,code
	s code=$&pcre.ovecsize(.ref,.n)
	s:code $ec=",U"_(-code)_","
	q n

fullinfo(ref,name)
	; name is one of: (case insensitive)
	;	"OPTIONS", "SIZE", "CAPTURECOUNT", "BACKREFMAX", "FIRSTBYTE",
	;	"FIRSTTABLE", "LASTLITERAL", "NAMEENTRYSIZE", "NAMECOUNT",
	; 	"STUDYSIZE", "OKPARTIAL", "JCHANGED", "HASCRORLF", "MINLENGTH",
	;	"JIT", "JITSIZE"
	; for NAME* options see also $$nametable^pcre()
	;
	d protect
	;
	n erropt,isstring,s,n,code
	s code=$&pcre.fullinfo(.ref,.name,.erropt,.isstring,.s,.n)
	s:code $ec=",U"_(-code)_","
	q $s(isstring:s,1:n)

nametable(ref,i,n) ; returns index (n) and name, or { 0, "" } for invalid i
	; i is indexed from 1
	;
	d protect
	;
	n s,code
	s code=$&pcre.nametable(.ref,.i,.n,.s)
	s:code $ec=",U"_(-code)_","
	q s

substring(ref,i,begin,end)
	s begin=$$ovector(.ref,i*2)+1,end=$$ovector(.ref,i*2+1)
	; ovector contains octet indexed data not UNICODE chars, so $ze is used
	q:'begin ""
	q $s($g(o,0):begin_","_end,1:$ze(subject,begin,end))

store(ref,i,n,o,key) ; same as above but stores captured data in n array
	n begin,end
	s begin=$$ovector(.ref,i*2)+1,end=$$ovector(.ref,i*2+1)
	q:'begin
	s key=$g(key,i)
	s:o n(key,0)=begin,n(key,1)=end
	s n(key)=$ze(subject,begin,end)
	q

gstore(ref,i,n,round,byref,o,key) ; store for global match
	n begin,end
	s begin=$$ovector(.ref,i*2)+1,end=$$ovector(.ref,i*2+1)
	q:'begin
	s key=$g(key,i)
	i byref d
	. s:o n(key,round,0)=begin,n(key,round,1)=end
	. s n(key,round)=$ze(subject,begin,end)
	e  d
	. s:o n(round,key,0)=begin,n(round,key,1)=end
	. s n(round,key)=$ze(subject,begin,end)
	q

test(subject,pattern,options,locale,mlimit,reclimit)
	; see $$compile^pcre for options, locale, mlimit and reclimit
	;
	d protect
	n ref,l
	s ref=$$compile(.pattern,$g(options),$g(locale),$g(mlimit,0),$g(reclimit,0))
	s l=$$exec(.ref,.subject)
	d free(.ref)
	q l

match(subject,pattern,match,capture,options,locale,mlimit,reclimit)
	; see $$compile^pcre for options, locale, mlimit and reclimit
	;
	; capture is case insensitive and optional string with "|" joined
	;	names or indexes to be capture
	;
	; extended options:
	;   "NAMED_ONLY" - capture only named groups
	;   "OVECTOR"    - return additional ovector data
	;
	d protect
	;
	n namedonly,ovector,ref,o,l,i,j,s,c,begin
	;
	s options=$g(options),(namedonly,ovector)=0
	f i=1:1:$l(options,"|") d
	. s o=$zco($p(options,"|",i),"u")
	. i o="NAMED_ONLY" s namedonly=1,$p(options,"|",i)=""
	. i o="OVECTOR" s ovector=1,$p(options,"|",i)=""
	s:namedonly options=options_"|NO_AUTO_CAPTURE"
	;
	k match
	s ref=$$compile(.pattern,.options,$g(locale),$g(mlimit,0),$g(reclimit,0))
	s l=$$exec(.ref,.subject)
	i $d(capture) d
	. s c="|"_capture_"|"
	. ; ovector indexed data
	. i 'namedonly f i=0:1:l-1 d:c[("|"_i_"|") store(.ref,.i,.match,.ovector)
	. ; named matches data
	. f i=1:1 s s=$$nametable(.ref,.i,.j) q:s=""  d:c[("|"_s_"|") store(.ref,.j,.match,.ovector,.s)
	e  d
	. i 'namedonly f i=0:1:l-1 d store(.ref,.i,.match,.ovector)
	. f i=1:1 s s=$$nametable(.ref,.i,.j) q:s=""  d store(.ref,.j,.match,.ovector,.s)
	d free(.ref)
	q:$q l q

global(subject,pattern,match,capture,options,locale,mlimit,reclimit)
	; options is the same as for match^pcre, extended options:
	;   "OVECTOR"    - return additional ovector data
	;   "GROUPED"    - group the result in match array by pattern groups
	;   "NAMED_ONLY" - capture only named patterns
	;
	; see pcredemo.c and pcreccp.cc from PCRE for comments on procedure
	; for Perl like global matching
	;
	d protect
	;
	n ref,c,o,ovector,byref,namedonly,utf8,crlf,start,end,matches,empty,skip,round,i,j,s,n,q
	k match
	;
	; determine additional options and remove them before calling the compile^pcre
	s options=$g(options),(ovector,byref,namedonly)=0
	f i=1:1:$l(options,"|") d
	. s o=$zco($p(options,"|",i),"u")
	. i o="NAMED_ONLY" s namedonly=1,$p(options,"|",i)=""
	. i o="GROUPED" s byref=1,$p(options,"|",i)=""
	. i o="OVECTOR" s ovector=1,$p(options,"|",i)=""
	s:namedonly options=options_"|NO_AUTO_CAPTURE"
	;
	; compile the pattern
	s ref=$$compile(.pattern,.options,$g(locale),$g(mlimit,0),$g(reclimit,0))
	;
	s:$d(capture) c="|"_capture_"|"
	s byref=$g(byref,0)
	;
	; check pattern options for UTF8 and double char new line
	s o="|"_$$fullinfo(.ref,"OPTIONS")_"|"
	s utf8=$s(o["|UTF8|":1,1:0)
	s crlf=$s(o["|NL_CRLF|":1,o["|NL_ANY|":1,o["|NL_ANYCRLF|":1,1:0)
	;
	; if none check the PCRE build options
	i crlf=0 d
	. s o=$$config("NEWLINE")
	. s crlf=$s(o="NL_CRLF":1,o="NL_ANY":1,o="NL_ANYCRLF":1,1:0)
	;
	s (start,round,i)=1,(empty,skip,q)=0
	s end=$l(subject)+1
	f  d  q:start>end!q
	. i empty d
	.. s matches=$$exec(.ref,.subject,"NOTEMPTY_ATSTART|ANCHORED",.start) ; unwind this call to optimize
	.. q:matches ; quit this do, leave empty=1, store the matches
	..
	.. ; advance if no match & clear empty
	.. s start=start+1
	.. i start>end s q=1 q
	..
	.. ; skip LF if CR was before and CRLF mode
	.. s:crlf&(($ze(subject,start-1)=$c(13))&($ze(subject,start)=$c(10))) start=start+1
	..
	.. ; skip if in a middle of UTF char
	.. i utf8 f  q:start'<end!($zbitand($c(0)_$ze(subject,start),$c(0)_$c(192))=$c(0)_$c(128))  s start=start+1
	..
	.. ; take into account skipped chars
	.. s skip=1,empty=0
	. e  d
	.. s matches=$$exec(.ref,.subject,,.start)
	.. i 'matches s q=1 q
	.
	. q:q
	. i skip s skip=0 q
	.
	. i $d(c) d
	.. ; ovector indexed data
	.. i 'namedonly f i=0:1:matches-1 d:c[("|"_i_"|") gstore(.ref,.i,.match,.round,.byref,.ovector)
	.. ; named matches data
	.. f i=1:1 s s=$$nametable(.ref,.i,.n) q:s=""  d:c[("|"_s_"|") gstore(.ref,.n,.match,.round,.byref,.ovector,.s)
	. e  d
	.. i 'namedonly f i=0:1:matches-1 d gstore(.ref,.i,.match,.round,.byref,.ovector)
	.. f i=1:1 s s=$$nametable(.ref,.i,.n) q:s=""  d gstore(.ref,.n,.match,.round,.byref,.ovector,.s)
	. s round=round+1
	.
	. s start=$$ovector(.ref,1)+1
	. s empty=(($$ovector(.ref,0)+1)=start)
	d free(.ref)
	q:$q round-1 q


replace(subject,pattern,subst,first,last,options,locale,mlimit,reclimit)
	; see $$match^pcre and $$compile^pcre for options, locale, mlimit and
	;	reclimit
	;
	; subst is a string to replace with all occurrences of matched data
	; 	\n (like \1, \2, ..) is a back ref for the n-th captured group
	;	\{name} is back ref for a named captured data
	;	\\ is replaced with \
	;
	; first is the n-th match in the subject where the substitution begins,
	;	1 .. n-1 matches are not substituted
	;	defaults to 1
	;
	; last is the n-th match in the subject where the substitution ends,
	;	n+1 .. matches are not substituted
	;	defaults to 0 (no limit)
	;
	n ref,o,n,i,j,begin,end,offset,backref,boffset,value,s
	s ref=$$compile(.pattern,,$g(locale),$g(mlimit,0),$g(reclimit,0))
	;
	; prepare back reference stack
	d global^pcre(.subst,"\\(?:(?<ref>(?:\d+|\\))|{(?<ref>[^}]+)})",.backref,,"ovector|dupnames")
	;
	s options=$g(options)_"|ovector"
	; silently remove "NAMED_ONLY" and "GROUPPED" options
	f i=1:1:$l(options,"|") d
	. s o=$zco($p(options,"|",i),"u")
	. s:o="NAMED_ONLY"!(o="GROUPED") $p(options,"|",i)=""
	q:'$$global(.subject,.pattern,.n,,.options,$g(locale),$g(mlimit,0),$g(reclimit,0)) subject
	;
	; perform the substitution on matched subject parts
	s first=$g(first,1),last=$g(last,0)
	s offset=0,i=""
	f  s i=$o(n(i)) q:i=""  d:i'<first  q:last>0&(i'<last)
	.
	. ; replace back refs in subst (s) with captured data
	. s s=subst,boffset=0,j=""
	. f  s j=$o(backref(j)) q:j=""  d
	..
	.. ; determine the back ref type and get the value
	.. ; silently ignore invalid refs
	.. s value=$s(backref(j,"ref")="\":"\\",1:$g(n(i,backref(j,"ref"))))
	..
	.. ; replace back ref with the value
	.. s begin=backref(j,0,0)
	.. s end=backref(j,0,1)
	.. s $ze(s,begin+boffset,end+boffset)=value
	.. s boffset=boffset-(end+1-begin)+$l(value)
	.
	. ; replace matched data with prepared s
	. s begin=n(i,0,0)
	. s end=n(i,0,1)
	.
	. s $ze(subject,begin+offset,end+offset)=s
	.
	. ; substitute empty matches also (Perl style)
	. ;
	. ; perl -e '$_ = "aa"; s/(b*|a)/Xy/g; print "$_\n"'
	. ; w $$replace^pcre("aa","(b*|a)","Xy")
	. ; 
	. ; perl -e '$_ = "aa"; s/(b*|aa)/Xy/g; print "$_\n"'
	. ; w $$replace^pcre("aa","(b*|aa)","Xy")
	. ; 
	. ; perl -e '$_ = "aaa"; s/(b*|aa)/Xy/g; print "$_\n"'
	. ; w $$replace^pcre("aaa","(b*|aa)","Xy")
	. ;
	. s:begin>end $ze(subject,begin+offset,begin+offset+1)=s_$ze(subject,begin+offset,begin+offset+1)
	.
	. s offset=offset-(end+1-begin)+$l(s)
	q:$q subject q

free(ref)
	d protect
	n code
	s code=$&pcre.free(.ref)
	s:code $ec=",U"_(-code)_","
	q

stackusage()
	; return the approximate amount of stack (in bytes) used per
	; recursion in pcre_exec()
	q -$&pcre.stackusage()


; Exception Handling
;
; Error conditions are handled by setting the $zc to user codes, see labels
; at the end of this file. When neither $zt nor $et are set by the user,
; the default handler (trap^pcre) is used within $zt mechanism.
;
; The default handler will write out the details of the exception, and
; depending on the caller type, it will re raise the exception. This will
; lead to:
; a) writing the exception details, when called from the GT.M prompt,
; b) writing the exception details, the M code place when the pcre routine
;    was called, and terminating the GT.M image.
;
; The user should define own exception handler using $zt or $et, see
; pcreexample.m for example exception handlers.
;

protect ; try setup $zt with default handler
	;
	; "n protect" in the $zt is a marker for trap^pcre
	s:'$l($et)&(($zt="B")!'$l($zt)) $zt="n protect d trap zg "_($zl-2)
	q

trap(stack)
	; see U* labels at the bottom of this file, some lvns are mandatory
	; all exceptions are passed through if we wasn't called from direct mode
	;
	n zl,ref,msg,place
	;
	; take the $zl if in default handler setup by protect^trap
	s zl=$p($zt,"n protect d trap zg ",2)
	;
	; clear the $zt
	s $zt=""
	;
	; source location from either stack argument, zl (default handler), or $st-2
	s place=$st($g(stack,$g(zl,$st-1)-1),"PLACE")
	;
	; clear location if called from direct mode
	s:place["^GTM$DMOD" place=""
	;
	s ref=$p($ec,",",$l($ec,",")-1)
	i $l($t(@ref)) d
	. u $p
	. w @$p($t(@ref),";",2)
	. ; %PCRE-E-COMPILE additional message
	. w:ref="U16392"&$g(erroffset) " in "_$e($g(pattern),1,erroffset)_" <-- HERE"
	. w !
	. ; write the location it has any meaning
	. w:$l(place) "%PCRE-I-RTSLOC,          At M source location ",place,!
	e  d
	. w $p($zs,",",3,4),!
	. w "%GTM-I-RTSLOC,          At M source location ",$p($zs,",",2),!
	; 
	; re raise the exception if in a default handler and not called from the direct mode
	s:$l(place)&$g(zl,0) $ec=$ec
	q

; XC API specific
;
U16384	;"%PCRE-E-ARGSMALL, Actual argument count is too small"
U16385	;"%PCRE-E-OPTNAME, Unknown option name "_$p($g(erropt),"|")
U16386	;"%PCRE-E-OBJLIMIT, Maximum number of objects exceeded"
U16387	;"%PCRE-E-INVREF, Invalid object reference"
U16388	;"%PCRE-E-INTBUF, Internal buffer too small"
U16389	;"%PCRE-E-MALLOC, Could not allocate memory"
U16390	;"%PCRE-E-STUDY, Pattern study failed: "_$g(err,"unknown reason")
U16391	;"%PCRE-E-LOCALE, Invalid locale name "_$g(locale)
U16392	;"%PCRE-E-COMPILE, Pattern compilation failed, "_$g(err,"unknown reason")
U16393	;"%PCRE-E-LENGTH, Invalid length value specified"

; PCRE specific
;
; NOTES:
;
;	U16401 exception is never raised; when pcre_exec() returns -1
;       (i.e. NOMATCH) the pcre.exec returns 0, so no exception will
;	ever raise, NOMATCH is not an uncommon situation
;
;	U16388 is raised when pcre_exec() returns 0, i.e. the ovector
;	was too small; considering that ovector size is not controlled
;	in M world, it is an exception here
;
U16401	;"%PCRE-E-NOMATCH, The subject string did not match the pattern"
U16402	;"%PCRE-E-NULL, Either compiled code or subject was passed as NULL, or ovector was NULL"
U16403	;"%PCRE-E-BADOPTION, An unrecognized bit was set in the options argument"
U16404	;"%PCRE-E-BADMAGIC, The magic number is not present in compiled code"
U16405	;"%PCRE-E-UNKNOWNOPCODE, While running the pattern match, an unknown item was encountered in the compiled pattern"
U16406	;"%PCRE-E-NOMEMORY, Call via pcre_malloc() or pcre_stack_malloc() failed"
U16407	;"%PCRE-E-NOSUBSTRING, No substring"
U16408	;"%PCRE-E-MATCHLIMIT, Match limit was reached"
U16409	;"%PCRE-E-CALLOUT, Callout function wanted to yield a distinctive error code"
U16410	;"%PCRE-E-BADUTF8, A string that contains an invalid UTF-8 byte sequence was passed as a subject"
U16411	;"%PCRE-E-BADUTF8OFFSET, The value of startoffset did not point to the beginning of a UTF-8 character or the end of the subject"
U16412	;"%PCRE-E-PARTIAL, The subject string did not match, but it did match partially"
U16414	;"%PCRE-E-INTERNAL, An unexpected internal error has occurred"
U16415	;"%PCRE-E-BADCOUNT, The value of the ovecsize argument is negative"
U16416	;"%PCRE-E-DFAUITEM, Unsupported item in the pattern, for e.g. \C o a back reference"
U16417	;"%PCRE-E-DFAUCOND, Unsupported condition item, for e.g. a back reference for a condition, or a test for recursion in a specific group"
U16418	;"%PCRE-E-DFAUMLIMIT, Match limits are unsupported for DTA matching"
U16419	;"%PCRE-E-DFAWSSIZE, Out of space in the workspace vector"
U16420	;"%PCRE-E-DFARECURSE, The output vector was not large enough while processing recursive subpattern"
U16421	;"%PCRE-E-RECURSIONLIMIT, The internal recursion limit was reached"
U16423	;"%PCRE-E-BADNEWLINE, An invalid combination of NL_xxx options was given"
U16424	;"%PCRE-E-BADOFFSET, The startoffset was negative or greater than the length of the value in length"
U16425	;"%PCRE-E-SHORTUTF8, The subject string ends with a truncated UTF-8 character and the PCRE_PARTIAL_HARD option is set"
U16426	;"%PCRE-E-RECURSELOOP, A recursion loop within the pattern was detected"
U16427	;"%PCRE-E-JITSTACKLIMIT, The memory available for the just-in-time processing stack is not large enough"
