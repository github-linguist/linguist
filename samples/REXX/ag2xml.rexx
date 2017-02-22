/* Las en Amigaguidefil och omvandla till nan slags XML */
/* $VER: 2 */
options AREXX_BIFS
options AREXX_SEMANTICS
if ~open(infil,'Blitz2_V1.3.guide',R) then exit 10
if ~open(utfil,'bb2.xml',W) then exit 10

call writeln utfil,'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
call writeln utfil,'<root>'
radnr=1
inrad=readln(infil)

do while ~eof(infil)
	och=1
	do while index(inrad,'&',och)>0
		och=index(inrad,'&',och)
		if index(inrad,';',och)=0 then do
			parse value inrad with prefix =(och) +1 suffix
			inrad=prefix'&amp;'suffix
			och=index(inrad,';',och)
		end
	end
	do while index(inrad,'<')>0
		parse var inrad prefix '<' suffix
		inrad = prefix'&lt;'suffix
	end
	do while index(inrad,'>')>0
		parse var inrad prefix '>' suffix
		inrad = prefix'&gt;'suffix
	end
	inrad=behandlarad(inrad)
	if right(inrad,1)~='>' & strip(inrad)~='' then inrad=inrad || ' '
	testrad=inrad
	do while index(testrad,'>') > 0
		parse var testrad prefix '<' . '>' suffix
		testrad = prefix || suffix
	end
	if length(testrad)<65 then inrad = inrad || '0d'x
	call writech utfil,inrad
	inrad=readln(infil)
	radnr=radnr+1
end
call close(infil)
call writeln utfil,'</root>'
call close(utfil)
exit 0

behandlarad: procedure
parse arg inrad
do forever
	if abbrev(inrad,'@NODE') then do
		parse var inrad '@NODE ' nod inrad
		/* say 'Hittade nod:' nod */
		inrad='<story id="' || nod || '">' || inrad
	end
	if inrad='@ENDNODE' then inrad='</story>' || '0d'x
	/* say inrad */
	if abbrev(inrad,'-----') then inrad='<streck>	</streck>'
	if abbrev(inrad,'Command'), 
	| abbrev(inrad,'Function'),
	| abbrev(inrad,'Statement') then do
		parse var inrad kommandotyp ':' inrad
		/* if index(inrad,'@{')>0 then */
		parse var inrad inrad '@{' rest
		if rest~='' then rest='@{' || rest
		/* say 'rest:' rest */
		inrad='<commandheadline>'||strip(kommandotyp)||'	</commandheadline><commandname>'||behandlarad(inrad)||'</commandname>'||behandlarad(rest)
	end
	if index(inrad,'@{')>0 then do
		parse var inrad inrad '@{' tagg '}' rest
		select
			when tagg='fg shine' then tagg='<fgshine>'
			when tagg='fg text'  then tagg='</fgshine>'
			when tagg='b'		 then tagg='<bold>'
			when tagg='ub'		 then tagg='</bold>'
			/* @{" SpriteMode " link BUM_SPRITEMODE} */
			when abbrev(tagg,'"') then do
				parse var tagg '"' besk '"' . 'link' dest
				tagg='<link dest="' || dest || '">' || besk || '</link>'
			end
			otherwise tagg='<okand>'
		end
		rest=behandlarad(rest)
		/*
			if index(rest,'@{')>0 then rest=behandlarad(left(rest,index(rest,'@{'))) || substr(rest,index(rest,'@{'))		
		*/
			inrad=inrad || tagg || rest
		/* iterate */
	end
	if abbrev(inrad,'@') then do
		say 'Hittade okand tagg:' inrad
		/* inrad='<okand>' inrad '</okand>' */
		parse var inrad '@' tagg inrad
		if abbrev(tagg,'$') then parse var tagg '$' tagg ':'
		inrad='<'tagg'>'inrad'</'tagg'>'
	end
	if abbrev(inrad,'Modes') then do
		parse var inrad . ':' inrad
		inrad='<modeheadline>Modes:	</modeheadline><modename>' || strip(inrad) || '</modename>'
	end
	if abbrev(inrad,'Syntax') then do
		parse var inrad . ':' inrad
		inrad='<syntaxheadline>Syntax:	</syntaxheadline><syntax>' || strip(inrad) || '</syntax>'
	end
	return inrad
end