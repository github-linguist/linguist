"MISC for
		  THE HITCHHIKER'S GUIDE TO THE GALAXY
	     (c) 1984 by Infocom, Inc.  All Rights Reserved."

;"former MACROS.ZIL stuff"

<SETG C-ENABLED? 0>

<SETG C-ENABLED 1>

<SETG C-DISABLED 0>

;<ROUTINE ENABLED? (RTN "AUX" C E)
	 <SET E <REST ,C-TABLE ,C-TABLELEN>>
	 <SET C <REST ,C-TABLE ,C-INTS>>
	 <REPEAT ()
		 <COND (<==? .C .E> <RFALSE>)
		       (<EQUAL? <GET .C ,C-RTN> .RTN>
			<COND (<0? <GET .C ,C-ENABLED?>>
			       <RFALSE>)
			      (T
			       <RTRUE>)>)>
		 <SET C <REST .C ,C-INTLEN>>>>

;<ROUTINE QUEUED? (RTN "AUX" C E)
	 <SET E <REST ,C-TABLE ,C-TABLELEN>>
	 <SET C <REST ,C-TABLE ,C-INTS>>
	 <REPEAT ()
		 <COND (<==? .C .E> <RFALSE>)
		       (<EQUAL? <GET .C ,C-RTN> .RTN>
			<COND (<OR <0? <GET .C ,C-ENABLED?>>
				   <0? <GET .C ,C-TICK>>>
			       <RFALSE>)
			      (T <RTRUE>)>)>
		 <SET C <REST .C ,C-INTLEN>>>>

<ROUTINE RUNNING? (RTN "AUX" C E TICK)
	 <SET E <REST ,C-TABLE ,C-TABLELEN>>
	 <SET C <REST ,C-TABLE ,C-INTS>>
	 <REPEAT ()
		 <COND (<==? .C .E> <RFALSE>)
		       (<EQUAL? <GET .C ,C-RTN> .RTN>
			<COND (<OR <0? <GET .C ,C-ENABLED?>>
				   <0? <SET TICK <GET .C ,C-TICK>>>
				   <G? .TICK 1>>
			       <RFALSE>)
			      (T <RTRUE>)>)>
		 <SET C <REST .C ,C-INTLEN>>>>

<DEFMAC TELL ("ARGS" A)
	<FORM PROG ()
	      !<MAPF ,LIST
		     <FUNCTION ("AUX" E P O)
			  <COND (<EMPTY? .A> <MAPSTOP>)
				(<SET E <NTH .A 1>>
				 <SET A <REST .A>>)>
			  <COND (<TYPE? .E ATOM>
				 <COND (<OR <=? <SET P <SPNAME .E>>
						"CRLF">
					    <=? .P "CR">>
					<MAPRET '<CRLF>>)
				       (<EMPTY? .A>
					<ERROR INDICATOR-AT-END? .E>)
				       (ELSE
					<SET O <NTH .A 1>>
					<SET A <REST .A>>
					<COND (<OR <=? <SET P <SPNAME .E>>
						       "DESC">
						   <=? .P "D">
						   <=? .P "OBJ">
						   <=? .P "O">>
					       <MAPRET <FORM PRINTD .O>>)
					      (<OR <=? .P "NUM">
						   <=? .P "N">>
					       <MAPRET <FORM PRINTN .O>>)
					      (<OR <=? .P "CHAR">
						   <=? .P "CHR">
						   <=? .P "C">>
					       <MAPRET <FORM PRINTC .O>>)
					      (ELSE
					       <MAPRET
						 <FORM PRINT
						       <FORM GETP .O .E>>>)>)>)
				(<TYPE? .E STRING ZSTRING>
				 <MAPRET <FORM PRINTI .E>>)
				(<TYPE? .E FORM LVAL GVAL>
				 <MAPRET <FORM PRINT .E>>)
				(ELSE <ERROR UNKNOWN-TYPE .E>)>>>>>

<DEFMAC VERB? ("ARGS" ATMS)
	<MULTIFROB PRSA .ATMS>>

<DEFMAC PRSO? ("ARGS" ATMS)
	<MULTIFROB PRSO .ATMS>>

<DEFMAC PRSI? ("ARGS" ATMS)
	<MULTIFROB PRSI .ATMS>>

<DEFMAC ROOM? ("ARGS" ATMS)
	<MULTIFROB HERE .ATMS>>

<DEFINE MULTIFROB (X ATMS "AUX" (OO (OR)) (O .OO) (L ()) ATM) 
	<REPEAT ()
		<COND (<EMPTY? .ATMS>
		       <RETURN!- <COND (<LENGTH? .OO 1> <ERROR .X>)
				       (<LENGTH? .OO 2> <NTH .OO 2>)
				       (ELSE <CHTYPE .OO FORM>)>>)>
		<REPEAT ()
			<COND (<EMPTY? .ATMS> <RETURN!->)>
			<SET ATM <NTH .ATMS 1>>
			<SET L
			     (<COND (<TYPE? .ATM ATOM>
				     <FORM GVAL
					   <COND (<==? .X PRSA>
						  <PARSE
						    <STRING "V?"
							    <SPNAME .ATM>>>)
						 (ELSE .ATM)>>)
				    (ELSE .ATM)>
			      !.L)>
			<SET ATMS <REST .ATMS>>
			<COND (<==? <LENGTH .L> 3> <RETURN!->)>>
		<SET O <REST <PUTREST .O (<FORM EQUAL? <FORM GVAL .X> !.L>)>>>
		<SET L ()>>>

<DEFMAC BSET ('OBJ "ARGS" BITS)
	<MULTIBITS FSET .OBJ .BITS>>

<DEFMAC BCLEAR ('OBJ "ARGS" BITS)
	<MULTIBITS FCLEAR .OBJ .BITS>>

<DEFMAC BSET? ('OBJ "ARGS" BITS)
	<MULTIBITS FSET? .OBJ .BITS>>

<DEFINE MULTIBITS (X OBJ ATMS "AUX" (O ()) ATM) 
	<REPEAT ()
		<COND (<EMPTY? .ATMS>
		       <RETURN!- <COND (<LENGTH? .O 1> <NTH .O 1>)
				       (<EQUAL? .X FSET?> <FORM OR !.O>)
				       (ELSE <FORM PROG () !.O>)>>)>
		<SET ATM <NTH .ATMS 1>>
		<SET ATMS <REST .ATMS>>
		<SET O
		     (<FORM .X
			    .OBJ
			    <COND (<TYPE? .ATM FORM> .ATM)
				  (ELSE <FORM GVAL .ATM>)>>
		      !.O)>>>

<DEFMAC RFATAL ()
	'<PROG () <PUSH 2> <RSTACK>>>

<DEFMAC PROB ('BASE?)
	<FORM NOT <FORM L? .BASE? '<RANDOM 100>>>>

<ROUTINE PICK-ONE (FROB)
	 <GET .FROB <RANDOM <GET .FROB 0>>>>

<DEFMAC ENABLE ('INT)
	<FORM PUT .INT ,C-ENABLED? 1>>

<DEFMAC DISABLE ('INT)
	<FORM PUT .INT ,C-ENABLED? 0>>

;"former MAIN.ZIL stuff"

<GLOBAL PLAYER <>>

<GLOBAL P-WON <>>

<CONSTANT M-FATAL 2>
<CONSTANT M-BEG 1>
<CONSTANT M-END 6>
<CONSTANT M-ENTER 2>
<CONSTANT M-LOOK 3>
<CONSTANT M-FLASH 4>
<CONSTANT M-OBJDESC 5>

<ROUTINE GO () 
	 <PUTB ,P-LEXV 0 59>
;"put interrupts on clock chain"
	 <ENABLE <QUEUE I-HOUSEWRECK 20>>
	 <ENABLE <QUEUE I-THING 21>>
	 <ENABLE <QUEUE I-VOGONS 50>>	 
;"set up and go"
	 <SETG WINNER ,PROTAGONIST>
	 <SETG PLAYER ,PROTAGONIST>
	 <SETG HERE ,BEDROOM>
	 <SETG IDENTITY-FLAG ,ARTHUR>
	 <MOVE ,ARTHUR ,GLOBAL-OBJECTS>
	 <SETG LYING-DOWN T>
	 <MOVE ,PROTAGONIST ,BED>
	 <V-VERSION>
	 <CRLF>
	 <TELL
"You wake up. The room is spinning very gently round your head. Or at least
it would be if you could see it which you can't." CR CR>
	 <V-LOOK>
	 <MAIN-LOOP>
	 <AGAIN>>    

<ROUTINE MAIN-LOOP ("AUX" ICNT OCNT NUM CNT OBJ TBL V PTBL OBJ1 TMP)
   <REPEAT ()
     <SET CNT 0>
     <SET OBJ <>>
     <SET PTBL T>
     <COND (<SETG P-WON <PARSER>>
	    <SET ICNT <GET ,P-PRSI ,P-MATCHLEN>>
	    <SET OCNT <GET ,P-PRSO ,P-MATCHLEN>>
	    <COND (<AND ,P-IT-OBJECT <ACCESSIBLE? ,P-IT-OBJECT>>
		   <SET TMP <>>
		   <REPEAT ()
			   <COND (<G? <SET CNT <+ .CNT 1>> .ICNT>
				  <RETURN>)
				 (T
				  <COND (<EQUAL? <GET ,P-PRSI .CNT> ,IT>
					 <PUT ,P-PRSI .CNT ,P-IT-OBJECT>
					 <SET TMP T>
					 <RETURN>)>)>>
		   <COND (<NOT .TMP>
			  <SET CNT 0>
			  <REPEAT ()
			   <COND (<G? <SET CNT <+ .CNT 1>> .OCNT>
				  <RETURN>)
				 (T
				  <COND (<EQUAL? <GET ,P-PRSO .CNT> ,IT>
					 <PUT ,P-PRSO .CNT ,P-IT-OBJECT>
					 <RETURN>)>)>>)>
		   <SET CNT 0>)>
	    <SET NUM
		 <COND (<0? .OCNT> .OCNT)
		       (<G? .OCNT 1>
			<SET TBL ,P-PRSO>
			<COND (<0? .ICNT> <SET OBJ <>>)
			      (T <SET OBJ <GET ,P-PRSI 1>>)>
			.OCNT)
		       (<G? .ICNT 1>
			<SET PTBL <>>
			<SET TBL ,P-PRSI>
			<SET OBJ <GET ,P-PRSO 1>>
			.ICNT)
		       (T 1)>>
	    <COND (<AND <NOT .OBJ>
			<1? .ICNT>>
		   <SET OBJ <GET ,P-PRSI 1>>)>
	    <COND (<EQUAL? ,PRSA ,V?WALK> <SET V <PERFORM ,PRSA ,PRSO>>)
		  (<0? .NUM>
		   <COND (<0? <BAND <GETB ,P-SYNTAX ,P-SBITS> ,P-SONUMS>>
			  <SET V <PERFORM ,PRSA>>
			  <SETG PRSO <>>)
			 (<NOT ,LIT>
			  <TELL ,TOO-DARK CR>
			  <FUCKING-CLEAR>)
			 (T
			  <TELL "There isn't anything to ">
			  <SET TMP <GET ,P-ITBL ,P-VERBN>>
			  <COND (<VERB? TELL>
				 <TELL "talk to">)
				(<OR ,P-OFLAG ,P-MERGED>
				 <PRINTB <GET .TMP 0>>)
				(T
				 <WORD-PRINT <GETB .TMP 2>
					     <GETB .TMP 3>>)>
			  <TELL "!" CR>
			  <SET V <>>
			  <FUCKING-CLEAR>)>)
		  (T
		   <SETG P-NOT-HERE 0>
		   <SETG P-MULT <>>
		   <COND (<G? .NUM 1> <SETG P-MULT T>)>
		   <SET TMP <>>
		   <REPEAT ()
			   <COND (<G? <SET CNT <+ .CNT 1>> .NUM>
				  <COND (<G? ,P-NOT-HERE 0>
					 <TELL "The ">
					 <COND (<NOT <EQUAL? ,P-NOT-HERE .NUM>>
						<TELL "other ">)>
					 <TELL "object">
					 <COND (<NOT <EQUAL? ,P-NOT-HERE 1>>
						<TELL "s">)>
					 <TELL " that you mentioned ">
					 <COND (<NOT <EQUAL? ,P-NOT-HERE 1>>
						<TELL "are">)
					       (T <TELL "is">)>
					 <TELL "n't here." CR>)
					(<NOT .TMP>
					 <TELL ,REFERRING CR>)>
				  <RETURN>)
				 (T
				  <COND (.PTBL <SET OBJ1 <GET ,P-PRSO .CNT>>)
					(T <SET OBJ1 <GET ,P-PRSI .CNT>>)>
				  <SETG PRSO <COND (.PTBL .OBJ1) (T .OBJ)>>
				  <SETG PRSI <COND (.PTBL .OBJ) (T .OBJ1)>>
				  <COND (<OR <G? .NUM 1>
					     <EQUAL? <GET <GET ,P-ITBL ,P-NC1>
							  0>
						     ,W?ALL>>
					 <COND (<EQUAL? .OBJ1
							,NOT-HERE-OBJECT>
						<SETG P-NOT-HERE
						      <+ ,P-NOT-HERE 1>>
						<AGAIN>)
					       (<AND <EQUAL? ,P-GETFLAGS
							     ,P-ALL>
						     <VERB? TAKE PICK-UP>
						     <OR <AND <NOT <EQUAL?
							       <LOC .OBJ1>
							       ,WINNER
							       ,HERE
							       ,PRSI>>
							      <NOT <FSET?
								  <LOC .OBJ1>
								 ,SURFACEBIT>>>
							 <AND <NOT <FSET? .OBJ1
							             ,TAKEBIT>>
							     <NOT <FSET? .OBJ1
							       ,TRYTAKEBIT>>>>>
						<AGAIN>)
					       (<AND <VERB? TAKE PICK-UP>
						     ,PRSI
						     <NOT <IN? ,PRSO ,PRSI>>>
						<AGAIN>)
					       (<AND <EQUAL? ,P-GETFLAGS
							     ,P-ALL>
						     <VERB? DROP>
						      <NOT <IN? .OBJ1 ,WINNER>>
						      ;"next frob semied by JW"
						      ;<NOT <IN? ,P-IT-OBJECT
								,WINNER>>>
						<AGAIN>)
					       (<AND <EQUAL? ,P-GETFLAGS
							     ,P-ALL>
						     ,PRSI
						     <==? ,PRSO ,PRSI>>
						<AGAIN>)
					       (<AND <EQUAL? ,P-GETFLAGS
							     ,P-ALL>
						     <VERB? PUT>
						     <HELD? ,PRSO ,PRSI>>
						<AGAIN>)
					       (<NOT <ACCESSIBLE? .OBJ1>>
						<AGAIN>)
					       (T
						<COND (<EQUAL? .OBJ1 ,IT>
						       <PRINTD ,P-IT-OBJECT>)
						      (<TEA-PRINT .OBJ1>
						       <PRINTD .OBJ1>)>
					        <COND (<TEA-PRINT .OBJ1>
						       <TELL ": ">)>)>)>
				  <SET TMP T>
				  <SET V <PERFORM ,PRSA ,PRSO ,PRSI>>
				  <COND (<EQUAL? .V ,M-FATAL> <RETURN>)>)>>)>
	    <COND (<NOT <EQUAL? .V ,M-FATAL>>
		   <COND (<VERB? TELL BRIEF SUPER-BRIEF VERBOSE
				 SAVE VERSION RESTORE SCRIPT UNSCRIPT>
			  T)
			 (T
			  <SET V <APPLY <GETP <LOC ,WINNER> ,P?ACTION>
					,M-END>>)>)>
	    <COND (<VERB? AGAIN SAVE RESTORE SCRIPT UNSCRIPT
			  VERBOSE BRIEF SUPER-BRIEF>
		   T)
		  (,P-OFLAG T)
		  (T
		   <SETG L-PRSA ,PRSA>
		   <SETG L-PRSO ,PRSO>
		   <SETG L-PRSI ,PRSI>)>
	    <COND (,DONT-FLAG
		   <SETG L-DONT-FLAG T>)
		  (T
		   <SETG L-DONT-FLAG <>>)>
	    <COND (,IN-FRONT-FLAG
		   <SETG L-FRONT-FLAG T>)
		  (T
		   <SETG L-FRONT-FLAG <>>)>
	    <COND (<EQUAL? .V ,M-FATAL>
		   <SETG P-CONT <>>)>)
	   (T
	    <SETG P-CONT <>>)>
     <COND (,P-WON
	    <COND (<VERB? TELL BRIEF SUPER-BRIEF VERBOSE VERSION QUIT SCORE
			  SAVE RESTORE SCRIPT UNSCRIPT FOOTNOTE HELP RESTART>
		   T)
		  (<AND <VERB? AGAIN>
			<OR <EQUAL? ,L-PRSA ,V?FIND ,V?FOLLOW ,V?CALL>
			    <EQUAL? ,L-PRSA ,V?WHAT ,V?WHERE ,V?WAIT-FOR>
			    <EQUAL? ,L-PRSA ,V?WHO ,V?WALK-TO ,V?WHAT-ABOUT>
			    <EQUAL? ,L-PRSA ,V?ASK-ABOUT ,V?ASK-FOR ,V?I-AM>
			    <EQUAL? ,L-PRSA ,V?MY-NAME ,V?CARVE ,V?SCORE>
			    <EQUAL? ,L-PRSA ,V?VERSION ,V?FOOTNOTE ,V?HELP>>>
		   T)
		  (<AND <VERB? WAIT>
			,DONT-FLAG>
		   T)
		  (<AND <VERB? AGAIN>
			<EQUAL? ,L-PRSA ,V?WAIT>
			,L-DONT-FLAG>
		   T)
		  (T
		   <SET V <CLOCKER>>)>
	    <SETG PRSA <>>
	    <SETG PRSO <>>
	    <SETG PRSI <>>)
	   (<AND <G? <GETB ,P-LEXV ,P-LEXWORDS> 3>
		 <NOT ,CARELESS-WORDS-FLAG>
		 ,EARTH-DEMOLISHED
		 <SAVE-INPUT ,FIRST-BUFFER>>
	    <SETG CARELESS-WORDS-FLAG T>
	    <ENABLE <QUEUE I-CARELESS-WORDS 3>>)>>>

<GLOBAL FIRST-BUFFER <ITABLE BYTE 100>>

<ROUTINE SAVE-INPUT (TBL "AUX" (OFFS 0) CNT TMP)
	 <SET CNT <+ <GETB ,P-LEXV <SET TMP <* 4 ,P-INPUT-WORDS>>>
		     <GETB ,P-LEXV <+ .TMP 1>>>>
	 <COND (<EQUAL? .CNT 0> ;"failed"
		<RFALSE>)>
	 <SET CNT <- .CNT 1>>
	 <REPEAT ()
		 <COND (<EQUAL? .OFFS .CNT>
			<PUTB .TBL .OFFS 0>
			<RETURN>)
		       (T
			<PUTB .TBL .OFFS <GETB ,P-INBUF <+ .OFFS 1>>>)>
		 <SET OFFS <+ .OFFS 1>>>
	 <RTRUE>>

<ROUTINE RESTORE-INPUT (TBL "AUX" CHR)
	 <REPEAT ()
		 <COND (<EQUAL? <SET CHR <GETB .TBL 0>> 0>
			<RETURN>)
		       (T
			<PRINTC .CHR>
			<SET TBL <REST .TBL>>)>>>

<GLOBAL L-PRSA <>>  

<GLOBAL L-PRSO <>>  

<GLOBAL L-PRSI <>>  

<GLOBAL L-DONT-FLAG <>>

<GLOBAL L-FRONT-FLAG <>>

<GLOBAL P-MULT <>>

<GLOBAL P-NOT-HERE 0>


<ROUTINE FAKE-ORPHAN ("AUX" TMP)
	 <ORPHAN ,P-SYNTAX <>>
	 <TELL "Be specific: what object do">
	 <COND (,DONT-FLAG
		<TELL "n't">)>
	 <TELL " you want to ">
	 <SET TMP <GET ,P-OTBL ,P-VERBN>>
	 <COND (<EQUAL? .TMP 0>
		<TELL "tell">)
	       (<0? <GETB ,P-VTBL 2>>
		<PRINTB <GET .TMP 0>>)
	       (T
		<WORD-PRINT <GETB .TMP 2> <GETB .TMP 3>>
		<PUTB ,P-VTBL 2 0>)>
	 <SETG P-OFLAG T>
	 <SETG P-WON <>>
	 <PREP-PRINT
	     <GETB ,P-SYNTAX ,P-SPREP1>>
	 <TELL "?" CR>>

<ROUTINE PERFORM (A "OPTIONAL" (O <>) (I <>) "AUX" V OA OO OI)
	;<COND (,DEBUG
	       <TELL "[Perform: ">
	       %<COND (<GASSIGNED? PREDGEN> '<TELL N .A>)
		      (T '<PRINC <NTH ,ACTIONS <+ <* .A 2> 1>>>)>
	       <COND (<AND .O <NOT <EQUAL? .A ,V?WALK>>>
		      <TELL " / PRSO = " D .O>)>
	       <COND (.I <TELL " / PRSI = " D .I>)>
	       <TELL "]" CR>
	       <COND (,IN-FRONT-FLAG
		      <TELL "[IN-FRONT-FLAG is set]" CR>)>)>
	<SET OA ,PRSA>
	<SET OO ,PRSO>
	<SET OI ,PRSI>
	<SETG PRSA .A>
	<COND (<EQUAL? ,IT .I .O>
	       <COND (<NOT .I>
		      <FAKE-ORPHAN>)
		     (T
		      <TELL ,REFERRING CR>)>
	       <RFATAL>)>
	<SETG PRSO .O>
	<COND (<AND ,PRSO
		    <NOT <VERB? WALK>>
		    <NOT <PRSO? ,NOT-HERE-OBJECT>>>
	       <SETG P-IT-OBJECT ,PRSO>)>
	<SETG PRSI .I>
	;<COND (<NOT <EQUAL? .A ,V?AGAIN>>
	       <SETG L-PRSA .A>
	       <COND (<EQUAL? .A ,V?WALK> <SETG L-PRSO <>>)
		     (T <SETG L-PRSO .O>)>
	       <SETG L-PRSI .I>)>
	<COND (<AND <NOT <EQUAL? .A ,V?WALK>>
		    <EQUAL? ,NOT-HERE-OBJECT ,PRSO ,PRSI>
		    <SET V <D-APPLY "Not Here" ,NOT-HERE-OBJECT-F>>>
	       <SETG P-WON <>>
	       .V)
	      (T
	       <SET O ,PRSO>
	       <SET I ,PRSI>
	       <COND (<SET V <D-APPLY "Actor" <GETP ,WINNER ,P?ACTION>>>
		      .V)
		     (<AND ,DONT-FLAG <SET V <DONT-F>>>
		      .V)
		     (<SET V <D-APPLY "M-Beg" <GETP <LOC ,WINNER> ,P?ACTION>
				      ,M-BEG>>
		      .V)
		     (<SET V <D-APPLY "Preaction" <GET ,PREACTIONS .A>>>
		      .V)
		     (<AND .I <SET V <D-APPLY "PRSI" <GETP .I ,P?ACTION>>>>
		      .V)
		     ;(<AND .O
			   <NOT <EQUAL? .A ,V?WALK>>
			   <LOC .O>
			   <GETP <LOC .O> ,P?CONTFCN>
			   <SET V <D-APPLY "Cont" <GETP <LOC .O> ,P?CONTFCN>>>>
		      .V) 
		     (<AND .O
			   <NOT <EQUAL? .A ,V?WALK>>
			   <SET V <D-APPLY "PRSO" <GETP .O ,P?ACTION>>>>
		      .V)
		     (<SET V <D-APPLY <> <GET ,ACTIONS .A>>>
		      .V)>)>
	<SETG PRSA .OA>
	<SETG PRSO .OO>
	<SETG PRSI .OI>
	.V>

<ROUTINE D-APPLY (STR FCN "OPTIONAL" (FOO <>) "AUX" RES)
	<COND (<NOT .FCN> <>)
	      (T
	       ;<COND (,DEBUG
		      <COND (<NOT .STR>
			     <TELL CR "  Default ->" CR>)
			    (T <TELL CR "  " .STR " -> ">)>)>
	       <SET RES
		    <COND (.FOO <APPLY .FCN .FOO>)
			  (T <APPLY .FCN>)>>
	       ;<COND (<AND ,DEBUG .STR>
		      <COND (<EQUAL? .RES ,M-FATAL>
			     <TELL "Fatal" CR>)
			    (<NOT .RES>
			     <TELL "Not handled">)
			    (T <TELL "Handled" CR>)>)>
	       .RES)>>


;"former CLOCK.ZIL stuff"

<GLOBAL CLOCK-WAIT <>>

<GLOBAL C-TABLE %<COND (<GASSIGNED? PREDGEN>
			'<ITABLE NONE 105>)
		       (T
			'<ITABLE NONE 210>)>>

<CONSTANT C-TABLELEN 210>

<GLOBAL C-INTS 210>

<CONSTANT C-INTLEN 6>

<CONSTANT C-ENABLED? 0>

<CONSTANT C-TICK 1>

<CONSTANT C-RTN 2>

<ROUTINE QUEUE (RTN TICK "AUX" CINT)
	 <PUT <SET CINT <INT .RTN>> ,C-TICK .TICK>
	 .CINT>

<ROUTINE INT (RTN "OPTIONAL" E C INT)
	 <SET E <REST ,C-TABLE ,C-TABLELEN>>
	 <SET C <REST ,C-TABLE ,C-INTS>>
	 <REPEAT ()
		 <COND (<EQUAL? .C .E>
			<SETG C-INTS <- ,C-INTS ,C-INTLEN>>
			<SET INT <REST ,C-TABLE ,C-INTS>>
			<PUT .INT ,C-RTN .RTN>
			<RETURN .INT>)
		       (<EQUAL? <GET .C ,C-RTN> .RTN> <RETURN .C>)>
		 <SET C <REST .C ,C-INTLEN>>>>

<ROUTINE CLOCKER ("AUX" C E TICK (FLG <>))
	 <COND (,CLOCK-WAIT <SETG CLOCK-WAIT <>> <RFALSE>)>
	 <SET C <REST ,C-TABLE <COND (,P-WON ,C-INTS)>>>
	 <SET E <REST ,C-TABLE ,C-TABLELEN>>
	 <REPEAT ()
		 <COND (<EQUAL? .C .E>
			<SETG MOVES <+ ,MOVES 1>>
			<RETURN .FLG>)
		       (<NOT <0? <GET .C ,C-ENABLED?>>>
			<SET TICK <GET .C ,C-TICK>>
			<COND (<0? .TICK>)
			      (T
			       <PUT .C ,C-TICK <- .TICK 1>>
			       <COND (<AND <NOT <G? .TICK 1>>
					   <APPLY <GET .C ,C-RTN>>>
				      <SET FLG T>)>)>)>
		 <SET C <REST .C ,C-INTLEN>>>>