ZDIOUT1 ; Experimental FileMan file output to host file
 ;---------------------------------------------------------------------------
 ; Copyright 2011 The Open Source Electronic Health Record Agent
 ;
 ; Licensed under the Apache License, Version 2.0 (the "License");
 ; you may not use this file except in compliance with the License.
 ; You may obtain a copy of the License at
 ;
 ;     http://www.apache.org/licenses/LICENSE-2.0
 ;
 ; Unless required by applicable law or agreed to in writing, software
 ; distributed under the License is distributed on an "AS IS" BASIS,
 ; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ; See the License for the specific language governing permissions and
 ; limitations under the License.
 ;---------------------------------------------------------------------------
 N  W "Experimental FileMan file output to host file",!
 D ASKFILE Q:FILE["^"
 D ASKDIR Q:DIR["^"
 D SAVEFILE(FILE,DIR)
 Q
SAVEFILE(FILE,DIR) ; Save FILE to given host directory
 I '$$SLASH(DIR) Q
 N FGR S FGR=$$FGR(FILE) Q:'$$CHECK(FGR,"Not a valid file number: "_FILE)
 S IO=DIR_$P($E(FGR,2,$L(FGR)),"(")_"+"_$$FILENAME(FILE,FGR)_".txt"
 W IO,!
 C IO O IO:("WNS"):1 E  U $P W "Cannot open """_IO_""" for write!",! Q
 D FILE("",FILE,FGR)
 C IO
 Q
PRNFILE(FILE,IO) ; Print FILE, optionally to IO device
 S:'$D(IO) IO=$P
 N FGR S FGR=$$FGR(FILE) Q:'$$CHECK(FGR,"Not a valid file number: "_FILE)
 D FILE("",FILE,FGR)
 Q
PRNENTRY(FILE,I,IO) ; Print FILE record #I, optionally to IO device
 S:'$D(IO) IO=$P
 N FGR S FGR=$$FGR(FILE) Q:'$$CHECK(FGR,"Not a valid file number: "_FILE)
 N DD D DDCR(FILE,.DD)
 D ENTITY("",FILE,.DD,$$EGR(FGR,I))
 Q
PRNDD(FILE,IO) ; Print DD for FILE, optionally to IO device
 S:'$D(IO) IO=$P
 ; DD(FILE) is a file#0 whose entries define fields of FILE
 N FGR S FGR=$NA(^DD(FILE))
 I '$D(@FGR) W "Not a valid file number: "_FILE,! Q
 D FILE("",0,FGR)
 Q
 ;---------------------------------------------------------------------------
 ; Private implementation entry points below.
 ; References cite the VA FileMan 22.0 Programmer Manual.
 ;
ASKFILE ; Ask for file number
 R !,"File#: ",FILE G:FILE="" ASKFILE Q:FILE["^"  S FILE=+FILE
 S FGR=$$FGR(FILE)
 I '$$CHECK(FGR," (Not a valid file number)") G ASKFILE
 W "  ",$$FILENAME(FILE,FGR)
 Q
ASKDIR ; Ask for host dir
 R !,!,"Host output directory: ",DIR,! Q:DIR["^"   G:'$$SLASH(DIR) ASKDIR
 Q
SLASH(DIR) ; Validate trailing slash
 I $E(DIR,$L(DIR))?1(1"/",1"\") Q 1
 E  U $P W "Output directory must end in a slash!" Q 0
FGR(FILE) ; Get FILE Global Root
 Q $$ROOT^DILFD(FILE,"",1)
EGR(FGR,I) ; Get ENTRY Global Root
 Q $NA(@FGR@(I))
CHECK(V,MSG) ; Validate non-empty value
 I V="" W MSG,! Q 0
 Q 1
DDCR(FILE,DD) ; X-ref global subscript location to DD field
 ; The DD field definition 0-node has ^-pieces "^^^S;P^" where
 ; "S;P" is the node Subscript and Piece within the node value (14.9.2).
 N F S F="" F  S F=$O(^DD(FILE,F)) Q:F=""  D:+F
 . N F4,S,P S F4=$P(^DD(FILE,F,0),"^",4),S=$P(F4,";",1),P=$P(F4,";",2) Q:S=" "
 . S DD(S,F)=P ; Subscript S contains field F at piece P
 Q
FILE(D,FILE,FGR) ; Write all entries in a file
 ; TODO: Sort entries by .01 or KEY to ensure consistent order
 N DD D DDCR(FILE,.DD)
 N I S I="" F  S I=$O(@FGR@(I)) Q:I=""  D
 . I +I D
 . . D ENTITY(D,FILE,.DD,$$EGR(FGR,I))
 . E  D ; TODO: Handle known non-entry subscripts such as "B"
 . . D SUBS(D,$$EGR(FGR,I),I)
 Q
WP(D,FGR) ; Write a word-processing value
 ; A word processing field is actually a file in which each entry has a
 ; .01 field containing the line of text, and the type of the field has "W".
 U IO W D,";",$$VALUE(@FGR@(0)),! ; TODO: Preserve date from ^(0)
 N I S I="" F  S I=$O(@FGR@(I)) Q:I=""  D:+I ; TODO: Other subscripts?
 . U IO W D,$$VALUE(@FGR@(I,0)),!
 U IO W D,";",!
 Q
ENTITY(D,FILE,DD,EGR) ; Write a file entry
 U IO W D,"ENTITY"_$C(9)_";;"_$$FILENAME(FILE,FGR)_"^"_$S(FILE=0:"",1:FILE)_" ;"_EGR,!
 U IO W D_$C(9)_";",!
 ; Add key tag with field .01 value (14.9.2).
 ; TODO: Use indexing cross-references or KEY file entries for key tags?
 ; TODO: Escape key values, handle pointers?
 U IO W D,"KA"_$C(9)_";;",$P(@EGR@(0),"^"),!
 U IO W D_$C(9)_";",!
 N S S S="" F  S S=$O(@EGR@(S)) Q:S=""  D ; Find DD fields at S.
 . I $D(DD(S))<10 D ; TODO: Field defs like "DEL" not in ^DD(0)
 . . D SUBS(D,$NA(@EGR@(S)),S)
 . N F S F="" F  S F=$O(DD(S,F)) Q:F=""  D
 . . D FIELD(D,FILE,F,$NA(@EGR@(S)),DD(S,F))
 Q
 ;
SUBS(D,G,S) ; Write an extraneous subscript
 U IO W D,"SUBS"_$C(9)_";;"_S,!
 I $D(@G)#10 U IO W D_$C(9),$$VALUE(@G),!
 I $D(@G)\10 U IO W D_$C(9),"; OMITTED CHILDREN",!
 U IO W D_$C(9),";",!
 Q
FIELD(D,FILE,F,EGRF,P) ; Write a field
 ; The DD field definition 0-node has ^-pieces "NAME^TYPE^" (14.9.2).
 N FD S FD=^DD(FILE,F,0)
 N NAME S NAME=$P(FD,"^",1)
 N TYPE S TYPE=$P(FD,"^",2)
 ; TYPE starts with a subfile number if the field is a multiple (14.9.2)
 N SUBFILE S SUBFILE=+TYPE
 I SUBFILE D
 . D FIELDSUB
 E  D
 . D FIELDONE
 Q
FIELDTAG ; Write tag for a field
 U IO W D,"F"_$TR(F,".","P")_$C(9)_";;"_NAME_"^"_F_" ;"_TYPE,!
 Q
FIELDSUB ; Write a multiple-valued field
 D FIELDTAG
 I $D(@EGRF)#10 U IO W D_$C(9),"; OMITTED SELF",!
 ; Word-processing values are files whose .01 field type has "W".
 I $P($G(^DD(SUBFILE,.01,0)),"^",2)["W" D
 . D WP(D_$C(9),EGRF)
 E  D
 . D FILE(D_$C(9),SUBFILE,EGRF) U IO W D_$C(9),";",!
 Q
FIELDONE ; Write a single-valued field
 N V S V=$$FIELDVAL(EGRF,P) Q:V=""
 N EV ; Some TYPEs have an external-format value
 N T S T=TYPE
 I T["F" S TYPE=TYPE_";"_"Free Text"
 I T["N" S TYPE=TYPE_";"_"Numeric"
 I T["K" S TYPE=TYPE_";"_"MUMPS Code"
 I T["P" S TYPE=TYPE_";"_"Pointer",EV=1
 I T["V" S TYPE=TYPE_";"_"Variable Pointer",EV=1
 I T["S" S TYPE=TYPE_";"_"Set of Codes",EV=1
 I T["D" S TYPE=TYPE_";"_"Date",EV=1
 I $D(EV) S V=V_"^"_$$EXTERNAL^DILFD(FILE,F,"",V)
 D FIELDTAG
 U IO W D_$C(9),$$VALUE(V),!
 I $D(@EGRF)\10 U IO W D_$C(9),"; OMITTED CHILDREN",!
 U IO W D_$C(9),";",!
 Q
FIELDVAL(EGRF,P) ; Extract piece P of node value holding field
 I +P Q $P(@EGRF,"^",P)
 I $E(P,1)="E" Q $E(@EGRF,$P($E(P,2,$L(P)),",",1),$P(P,",",2))
 Q ";UNKNOWN ""GLOBAL SUBSCRIPT LOCATION"" PIECE """_P_""""
 ;
FILENAME(FILE,FGR) ; Lookup the name of given FILE# (or subfile#)
 I FILE=0 Q $P(@FGR@(0),"^") ; DD
 Q $O(^DD(FILE,0,"NM","")) ; TODO: Reliable?  Any documented API?
VALUE(V) ; Write value line to output
 ; TODO: If value starts in one of " $ ; or contains non-printing
 ; characters then it must be escaped for evaluation on RHS of SET.
 ; TODO: Caller must define indentation level with a comment if
 ; the first character of the first value is a tab or space.
 Q V
