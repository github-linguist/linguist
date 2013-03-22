PXAI ;ISL/JVS,ISA/KWP,ESW - PCE DRIVING RTN FOR 'DATA2PCE' API ;6/20/03 11:15am
 ;;1.0;PCE PATIENT CARE ENCOUNTER;**15,74,69,102,111,112,130,164,168**;Aug 12, 1996;Build 14
 Q
 ;
 ;+  1       2       3        4        5       6      7      8       9
DATA2PCE(PXADATA,PXAPKG,PXASOURC,PXAVISIT,PXAUSER,PXANOT,ERRRET,PXAPREDT,PXAPROB,PXACCNT) ;+API to pass data for add/edit/delete to PCE.
 ;+  PXADATA  (required)
 ;+  PXAPKG   (required)
 ;+  PXASOURC (required)
 ;+  PXAVISIT (optional) is pointer to a visit for which the data is to
 ;+        be related.  If the visit is not known then there must be
 ;+        the ENCOUNTER nodes needed to lookup/create the visit.
 ;+  PXAUSER  (optional) this is a pointer to the user adding the data.
 ;+  PXANOT   (optional) set to 1 if errors are to be displayed to the screen should only be set while writing and debugging the initial code.
 ;+  ERRRET   (optional) passed by reference.  If present will return PXKERROR
 ;+                      array elements to the caller.
 ;+  PXAPREDT  (optional) Set to 1 if you want to edit the Primary Provider
 ;+            only use if for the moment that editing is being done. (dangerous)
 ;+  PXAPROB   (optional) A dotted variable name. When errors and
 ;+             warnings occur, They will be passed back in the form
 ;+            of an array with the general description of the problem.
 ;+ IF ERROR1 - (GENERAL ERRORS)
 ;+      PXAPROB($J,SUBSCRIPT,"ERROR1",PASSED IN 'FILE',PASSED IN FIELD,
 ;+              SUBSCRIPT FROM PXADATA)
 ;+      PXAPROB(23432234,2,"ERROR1","PROVIDER","NAME",7)="BECAUSE..."
 ;+ IF WARNING2 - (GENERAL WARNINGS)
 ;+      PXAPROB($J,SUBSCRIPT,"WARNING2",PASSED IN 'FILE',PASSED IN FIELD,
 ;+              SUBSCRIPT FROM PXADATA)
 ;+      PXAPROB(23432234,3,"WARNING2","PROCEDURE","QTY",3)="BECAUSE..."
 ;+ IF WARNING3 - (WARNINGS FOR SERVICE CONNECTION)
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"AO")=REASON
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"EC")=REASON
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"IR")=REASON
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"SC")=REASON
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"MST")=REASON
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"HNC")=REASON
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"CV")=REASON
 ;+      PXAPROB($J,1,"WARNING3","ENCOUNTER",1,"SHAD")=REASON
 ;+ IF ERROR4 - (PROBLEM LIST ERRORS)
 ;+      PXAPROB($J,6,"ERROR4","PX/DL",(SUBSCRIPT FROM PXADATA))=REASON
 ;+ PXACCNT    (optional)  passed by reference.  Returns the PFSS Account Reference if known.
 ;              Returned as null if the PFSS Account Reference is located in the Order file(#100)
 ;+
 ;+
 ;+ Returns:
 ;+   1  if no errors and process completely
 ;+  -1  if errors occurred but processed completely as possible
 ;+  -2  if could not get a visit
 ;+  -3  if called incorrectly
 ;
NEW ;--NEW VARIABLES
 N NOVSIT,PXAK,DFN,PXAERRF,PXADEC,PXELAP,PXASUB
 N PATIENT,VALQUIET,PRIMFND
 K PXAERROR,PXKERROR,PXAERR,PRVDR
 S PXASUB=0,VALQUIET=1
 ; needs to look up if not passed. 
 I '$G(PXAVISIT),'$D(@PXADATA@("ENCOUNTER")) Q -3
 I $G(PXAUSER)<1 S PXAUSER=DUZ
 ;
 K ^TMP("PXK",$J),^TMP("DIERR",$J),^TMP("PXAIADDPRV",$J)
SOR ;--SOURCE
 I PXAPKG=+PXAPKG S PXAPKG=PXAPKG
 E  S PXAPKG=$$PKG2IEN^VSIT(PXAPKG)
 I PXASOURC=+PXASOURC S PXASOURC=PXASOURC
 E  S PXASOURC=$$SOURCE^PXAPIUTL(PXASOURC)
 ;
 D TMPSOURC^PXAPIUTL(PXASOURC) ;-SAVES & CREATES ^TMP("PXK",$J,"SOR")
VST ;--VISIT
 ;--KILL VISIT
 I $G(PXAVISIT) D VPTR^PXAIVSTV I $G(PXAERRF) D ERR Q -2
 D VST^PXAIVST
 I $G(PXAVISIT)<0 Q -2
 I $G(PXAERRF) D ERR K PXAERR Q -2
PRV ;--PROVIDER
 S PATIENT=$P($G(^AUPNVSIT(PXAVISIT,0)),"^",5)
 S (PXAK,PRIMFND)=0
 F  S PXAK=$O(@PXADATA@("PROVIDER",PXAK)) Q:(PRIMFND)!(PXAK="")  D
 .I $D(@PXADATA@("PROVIDER",PXAK,"PRIMARY")) D
 ..S PRIMFND=$G(@PXADATA@("PROVIDER",PXAK,"PRIMARY"))
 I 'PRIMFND D  ;Check for each provider's status as Primary or Secondary
 .S PXAK=0 F  S PXAK=$O(@PXADATA@("PROVIDER",PXAK)) Q:PXAK=""  D
 ..I '$D(@PXADATA@("PROVIDER",PXAK,"PRIMARY")) D PROVDRST
 S PXAK=0 F  S PXAK=$O(@PXADATA@("PROVIDER",PXAK)) Q:PXAK=""  D
 . D PRV^PXAIPRV I $G(PXAERRF) D ERR
 K PRI ;--FLAG FOR PRIMARY PROVIDER
 K PXAERR
POV ;--DIAGNOSIS
 S (PXAK,PRIMFND)=0
 F  S PXAK=$O(@PXADATA@("DX/PL",PXAK)) Q:(PXAK="")  D  Q:PRIMFND
 .I +$G(@PXADATA@("DX/PL",PXAK,"PRIMARY"))=1 D
 ..S PRIMFND=$G(@PXADATA@("DX/PL",PXAK,"DIAGNOSIS"))
 I $D(@PXADATA@("DX/PL")) D POVPRM(PXAVISIT,PRIMFND,.PXADATA) D
 .S PXAK=0 F  S PXAK=$O(@PXADATA@("DX/PL",PXAK))  Q:PXAK=""  D
 ..D POV^PXAIPOV I $G(PXAERRF) D ERR
 K PXAERR
 ;
CPT ;--PROCEDURE
 S PXAK=0 F  S PXAK=$O(@PXADATA@("PROCEDURE",PXAK))  Q:PXAK=""  D
 . D CPT^PXAICPT I $G(PXAERRF) D ERR
 K PXAERR
 ;
EDU ;--PATIENT EDUCATION
 S PXAK=0 F  S PXAK=$O(@PXADATA@("PATIENT ED",PXAK))  Q:PXAK=""  D
 . D EDU^PXAIPED I $G(PXAERRF) D ERR
 K PXAERR
 ;
EXAM ;--EXAMINATION
 S PXAK=0 F  S PXAK=$O(@PXADATA@("EXAM",PXAK))  Q:PXAK=""  D
 . D EXAM^PXAIXAM I $G(PXAERRF) D ERR
 K PXAERR
 ;
HF ;--HEALTH FACTOR
 S PXAK=0 F  S PXAK=$O(@PXADATA@("HEALTH FACTOR",PXAK))  Q:PXAK=""  D
 . D HF^PXAIHF I $G(PXAERRF) D ERR
 K PXAERR
 ;
IMM ;--IMMUNIZATION
 S PXAK=0 F  S PXAK=$O(@PXADATA@("IMMUNIZATION",PXAK))  Q:PXAK=""  D
 . D IMM^PXAIIMM I $G(PXAERRF) D ERR
 K PXAERR
 ;
SKIN ;--SKIN TEST
 S PXAK=0 F  S PXAK=$O(@PXADATA@("SKIN TEST",PXAK))  Q:PXAK=""  D
 . D SKIN^PXAISK I $G(PXAERRF) D ERR
 K PXAERR
 ;
 ;
 D OTHER^PXAIPRV
 ;
 ;
 I $D(^TMP("PXK",$J)) D
 . D EN1^PXKMAIN
 . M ERRRET=PXKERROR
 . D PRIM^PXAIPRV K PRVDR
 . D EVENT^PXKMAIN
 S PXACCNT=$P($G(^AUPNVSIT(PXAVISIT,0)),"^",26) ;PX*1.0*164 ;Sets the PFSS Account Reference, if any
 K ^TMP("PXK",$J),PXAERR,PXKERROR
 Q $S($G(PXAERRF):-1,1:1)
 ;
 ;
EXIT ;--EXIT AND CLEAN UP
 D EVENT^PXKMAIN
 K ^TMP("PXK",$J),PRVDR
 K PXAERR
 Q
 ;-----------------SUBROUTINES-----------------------
ERR ;
 ;
 ;
 I '$D(PXADI("DIALOG")) Q
 N NODE,SCREEN
 S PXAERR(1)=$G(PXADATA),PXAERR(2)=$G(PXAPKG),PXAERR(3)=$G(PXASOURC)
 S PXAERR(4)=$G(PXAVISIT),PXAERR(5)=$G(PXAUSER)_"  "_$P($G(^VA(200,PXAUSER,0)),"^",1)
 I $G(PXANOT)=1 D EXTERNAL
 E  D INTERNAL
 D ARRAY^PXAICPTV
 K PXADI("DIALOG")
 Q
 ;
EXTERNAL ;---SEND ERRORS TO SCREEN
 W !,"-----------------------------------------------------------------"
 D BLD^DIALOG($G(PXADI("DIALOG")),.PXAERR,"","SCREEN","F")
 D MSG^DIALOG("ESW","",50,10,"SCREEN")
 ;
 Q
INTERNAL ;---SET ERRORS TO GLOBAL ARRAY
 S NODE=PXADATA
 D BLD^DIALOG($G(PXADI("DIALOG")),.PXAERR,.PXAERR,NODE,"F")
 S NODE=$NA(@PXADATA@("DIERR",$J)) D MSG^DIALOG("ESW","",50,10,NODE)
 Q
 ;
PROVDRST ; Check provider status (Primary or Secondary)
 N PRVIEN,DETS,DIC,DR,DA,DIQ,PRI,PRVPRIM
 I $G(PXAK)="" QUIT
 S PRVIEN=0
 F  S PRVIEN=$O(^AUPNVPRV("AD",PXAVISIT,PRVIEN)) Q:PRVIEN=""  D
 .S DETS=$G(^AUPNVPRV(PRVIEN,0))
 .I $P(DETS,U)=$G(@PXADATA@("PROVIDER",PXAK,"NAME")) D
 ..S DIC=9000010.06,DR=.04,DA=PRVIEN
 ..S DIQ="PRVPRIM(",DIQ(0)="EI" D EN^DIQ1
 ..S PRI=$E($G(PRVPRIM(9000010.06,DA,DR,"E")),1,1)
 ..S @PXADATA@("PROVIDER",PXAK,"PRIMARY")=$S(PRI="P":1,1:0)
 Q
POVPRM(VISIT,PRIMFND,POVARR) ;
 N PRVIEN,DETS,STOP,LPXAK,ORDX,NDX,ORDXP
 S PRVIEN=0
 ;create array of existing DX; ORDX - pointer to ^ICD9(
 F  S PRVIEN=$O(^AUPNVPOV("AD",PXAVISIT,PRVIEN)) Q:PRVIEN=""  D
 .S DETS=$G(^AUPNVPOV(PRVIEN,0)),ORDX=$P(DETS,U)
 .S ORDX(ORDX)=PRVIEN I $P(DETS,U,12)="P" S ORDXP(ORDX)=""
 ; create array of passed DX; NDX - pointer to ^ICD9(
 S PXAK=0 F  S PXAK=$O(@POVARR@("DX/PL",PXAK)) Q:PXAK=""  D
 .S NDX=$G(@POVARR@("DX/PL",PXAK,"DIAGNOSIS")) S NDX(NDX)=PXAK
 ; force entry of originally primary diagnosis with "S" flag
 I PRIMFND S ORDX="" D
 .F  S ORDX=$O(ORDXP(ORDX)) Q:ORDX=""  I PRIMFND'=ORDX D
 ..I $D(NDX(ORDX)) S @POVARR@("DX/PL",NDX(ORDX),"PRIMARY")=0
 ..E  D
 ...S LPXAK=$O(@POVARR@("DX/PL",""),-1)
 ...S @POVARR@("DX/PL",LPXAK+1,"DIAGNOSIS")=ORDX
 ...S @POVARR@("DX/PL",LPXAK+1,"PRIMARY")=0
 Q
 ;
