IMPLEMENTATION MODULE HuffChan;

(*
 This module shows how to redefine standard IO file functions. It provides
 functions for reading and writing packed files opened in Raw mode.
*)

IMPORT IOChan, IOLink, ChanConsts, IOConsts, SYSTEM, Strings;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST
  rbldFrq = 512;	(* means: every 512 bytes rebuild table *)

TYPE
  charTap  = POINTER TO ARRAY [0..MAX(INTEGER)-1] OF CHAR;
  smbTp = POINTER TO smbT;

  smbT = RECORD			(* Huffman's tree *)
    ch			: CHAR;
    n			: CARDINAL; (* frequncy of char ch *)
    left,right,next	: smbTp;
  END;

  tblT = RECORD		(* bit sequence for code *)
    vl		: CARDINAL;	(* bit sequence *)
    cnt		: INTEGER;	(* it length *)
  END;

  lclDataT = RECORD	(* channel's local data *)
    tRoot 	: smbTp;
    htbl	: ARRAY [0..255] OF tblT;     (* code -> bit sequence table *)
    ftbl  	: ARRAY [0..255] OF CARDINAL; (* frequncey table *)
    wBf,rb1,rb2	: CARDINAL;
    wbc,rbc,smc	: INTEGER;
    chid	: IOChan.ChanId;
  END;
  lclDataTp = POINTER TO lclDataT;
  charp     = POINTER TO CHAR;

VAR
  did	: IOLink.DeviceId;
  ldt	: lclDataTp;


PROCEDURE Shf(a:CARDINAL; b : INTEGER) : CARDINAL; (* shl a,b (or shr) *)
BEGIN
  RETURN SYSTEM.CAST(CARDINAL,SYSTEM.SHIFT(SYSTEM.CAST(BITSET,a),b));
END Shf;

PROCEDURE wrDword(a:CARDINAL);	(* write 4 bytes to file *)
BEGIN
  IOChan.RawWrite(ldt^.chid,SYSTEM.ADR(a),4);
END wrDword;

PROCEDURE rdDword() : CARDINAL;  (* read 4 bytes from file *)
VAR
  a,z : CARDINAL;
BEGIN
  a:=0;
  IOChan.RawRead(ldt^.chid,SYSTEM.ADR(a),4,z);
  RETURN a;
END rdDword;

PROCEDURE wrSmb(ch : CHAR);	(* write bit sequence for code ch *)
VAR
  v,h : CARDINAL;
  b,c : INTEGER;
BEGIN
  WITH ldt^ DO
    v:=htbl[ORD(ch)].vl;
    c:=htbl[ORD(ch)].cnt;
    IF c+wbc<=32 THEN
      wBf:=Shf(wBf,c);
      wBf:=wBf+v;
      wbc:=wbc+c;
      IF wbc=32 THEN
	wrDword(wBf);
	wBf:=0; wbc:=0;
      END;
      RETURN;
    END;
    b:=c+wbc-32;
    h:=Shf(v,-b);
    wBf:=Shf(wBf,32-wbc)+h;
    wrDword(wBf);
    wBf:=v-Shf(h,b);
    wbc:=b;
  END;
END wrSmb;

PROCEDURE flush();	(* write data in buffer *)
BEGIN
  WITH ldt^ DO
    wBf:=Shf(wBf,32-wbc);
    wrDword(wBf);
  END;
END flush;

PROCEDURE getSym() : CHAR; (* find code for first bit sequence in buffer *)
VAR
  t,i : CARDINAL;
  b   : INTEGER;
BEGIN
  WITH ldt^ DO
    IF rbc<=32 THEN
      rb2:=rdDword();
      t:=Shf(rb2,-rbc);
      IF rbc=32 THEN t:=0; END;
      rb1:=rb1+t;
      rb2:=Shf(rb2,32-rbc);
      IF rbc=0 THEN rb2:=0; END;
      rbc:=rbc+32;
    END;
    FOR i:=0 TO 255 DO
      t:=Shf(rb1,htbl[i].cnt-32);
      IF t=htbl[i].vl THEN
	rb1:=Shf(rb1,htbl[i].cnt);
	b:=32-htbl[i].cnt;
	t:=Shf(rb2,-b);
	rb1:=rb1+t;
	rb2:=Shf(rb2,32-b);
	rbc:=rbc+b-32;
	RETURN CHR(i);
      END;
    END;
  END;
END getSym;

PROCEDURE Insert(s : smbTp); (* insert new character in Huffman's tree *)
VAR
  cr : smbTp;
BEGIN
  WITH ldt^ DO
    IF tRoot=NIL THEN
      cr:=tRoot;
      tRoot:=s;
      s^.next:=cr;
      RETURN;
    ELSIF tRoot^.n<=s^.n THEN
      cr:=tRoot;
      tRoot:=s;
      s^.next:=cr;
      RETURN;
    END;
    cr:=tRoot;
    WHILE (cr^.next<>NIL) & (cr^.next^.n>s^.n) DO
      cr:=cr^.next;
    END;
    s^.next:=cr^.next;
    cr^.next:=s;
  END;
END Insert;

PROCEDURE BuildTree(); (* build Huffman's tree *)
VAR
  cr,ocr,ncr : smbTp;
BEGIN
  WITH ldt^ DO
    LOOP
      ocr:=NIL; cr:=tRoot;
      WHILE cr^.next^.next<>NIL  DO
	ocr:=cr; cr:=cr^.next;
      END;
      NEW(ncr);
      ncr^.n:=cr^.n+cr^.next^.n;
      ncr^.left:=cr;
      ncr^.right:=cr^.next;
      IF ocr<>NIL THEN
	ocr^.next:=NIL;
	Insert(ncr);
      ELSE
	tRoot:=NIL;
	Insert(ncr);
	EXIT;
      END;
    END;
  END;
END BuildTree;

PROCEDURE BuildTable(cr: smbTp; vl,n: CARDINAL); (* build table: code -> bit sequence *)
BEGIN
  WITH ldt^ DO
    IF cr^.left=NIL THEN
      htbl[ORD(cr^.ch)].vl:=vl;
      htbl[ORD(cr^.ch)].cnt:=n;
      DISPOSE(cr);
      RETURN;
    END;
    vl:=vl*2;
    BuildTable(cr^.left,vl,n+1);
    BuildTable(cr^.right,vl+1,n+1);
    DISPOSE(cr);
  END;
END BuildTable;

PROCEDURE clcTab(); (* build code/bitseq. table from frequency table *)
VAR
  i : CARDINAL;
  s : smbTp;
BEGIN
  WITH ldt^ DO
    tRoot:=NIL;
    FOR i:=0 TO 255 DO
      NEW(s);
      s^.ch:=CHR(i);
      s^.n:=ftbl[i];
      s^.left:=NIL; s^.right:=NIL; s^.next:=NIL;
      Insert(s);
    END;
    BuildTree();
    BuildTable(tRoot,0,0);
  END;
END clcTab;

PROCEDURE iniHuf();
VAR
  i : CARDINAL;
BEGIN
  WITH ldt^ DO
    FOR i:=0 TO 255 DO
      ftbl[i]:=1;
    END;
    wBf:=0; wbc:=0; rb1:=0; rb2:=0; rbc:=0;
    smc:=0;
    clcTab();
  END;
END iniHuf;


PROCEDURE RawWrite(x: IOLink.DeviceTablePtr; buf: SYSTEM.ADDRESS;
		len: CARDINAL);
VAR
  i	: CARDINAL;
  ch	: CHAR;
  cht	: charTap;
BEGIN
  IF len = 0 THEN RETURN; END;
  ldt:=SYSTEM.CAST(lclDataTp,x^.cd);
  cht:=SYSTEM.CAST(charTap,buf);
  WITH ldt^ DO
    FOR i:=0 TO len-1 DO
      ch:=cht^[i];
      wrSmb(ch);
      IF ch = 377C THEN wrSmb(ch); END;
      ftbl[ORD(ch)]:=ftbl[ORD(ch)]+1; smc:=smc+1;
      IF smc=rbldFrq THEN
	clcTab();
	smc:=0;
      END;
    END;
  END;
  x^.result:=IOChan.ReadResult(ldt^.chid);
END RawWrite;

PROCEDURE RawRead(x: IOLink.DeviceTablePtr; buf: SYSTEM.ADDRESS;
		blen: CARDINAL; VAR len: CARDINAL);
VAR
  i	: CARDINAL;
  cht	: charTap;
  ch	: CHAR;
BEGIN
  ldt:=SYSTEM.CAST(lclDataTp,x^.cd);
  cht:=SYSTEM.CAST(charTap,buf);
  IF (blen=0) OR (x^.result<>IOConsts.allRight) THEN len:=0; RETURN; END;
  WITH ldt^ DO
    FOR i:=0 TO blen-1 DO
      ch:=getSym();
      IF ch = 377C THEN
	ch:=getSym();
	IF ch = 0C THEN
	  x^.result:=IOConsts.endOfInput;
	  len:=i; cht^[i]:=0C;
	  RETURN;
	END;
      END;
      cht^[i]:=ch;
      ftbl[ORD(ch)]:=ftbl[ORD(ch)]+1; smc:=smc+1;
      IF smc=rbldFrq THEN
	clcTab();
	smc:=0;
      END;
    END;
    len:=blen;
  END;
END RawRead;

PROCEDURE CreateAlias(VAR cid: ChanId; io: ChanId; VAR res: OpenResults);
VAR
  x	: IOLink.DeviceTablePtr;
BEGIN
  IOLink.MakeChan(did,cid);
  IF cid = IOChan.InvalidChan() THEN
    res:=ChanConsts.outOfChans
  ELSE
    NEW(ldt);
    IF ldt=NIL THEN
      IOLink.UnMakeChan(did,cid);
      res:=ChanConsts.outOfChans;
      RETURN;
    END;
    x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
    ldt^.chid:=io;
    x^.cd:=ldt;
    x^.doRawWrite:=RawWrite;
    x^.doRawRead:=RawRead;
    res:=ChanConsts.opened;
    iniHuf();
    x^.result:=IOConsts.allRight;
  END;
END CreateAlias;

PROCEDURE DeleteAlias(VAR cid: ChanId);
VAR
  x	: IOLink.DeviceTablePtr;
BEGIN
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  ldt:=x^.cd;
  IF ldt^.rbc=0 THEN
    wrSmb(377C);
    wrSmb(0C);
    flush();
  END;
  DISPOSE(ldt);
  IOLink.UnMakeChan(did,cid);
END DeleteAlias;

BEGIN
  IOLink.AllocateDeviceId(did);
END HuffChan.
