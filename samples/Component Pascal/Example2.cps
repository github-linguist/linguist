MODULE ObxFact;
(**
    project         = "BlackBox"
    organization    = "www.oberon.ch"
    contributors    = "Oberon microsystems"
    version         = "System/Rsrc/About"
    copyright       = "System/Rsrc/About"
    license         = "Docu/BB-License"
    changes         = ""
    issues          = ""

**)

IMPORT
    Stores, Models, TextModels, TextControllers, Integers;

PROCEDURE Read(r: TextModels.Reader; VAR x: Integers.Integer);
    VAR i, len, beg: INTEGER; ch: CHAR; buf: POINTER TO ARRAY OF CHAR;
BEGIN
    r.ReadChar(ch);
    WHILE ~r.eot & (ch <= " ") DO r.ReadChar(ch) END;
    ASSERT(~r.eot & (((ch >= "0") & (ch <= "9")) OR (ch = "-")));
    beg := r.Pos() - 1; len := 0;
    REPEAT INC(len); r.ReadChar(ch) UNTIL r.eot OR (ch < "0") OR (ch > "9");
    NEW(buf, len + 1);
    i := 0; r.SetPos(beg);
    REPEAT r.ReadChar(buf[i]); INC(i) UNTIL i = len;
    buf[i] := 0X;
    Integers.ConvertFromString(buf^, x)
END Read;

PROCEDURE Write(w: TextModels.Writer; x: Integers.Integer);
    VAR i: INTEGER;
BEGIN
    IF Integers.Sign(x) < 0 THEN w.WriteChar("-") END;
    i := Integers.Digits10Of(x);
    IF i # 0 THEN
        REPEAT DEC(i); w.WriteChar(Integers.ThisDigit10(x, i)) UNTIL i = 0
    ELSE w.WriteChar("0")
    END
END Write;

PROCEDURE Compute*;
    VAR beg, end, i, n: INTEGER; ch: CHAR;
        s: Stores.Operation;
        r: TextModels.Reader; w: TextModels.Writer; attr: TextModels.Attributes;
        c: TextControllers.Controller;
        x: Integers.Integer;
BEGIN
    c := TextControllers.Focus();
    IF (c # NIL) & c.HasSelection() THEN
        c.GetSelection(beg, end);
        r := c.text.NewReader(NIL); r.SetPos(beg); r.ReadChar(ch);
        WHILE ~r.eot & (beg < end) & (ch <= " ") DO r.ReadChar(ch); INC(beg) END;
        IF ~r.eot & (beg < end) THEN
            r.ReadPrev; Read(r, x);
            end := r.Pos(); r.ReadPrev; attr :=r.attr;
            IF (Integers.Sign(x) > 0) & (Integers.Compare(x, Integers.Long(MAX(LONGINT))) <= 0) THEN
                n := SHORT(Integers.Short(x)); i := 2; x := Integers.Long(1);
                WHILE i <= n DO x := Integers.Product(x, Integers.Long(i)); INC(i) END;
                Models.BeginScript(c.text, "computation", s);
                c.text.Delete(beg, end);
                w := c.text.NewWriter(NIL); w.SetPos(beg); w.SetAttr(attr);
                Write(w, x);
                Models.EndScript(c.text, s)
            END
        END
    END
END Compute;

END ObxFact.