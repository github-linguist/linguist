
ds := DATASET([{'the fox; and the hen'}], {STRING100 line});

PATTERN ws            := PATTERN('[ \t\r\n]');
PATTERN Alpha         := PATTERN('[A-Za-z]');
PATTERN Word          := Alpha+;
PATTERN Article       := ['the', 'A'];
PATTERN JustAWord     := Word PENALTY(1);
PATTERN notHen        := VALIDATE(Word, MATCHTEXT != 'hen');
PATTERN NoHenWord     := notHen PENALTY(1);
RULE NounPhraseComp1  := JustAWord | Article ws Word;
RULE NounPhraseComp2  := NoHenWord | Article ws Word;

ps1 := { STRING100 out1 := MATCHTEXT(NounPhraseComp1) };
ps2 := { STRING100 out2 := MATCHTEXT(NounPhraseComp2) };

p1 := PARSE(ds, line, NounPhraseComp1, ps1, BEST, MANY, NOCASE);
p2 := PARSE(ds, line, NounPhraseComp2, ps2, BEST, MANY, NOCASE);

output(p1);
output(p2);

datafile := DATASET([{'Ge 34:2 And when Shechem the son of Hamor the Hivite, prince of the country, saw her, ' +
                      ' he took her, and lay with her, and defiled her.'}, 
                      {'Ge 36:10 These are the names of Esaus sons; Eliphaz the son of Adah the wife of Esau, ' +
                       ' Reuel the son of Bashemath the wife of Esau.'}], 
                    {STRING150 line}); 
PATTERN ws1           := [' ', '\t', ', '];
PATTERN ws2           := ws1 ws1?;
PATTERN article2      := ['A', 'The', 'Thou', 'a', 'the', 'thou'];
TOKEN   Name          := PATTERN('[A-Z][a-zA-Z]+');    
RULE    Namet         := name OPT(ws2 ['the', 'king of', 'prince of'] ws2 name);
PATTERN produced      := OPT(article2 ws2) ['begat', 'father of', 'mother of'];
PATTERN produced_by   := OPT(article2 ws2) ['son of', 'daughter of'];
PATTERN produces_with := OPT(article2 ws2) ['wife of'];
RULE    relationtype  := ( produced | produced_by | produces_with );
RULE    progeny       := namet ws2 relationtype ws2 namet;

results := RECORD
    STRING60 Le             :=  MATCHTEXT(Namet[1]);
    STRING60 Ri             :=  MATCHTEXT(Namet[2]);
    STRING30 RelationPhrase := MATCHTEXT(relationtype) 
END;
outfile1 := PARSE(datafile, line, progeny, results, SCAN ALL);

output(outfile1);
 
d := DATASET([{ '<library><book isbn="123456789X">' +
                '<author>Bayliss</author><title>A Way Too Far</title></book>' +
                '<book isbn="1234567801">' +
                '<author>Smith</author><title>A Way Too Short</title></book>' +
                '</library>'}],    
             {STRING line });

rform := RECORD
    STRING author := XMLTEXT('author');
    STRING title  := XMLTEXT('title');
END;
books := PARSE(d, line, rform, XML('library/book'));
output(books);

in1 := DATASET([{'<ENTITY eid="P101" type="PERSON" subtype="MILITARY">' +
                 '<ATTR name="fullname">JOHN SMITH</ATTR>' +
                 '<ATTRGRP descriptor="passport">' +
                 '<ATTR name="idNumber">W12468</ATTR><ATTR name="idType">pp</ATTR>' +
                 '<ATTR name="issuingAuthority">JAPAN PASSPORT AUTHORITY</ATTR>' +
                 '<ATTR name="country" value="L202"/></ATTRGRP></ENTITY>'}], 
               {STRING line});

passportRec := { STRING id, STRING country};
outrec      := { STRING  id, UNICODE  fullname,  passportRec passport };

outrec t( in1 L) := TRANSFORM
    SELF.id               := XMLTEXT('@eid');
    SELF.fullname         := XMLUNICODE('ATTR[@name="fullname"]');
    SELF.passport.id      := XMLTEXT('ATTRGRP[@descriptor="passport"]/ATTR[@name="idNumber"]');
    SELF.passport.country := XMLTEXT('ATTRGRP[@descriptor="passport"]' +
                                     '/ATTR[@name="country"]/@value');
END;

Textout := PARSE(in1, line, t(LEFT), XML('/ENTITY[@type="PERSON"]'));
output(Textout);
