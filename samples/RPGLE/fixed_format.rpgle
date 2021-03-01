     H/TITLE Testing syntax highlighting
      *
     H* Test control spec
     H DATFMT(*YMD) DATEDIT(*YMD) DEBUG(*YES) OPTION(*NODEBUGIO)                h spec comment             
      *
     F* Test file spec (program described)
     FSOMEFILE  OTEAAF12345L12345ZIWORKSTN EXTDESC(FILE123)                     f spec comment
      *
     F* Test file spec (externally described)
     FANOTHRFILEUPEADE     L     G SPECIAL INDDS(SOMEDS)                        f spec comment
      *
     I* Test input spec
     IMYFILE    A1NO**11111NCX22222NZY33333NDZ                                  i spec comment
      *
     D* Test definition spec
     DMYFIELD        ESS *OPCODE-12345 *12 NOOPT                                d spec comment
      *
     C* Test calculation spec
     CSRNH4FACTOR1       ADD       FACTOR2       RESULT        1234533KCU7OV    c spec comment
      *
     CL4 KPMYFIELD       IF        ANOTHERFIELD = 'something'                   c spec comment
      *
     C/COPY /something
      *
     O* Test ouput spec
     OMYFILE    TF  N02 OGN1PSOMERECRDS123222100105                             o spec comment
      *
     P* Test procedure spec
     PMYPROC           B                   EXPORT                               p spec - begin procedure
     PMYPROC           E                                                        p spec - end procedure
      *
     /EJECT