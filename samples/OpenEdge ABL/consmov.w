&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          money            PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-mov-cons LIKE mov-conta
    FIELD de-valor-real AS DEC COLUMN-LABEL "Valor (R$)".

DEFINE VARIABLE cFav AS CHARACTER COLUMN-LABEL "Favorecido" FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cCat AS CHARACTER COLUMN-LABEL "Categoria" FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cSub AS CHARACTER COLUMN-LABEL "Sub-Categoria" FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cResp AS CHARACTER COLUMN-LABEL "Responsável" FORMAT "X(12)" NO-UNDO.

{seg.i}
{cotacao.i}
{func\data.i}

DEFINE NEW GLOBAL SHARED VARIABLE r-mov-conta AS ROWID      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE r-Parent   AS ROWID      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE pcActionBT AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hLastWidget AS HANDLE     NO-UNDO.

DEFINE VARIABLE cProgRelat AS CHARACTER  NO-UNDO.
/*DEFINE NEW GLOBAL SHARED VARIABLE glImplantar AS LOGICAL    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gcFiltro AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hProgsMenu AS HANDLE     NO-UNDO EXTENT 200.
DEFINE NEW GLOBAL SHARED VARIABLE iProgsCont AS INTEGER    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cParamRelat AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hLastWidget AS HANDLE     NO-UNDO.*/

ASSIGN cProgRelat = ENTRY(1,ENTRY(NUM-ENTRIES(THIS-PROCEDURE:FILE-NAME,"\"),THIS-PROCEDURE:FILE-NAME,"\"),".").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brConsulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mov-cons moeda

/* Definitions for BROWSE brConsulta                                    */
&Scoped-define FIELDS-IN-QUERY-brConsulta tt-mov-cons.dt-mov tt-mov-cons.cd-conta fnFav(tt-mov-cons.cd-favorecido) @ cFav fnCat(tt-mov-cons.cod-categoria) @ cCat fnSub(tt-mov-cons.cod-categoria,tt-mov-cons.cd-sub) @ cSub fnResp(tt-mov-cons.usuar-resp) @ cResp moeda.sigla tt-mov-cons.de-valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brConsulta   
&Scoped-define SELF-NAME brConsulta
&Scoped-define QUERY-STRING-brConsulta FOR EACH tt-mov-cons NO-LOCK, ~
             EACH moeda OF tt-mov-cons NO-LOCK     BY tt-mov-cons.dt-mov
&Scoped-define OPEN-QUERY-brConsulta OPEN QUERY {&SELF-NAME} FOR EACH tt-mov-cons NO-LOCK, ~
             EACH moeda OF tt-mov-cons NO-LOCK     BY tt-mov-cons.dt-mov.
&Scoped-define TABLES-IN-QUERY-brConsulta tt-mov-cons moeda
&Scoped-define FIRST-TABLE-IN-QUERY-brConsulta tt-mov-cons
&Scoped-define SECOND-TABLE-IN-QUERY-brConsulta moeda


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brConsulta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brConsulta btRel btDet data-ini data-end ~
btExit IMAGE-1 IMAGE-2 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS deTotal data-ini data-end 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnCat C-Win 
FUNCTION fnCat RETURNS CHARACTER
  ( iCat AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnFav C-Win 
FUNCTION fnFav RETURNS CHARACTER
  ( iFav AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnResp C-Win 
FUNCTION fnResp RETURNS CHARACTER
  ( cResp AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnSub C-Win 
FUNCTION fnSub RETURNS CHARACTER
  ( iCat AS INTEGER, iSub AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btDet 
     IMAGE-UP FILE "image/im-det.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-det.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Det" 
     SIZE 6.14 BY 1.79 TOOLTIP "Detalhar".

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image/im-exi.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-exi.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 6.14 BY 1.79 TOOLTIP "Sair".

DEFINE BUTTON btRel 
     IMAGE-UP FILE "image/im-print.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-print.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btexit 2" 
     SIZE 6.14 BY 1.79 TOOLTIP "Gerar Relat¢rio".

DEFINE VARIABLE data-end AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79 NO-UNDO.

DEFINE VARIABLE data-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79 NO-UNDO.

DEFINE VARIABLE deTotal AS DECIMAL FORMAT "->>>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .79
     BGCOLOR 21 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image/im-flee.bmp":U
     SIZE 6 BY 1.5.

DEFINE IMAGE IMAGE-2
     FILENAME "image/im-fled.bmp":U
     SIZE 6 BY 1.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 2.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brConsulta FOR 
      tt-mov-cons, 
      moeda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brConsulta C-Win _FREEFORM
  QUERY brConsulta NO-LOCK DISPLAY
      tt-mov-cons.dt-mov FORMAT "99/99/9999":U
      tt-mov-cons.cd-conta COLUMN-LABEL "Conta" FORMAT "999":U
      fnFav(tt-mov-cons.cd-favorecido) @ cFav
      fnCat(tt-mov-cons.cod-categoria) @ cCat
      fnSub(tt-mov-cons.cod-categoria,tt-mov-cons.cd-sub) @ cSub
      fnResp(tt-mov-cons.usuar-resp) @ cResp
      moeda.sigla FORMAT "X(3)":U
      tt-mov-cons.de-valor FORMAT "->>>,>>>,>>9.99":U WIDTH 12
      tt-mov-cons.de-valor-real FORMAT "->>>,>>>,>>9.99":U WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 99 BY 18.5
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brConsulta AT ROW 3.75 COL 2
     btRel AT ROW 1.5 COL 9.14 WIDGET-ID 4
     deTotal AT ROW 22.38 COL 83 COLON-ALIGNED WIDGET-ID 2
     btDet AT ROW 1.5 COL 3
     data-ini AT ROW 1.92 COL 37 COLON-ALIGNED
     data-end AT ROW 1.92 COL 58 COLON-ALIGNED NO-LABEL
     btExit AT ROW 1.5 COL 94
     IMAGE-1 AT ROW 1.5 COL 50
     IMAGE-2 AT ROW 1.5 COL 54
     RECT-5 AT ROW 1.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.43 BY 22.25
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Movimentos"
         COLUMN             = 7.86
         ROW                = 7.08
         HEIGHT             = 22.25
         WIDTH              = 100.43
         MAX-HEIGHT         = 34.92
         MAX-WIDTH          = 205.71
         VIRTUAL-HEIGHT     = 34.92
         VIRTUAL-WIDTH      = 205.71
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brConsulta 1 DEFAULT-FRAME */
ASSIGN 
       brConsulta:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       brConsulta:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE.

/* SETTINGS FOR FILL-IN deTotal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brConsulta
/* Query rebuild information for BROWSE brConsulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mov-cons NO-LOCK,
      EACH moeda OF tt-mov-cons NO-LOCK
    BY tt-mov-cons.dt-mov.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _OrdList          = "money.mov-conta.dt-mov|yes"
     _Where[1]         = "LOOKUP(STRING(ROWID(mov-conta)),cLista) <> 0
 AND mov-conta.dt-mov >= data-ini
 AND mov-conta.dt-mov <= data-end"
     _Query            is OPENED
*/  /* BROWSE brConsulta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Movimentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Movimentos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brConsulta
&Scoped-define SELF-NAME brConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brConsulta C-Win
ON MOUSE-SELECT-DBLCLICK OF brConsulta IN FRAME DEFAULT-FRAME
DO:
  
    APPLY "CHOOSE" TO btDet IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brConsulta C-Win
ON RETURN OF brConsulta IN FRAME DEFAULT-FRAME
DO:
  
    APPLY "CHOOSE" TO btDet IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDet C-Win
ON CHOOSE OF btDet IN FRAME DEFAULT-FRAME /* Det */
DO:

    RUN som.p(INPUT "music\click.wav").
    FIND FIRST mov-conta OF tt-mov-cons NO-LOCK NO-ERROR.
    FIND FIRST conta OF mov-conta NO-LOCK NO-ERROR.
    ASSIGN r-mov-conta = ROWID(mov-conta)
           r-Parent = ROWID(conta)
           pcActionBT = "DETAIL".
    {func\run.i &Programa = "mov-conta_det.w"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  
    {func\bt_fechar.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRel C-Win
ON CHOOSE OF btRel IN FRAME DEFAULT-FRAME /* btexit 2 */
DO:

    DEFINE VARIABLE cArquivo AS CHARACTER  NO-UNDO.

    RUN som.p(INPUT "music\click.wav").

    SESSION:SET-WAIT-STATE("image\calc.cur").
    RUN executaRelatorio.
    SESSION:SET-WAIT-STATE("").
    ASSIGN cArquivo = SEARCH("temp\" + cProgRelat + ".htm").
    {func\run.i &Programa = "relview.w (INPUT cArquivo)"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME data-end
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL data-end C-Win
ON ENTRY OF data-end IN FRAME DEFAULT-FRAME
DO:
    {func\set_entry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL data-end C-Win
ON LEAVE OF data-end IN FRAME DEFAULT-FRAME
DO:

    {func\set_leave.i}  
    IF data-end <> INPUT FRAME {&FRAME-NAME} data-end THEN DO:
        ASSIGN INPUT FRAME {&FRAME-NAME} data-end.
        {&OPEN-QUERY-brConsulta}
        RUN initialize.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME data-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL data-ini C-Win
ON ENTRY OF data-ini IN FRAME DEFAULT-FRAME /* Data */
DO:
    {func\set_entry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL data-ini C-Win
ON LEAVE OF data-ini IN FRAME DEFAULT-FRAME /* Data */
DO:

    {func\set_leave.i}  
    IF data-ini <> INPUT FRAME {&FRAME-NAME} data-ini THEN DO:
        ASSIGN INPUT FRAME {&FRAME-NAME} data-ini.
        {&OPEN-QUERY-brConsulta}
        RUN initialize.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{func\calendar.i data-ini}
{func\calendar.i data-end}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

ON F5 ANYWHERE 
DO:
    IF CAN-FIND(FIRST usuario NO-LOCK
                WHERE usuario.nome = gcUsuario
                AND   usuario.administrador) THEN
        {&OPEN-QUERY-BROWSE-1}
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {version.i consmov}
  RUN enable_UI.
  RUN initialize.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY deTotal data-ini data-end 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brConsulta btRel btDet data-ini data-end btExit IMAGE-1 IMAGE-2 RECT-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE executaRelatorio C-Win 
PROCEDURE executaRelatorio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Definiá∆o das vari†veis */
DEFINE VARIABLE cVrsTmp AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLinhaProg AS CHARACTER  NO-UNDO.
def var v_hora             as char                no-undo.
def var v_data             as date initial today format "99/99/9999" no-undo.
/*DEFINE VARIABLE iTotReg AS INTEGER    NO-UNDO.
DEFINE VARIABLE cVersion AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iContParam AS INTEGER    NO-UNDO.
DEFINE VARIABLE cTempParam AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cParamFields AS CHARACTER  NO-UNDO.*/

/*DEFINE VARIABLE deValor AS DECIMAL    NO-UNDO EXTENT 5.
DEFINE VARIABLE iMax AS INTEGER    NO-UNDO.
DEFINE VARIABLE deTotCat AS DECIMAL    NO-UNDO EXTENT 5.
DEFINE VARIABLE deTotal AS DECIMAL    NO-UNDO EXTENT 5.
DEFINE VARIABLE lCat AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cAux AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE deSaldo AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iSemana AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtAux AS DATE    NO-UNDO.
DEFINE VARIABLE dtSemana AS DATE    NO-UNDO.  
DEFINE VARIABLE deDesc AS DECIMAL     NO-UNDO.*/

DEFINE VARIABLE deTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cFav AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE cCat AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE cSub AS CHARACTER FORMAT "X(40)" NO-UNDO.

/* Acerto de datas */

/* Output padr∆o */

INPUT FROM VALUE(SEARCH("version")) CONVERT TARGET "ibm850".
REPEAT:
    IMPORT UNFORMATTED cVrsTmp.
    IF cVrsTmp MATCHES("*[PRODUTO]*") THEN DO:
        ASSIGN cVersion = ENTRY(2,cVrsTmp," ").
    END.
    IF ENTRY(1,ENTRY(2,cVrsTmp," "),".") = cProgRelat THEN DO:
        ASSIGN cLinhaProg = cVrsTmp.
    END.
END.
INPUT CLOSE.

ASSIGN v_hora = STRING(TIME,"HH:MM:SS").

OUTPUT TO VALUE("temp\" + cProgRelat + ".htm") CONVERT TARGET "iso8859-1".

PUT UNFORMATTED "<html>" SKIP(1)
                "<head>" SKIP
                "<title>" CURRENT-WINDOW:TITLE "</title>" SKIP
                "</head>" SKIP(1)
                "<body>" SKIP
                "<div align=~"center~"><center>" SKIP(1)
                "<table width=~"800~" border=~"0~">" SKIP
                "<TBODY>" SKIP
                "  <tr>" SKIP
                "    <td width=~"800~" bgColor=~"#319B64~" colSpan=~"2~" height=~"150~"><p align=~"center~"><font" SKIP
                "    face=~"Arial~" color=~"#ffffff~"><big><big>Movimentos Consultados</big></big></font></td>" SKIP
                "  </tr>" SKIP.

/* L¢gica do relat¢rio */

{func\relat\capitulo.i '"Movimentos"' 800}

{func\relat\titulo.i &Coluna1='"Data"'
                     &Tamanho1=60
                     &Coluna2='"Conta"'
                     &Tamanho2=160
                     &Coluna3='"Favorecido"'
                     &Tamanho3=140
                     &Coluna4='"Categoria"'
                     &Tamanho4=140
                     &Coluna5='"Sub-Categoria"'
                     &Tamanho5=140
                     &Coluna6=''
                     &Tamanho6=20
                     &Coluna7='"Valor"'
                     &Tamanho7=60
                     &Coluna8='"Respons†vel"'
                     &Tamanho8=80
                     &Small=YES}
                         
ASSIGN deTotal = 0.
FOR EACH tt-mov-cons NO-LOCK,
    EACH moeda OF tt-mov-cons NO-LOCK,
    FIRST conta OF tt-mov-cons NO-LOCK
    BY tt-mov-cons.dt-mov:

    ASSIGN cCat = fnCat(tt-mov-cons.cod-categoria)
           cSub = fnSub(tt-mov-cons.cod-categoria,tt-mov-cons.cd-sub)
           cFav = fnFav(tt-mov-cons.cd-favorecido).

    {func\relat\linha.i &Coluna1=tt-mov-cons.dt-mov
                        &Align1="left"
                        &Coluna2=conta.ds-conta
                        &Align2="left"
                        &Coluna3=cFav
                        &Align3="left"
                        &Coluna4=cCat
                        &Align4="left"
                        &Coluna5=cSub
                        &Align5="left"
                        &Coluna6=moeda.sigla
                        &Align6="right"
                        &Coluna7=tt-mov-cons.de-valor
                        &Align7="right"
                        &Coluna8=fnResp(tt-mov-cons.usuar-resp)
                        &Align8="right"
                        &Small=YES}
    ASSIGN deTotal = deTotal + fnCotacao(tt-mov-cons.de-valor, tt-mov-cons.cd-moeda, 0, tt-mov-cons.dt-mov).
END.

{func\relat\total.i &Coluna1='"Total"'
                    &Align1="left"
                    &Coluna2=''
                    &Align2="right"
                    &Coluna3=''
                    &Align3="right"
                    &Coluna4=''
                    &Align4="right"
                    &Coluna5=''
                    &Align5="right"
                    &Coluna6='"R$"'
                    &Align6="right"
                    &Coluna7=deTotal
                    &Align7="right"
                    &Coluna8=''
                    &Align8="right"
                    &Small=YES}

{func\relat\end-capitulo.i}

PUT UNFORMATTED "<tr>" SKIP
                "  <td vAlign=~"bottom~" width=~"800~" bgColor=~"#FFFFFF~" colSpan=~"2~" height=~"50~"></td>" SKIP
                "</tr>" SKIP
                "<tr>" SKIP
                "  <td vAlign=~"bottom~" width=~"800~" bgColor=~"#319B64~" colSpan=~"2~"><font face=~"Arial~"" SKIP
                "  color=~"#ffffff~"><small><strong>MH Money 2005 - " cVersion "</strong></small></font></td>" SKIP
                "</tr>" SKIP
                "<tr>" SKIP
                "  <td vAlign=~"bottom~" width=~"400~"><font face=~"Arial~" color=~"#319B64~"><small><strong>" CAPS(ENTRY(1,ENTRY(2,cLinhaProg," "),".")) "</strong>" SKIP
                "  - " ENTRY(3,cLinhaProg," ") " - " ENTRY(2,cLinhaProg,"%") "</small></font></td>" SKIP
                "  <td vAlign=~"bottom~" width=~"400~"><p align=~"right~"><font face=~"Arial~" color=~"#319B64~"><strong><small>" STRING(TODAY,"99/99/9999") SKIP
                "  - " v_hora "</small></strong></font></td>" SKIP
                "</tr>" SKIP
                "</TBODY>" SKIP
                "</table>" SKIP
                "</center></div>" SKIP
                "</body>" SKIP
                "</html>".

OUTPUT CLOSE.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize C-Win 
PROCEDURE initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hTemp-results AS HANDLE     NO-UNDO.
DEFINE VARIABLE cListaConsMov AS LONGCHAR   NO-UNDO.

SESSION:SET-WAIT-STATE("GENERAL").

ASSIGN hTemp-results = brConsulta:QUERY IN FRAME {&FRAME-NAME}.
IF hTemp-results:NUM-RESULTS > 0 THEN DO:
    ENABLE btDet WITH FRAME {&FRAME-NAME}.
END.
ELSE DO:
    DISABLE btDet WITH FRAME {&FRAME-NAME}.
END.

COPY-LOB FROM FILE SESSION:TEMP-DIRECTORY + "consmov.txt" TO cListaConsMov.

EMPTY TEMP-TABLE tt-mov-cons.

ASSIGN deTotal = 0.
FOR EACH money.mov-conta NO-LOCK
    WHERE mov-conta.dt-mov >= data-ini
    AND mov-conta.dt-mov <= data-end:
    IF LOOKUP(STRING(ROWID(mov-conta)),cListaConsMov) <> 0 THEN DO: 
        ASSIGN deTotal = deTotal + fnCotacao(mov-conta.de-valor, mov-conta.cd-moeda, 0, mov-conta.dt-mov).
        CREATE tt-mov-cons.
        BUFFER-COPY mov-conta TO tt-mov-cons.
        ASSIGN tt-mov-cons.de-valor-real = fnCotacao(mov-conta.de-valor, mov-conta.cd-moeda, 0, mov-conta.dt-mov).
    END.
END.
DISPLAY deTotal
    WITH FRAME {&FRAME-NAME}.

{&OPEN-QUERY-brConsulta}

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnCat C-Win 
FUNCTION fnCat RETURNS CHARACTER
  ( iCat AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.

    FIND FIRST categoria WHERE categoria.cod-categoria = iCat NO-LOCK NO-ERROR.
    IF AVAIL categoria THEN
        ASSIGN cReturn = categoria.ds-categoria.

    RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnFav C-Win 
FUNCTION fnFav RETURNS CHARACTER
  ( iFav AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.

    FIND FIRST favorecido WHERE favorecido.cd-favorecido = iFav NO-LOCK NO-ERROR.
    IF AVAIL favorecido THEN
        ASSIGN cReturn = favorecido.ds-favorecido.

    RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnResp C-Win 
FUNCTION fnResp RETURNS CHARACTER
  ( cResp AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    IF cResp = "" THEN
        RETURN "Todos".
    ELSE
        RETURN cResp.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnSub C-Win 
FUNCTION fnSub RETURNS CHARACTER
  ( iCat AS INTEGER, iSub AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cReturn AS CHARACTER  NO-UNDO.

    FIND FIRST sub-cat 
        WHERE sub-cat.cd-sub = iSub
        AND   sub-cat.cod-categoria = iCat NO-LOCK NO-ERROR.
    IF AVAIL sub-cat THEN
        ASSIGN cReturn = sub-cat.ds-sub.

    RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

