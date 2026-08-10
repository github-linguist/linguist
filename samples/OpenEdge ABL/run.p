DEFINE VARIABLE ch_prog AS CHARACTER NO-UNDO.

/* Extracts the parameters */
ASSIGN ch_prog = ENTRY( 1, SESSION:PARAMETER ).

RUN VALUE( REPLACE( PROGRAM-NAME( 1 ), "run.p", "read-env-var.p") ).

/* OpenEdge Startup Procedure */
DEFINE VARIABLE vsabl_oe_startup_procedure AS CHARACTER NO-UNDO.
vsabl_oe_startup_procedure = OS-GETENV ( "VSABL_OE_STARTUP_PROCEDURE" ).
IF LENGTH( vsabl_oe_startup_procedure ) > 0 THEN RUN VALUE( vsabl_oe_startup_procedure ).

/* RUN */
RUN VALUE( ch_prog ).