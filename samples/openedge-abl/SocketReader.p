
/*------------------------------------------------------------------------
    File        : SocketReader.p
    Purpose     :
    Author(s)   : Abe Voelker
    Created     : Sat Aug 21 08:31:38 CDT 2010
    Notes       : Based on code from smtpmail.p
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER objSendEmailAlg    AS email.SendEmailSocket NO-UNDO.
DEFINE VARIABLE vbuffer AS MEMPTR  NO-UNDO.
DEFINE VARIABLE vstatus AS LOGICAL NO-UNDO.
DEFINE VARIABLE vState  AS INTEGER NO-UNDO.


ASSIGN vstate = 1.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FUNCTION getHostname RETURNS CHARACTER():
    DEFINE VARIABLE cHostname AS CHARACTER NO-UNDO.
    INPUT THROUGH hostname NO-ECHO.
      IMPORT UNFORMATTED cHostname.
    INPUT CLOSE.
    RETURN cHostname.
END FUNCTION.

/*
  Status:
        0 - No Connection to the server
        1 - Waiting for 220 connection to SMTP server
        2 - Waiting for 250 OK status to start sending email
        3 - Waiting for 250 OK status for sender
        4 - Waiting for 250 OK status for recipient
        5 - Waiting for 354 OK status to send data
        6 - Waiting for 250 OK status for message received
        7 - Quiting
*/

PROCEDURE newState:
    DEFINE INPUT PARAMETER newState AS INTEGER.
    DEFINE INPUT PARAMETER pstring  AS CHARACTER.
    vState = newState.
    IF pstring = "" THEN
        RETURN.
    SET-SIZE(vbuffer) = LENGTH(pstring) + 1.
    PUT-STRING(vbuffer,1) = pstring.
    SELF:WRITE(vbuffer, 1, LENGTH(pstring)).
    SET-SIZE(vbuffer) = 0.
END PROCEDURE.

PROCEDURE ReadSocketResponse:
    DEFINE VARIABLE vlength AS INTEGER NO-UNDO.
    DEFINE VARIABLE str AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v AS INTEGER NO-UNDO.

    MESSAGE SELF:GET-BYTES-AVAILABLE() VIEW-AS ALERT-BOX.
    vlength = SELF:GET-BYTES-AVAILABLE().
    IF vlength > 0 THEN DO:
        SET-SIZE(vbuffer) = vlength + 1.
        SELF:READ(vbuffer, 1, vlength, 1).
        str = GET-STRING(vbuffer,1).
        SET-SIZE(vbuffer) = 0.
        objSendEmailAlg:handleResponse(str).
        /*
        v = INTEGER(ENTRY(1, str," ")).
        CASE vState:
            WHEN 1 THEN
                IF v = 220 THEN
                    RUN newState(2, "HELO " + getHostname() + "~r~n").
                ELSE
                    vState = -1.
            WHEN 2 THEN
                IF v = 250 THEN
                    RUN newState(3, "MAIL From: " + "hardcoded@gmail.com" + "~r~n").
                ELSE
                    vState = -1.
            WHEN 3 THEN
                IF v = 250 THEN
                    RUN newState(4, "RCPT TO: " + "hardcoded@gmail.com" + "~r~n").
                ELSE
                    vState = -1.
            WHEN 4 THEN
                IF v = 250 THEN
                    RUN newState(5, "DATA ~r~n").
                ELSE
                    vState = -1.
            WHEN 5 THEN
                IF v = 354 THEN
                    RUN newState(6, "From: " + "hardcoded@gmail.com" + "~r~n" +
                                "To: " + "hardcoded@gmail.com" + " ~r~n" +
                          "Subject: " + "Test Subject" +
                           " ~r~n~r~n" +
                           "Test Body" + "~r~n" +
                           ".~r~n").
                ELSE
                    vState = -1.

            WHEN 6 THEN
                IF v = 250 THEN
                    RUN newState(7,"QUIT~r~n").
                ELSE
                    vState = -1.
        END CASE.
        */
    END.
    /*
    IF vState = 7 THEN
       MESSAGE "Email has been accepted for delivery.".
    IF vState < 0 THEN
        MESSAGE "Email has been aborted".
    */
END PROCEDURE.

