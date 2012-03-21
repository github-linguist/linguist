
 /*------------------------------------------------------------------------
    File        : Email
    Purpose     : Abstraction of an Email message
    Description : Holds information needed for an email message - senders,
                  recipients, subject, a message body, attachment files, and
                  other extra information such as importance, priority,
                  sensitivity, custom reply-to addresses, delivery receipts,
                  read receipts, custom sent date, reply-by date, and expire date.
    Author(s)   : Abe Voelker
    Created     : Sat Jul 17 16:27:05 CDT 2010
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS email.Email  USE-WIDGET-POOL:

    &SCOPED-DEFINE QUOTES                """"
    &SCOPED-DEFINE CR                    CHR(13)
    &SCOPED-DEFINE LF                    CHR(10)
    &SCOPED-DEFINE DEFAULT_MIME_BOUNDARY "!@#$%^&*+-._MIME_BOUNDARY_.-+*&^%$#@!"

    /*------------------------------------------------------------------------------
            Purpose:
            Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PRIVATE VARIABLE objSendEmailAlgorithm AS email.SendEmailAlgorithm NO-UNDO.

    DEFINE PRIVATE TEMP-TABLE ttSenders NO-UNDO
      FIELD cEmailAddress AS CHARACTER
      FIELD cRealName     AS CHARACTER INITIAL ?
      INDEX IXPK_ttSenders cEmailAddress.

    DEFINE PRIVATE TEMP-TABLE ttToRecipients NO-UNDO
      FIELD cEmailAddress AS CHARACTER
      FIELD cRealName     AS CHARACTER INITIAL ?
      INDEX IXPK_ttToRecipients cEmailAddress.

    DEFINE PRIVATE TEMP-TABLE ttCCRecipients NO-UNDO
      FIELD cEmailAddress AS CHARACTER
      FIELD cRealName     AS CHARACTER INITIAL ?
      INDEX IXPK_ttCCRecipients cEmailAddress.

    DEFINE PRIVATE TEMP-TABLE ttBCCRecipients NO-UNDO
      FIELD cEmailAddress AS CHARACTER
      FIELD cRealName     AS CHARACTER INITIAL ?
      INDEX IXPK_ttBCCRecipients cEmailAddress.

    DEFINE PRIVATE TEMP-TABLE ttReplyToRecipients NO-UNDO
      FIELD cEmailAddress AS CHARACTER
      FIELD cRealName     AS CHARACTER INITIAL ?
      INDEX IXPK_ttReplyToRecipients cEmailAddress.

    DEFINE PRIVATE TEMP-TABLE ttReadReceiptRecipients NO-UNDO
      FIELD cEmailAddress AS CHARACTER
      FIELD cRealName     AS CHARACTER INITIAL ?
      INDEX IXPK_ttReadReceiptRecipients cEmailAddress.

    DEFINE PRIVATE TEMP-TABLE ttDeliveryReceiptRecipients NO-UNDO
      FIELD cEmailAddress AS CHARACTER
      FIELD cRealName     AS CHARACTER INITIAL ?
      INDEX IXPK_ttDeliveryReceiptRecipients cEmailAddress.

    DEFINE PRIVATE TEMP-TABLE ttAttachments NO-UNDO
      FIELD cFileName     AS CHARACTER
      FIELD lcData        AS Object    /* Longchar object */
      FIELD lBase64Encode AS LOGICAL.

    DEFINE PRIVATE VARIABLE cMimeBoundary     AS CHARACTER   NO-UNDO.
    DEFINE PRIVATE VARIABLE lcBody            AS LONGCHAR    NO-UNDO.
    DEFINE PRIVATE VARIABLE lBodyIsBase64     AS LOGICAL     NO-UNDO.
    DEFINE PRIVATE VARIABLE cSubject          AS CHARACTER   NO-UNDO.
    DEFINE PRIVATE VARIABLE mptrAttachments   AS MEMPTR      NO-UNDO.
    DEFINE PRIVATE VARIABLE cImportance       AS CHARACTER   NO-UNDO.
    DEFINE PRIVATE VARIABLE cSensitivity      AS CHARACTER   NO-UNDO.
    DEFINE PRIVATE VARIABLE cPriority         AS CHARACTER   NO-UNDO.
    DEFINE PRIVATE VARIABLE dttmtzSentDate    AS DATETIME-TZ INITIAL ? NO-UNDO.
    DEFINE PRIVATE VARIABLE dttmtzReplyByDate AS DATETIME-TZ INITIAL ? NO-UNDO.
    DEFINE PRIVATE VARIABLE dttmtzExpireDate  AS DATETIME-TZ INITIAL ? NO-UNDO.

    DEFINE PRIVATE VARIABLE cNewLine          AS CHARACTER   NO-UNDO.

    /* Other email headers: */

    CONSTRUCTOR PUBLIC Email (INPUT ipobjSendEmailAlgorithm AS email.SendEmailAlgorithm):
        SUPER ().
        ASSIGN objSendEmailAlgorithm = ipobjSendEmailAlgorithm
               cMimeBoundary         = {&DEFAULT_MIME_BOUNDARY}
               lBodyIsBase64         = TRUE.
        IF (OPSYS BEGINS "WIN") THEN
            ASSIGN cNewLine          = {&CR} + {&LF}.
        ELSE
            ASSIGN cNewLine          = {&LF}.
    END CONSTRUCTOR.

    DESTRUCTOR PUBLIC Email ():
        FOR EACH ttAttachments:
            IF VALID-OBJECT(ttAttachments.lcData) THEN
                DELETE OBJECT ttAttachments.lcData NO-ERROR.
        END. /* FOR EACH ttAttachments */
    END DESTRUCTOR.

    /* Add a sender ("From:" address) to the email */
    METHOD PUBLIC VOID addSender(INPUT ipcEmailAddress AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttSenders
                        WHERE ttSenders.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttSenders.
            ASSIGN ttSenders.cEmailAddress = ipcEmailAddress.
        END.
    END METHOD.

    /* Add a sender ("From:" address) (with Real Name) to the email */
    METHOD PUBLIC VOID addSender(INPUT ipcEmailAddress AS CHARACTER,
                                 INPUT ipcRealName     AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttSenders
                        WHERE ttSenders.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttSenders.
            ASSIGN ttSenders.cEmailAddress = ipcEmailAddress
                   ttSenders.cRealName     = ipcRealName.
        END.
    END METHOD.

    /* Add a "To:" recipient to the email */
    METHOD PUBLIC VOID addToRecipient(INPUT ipcEmailAddress AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttToRecipients
                        WHERE ttToRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttToRecipients.
            ASSIGN ttToRecipients.cEmailAddress = ipcEmailAddress.
        END.
    END METHOD.

    /* Add a "To:" recipient (with Real Name) to the email */
    METHOD PUBLIC VOID addToRecipient(INPUT ipcEmailAddress AS CHARACTER,
                                      INPUT ipcRealName     AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttToRecipients
                        WHERE ttToRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttToRecipients.
            ASSIGN ttToRecipients.cEmailAddress = ipcEmailAddress
                   ttToRecipients.cRealName     = ipcRealName.
        END.
    END METHOD.

    /* Add a "CC:" recipient to the email */
    METHOD PUBLIC VOID addCCRecipient(INPUT ipcEmailAddress AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttCCRecipients
                        WHERE ttCCRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttCCRecipients.
            ASSIGN ttCCRecipients.cEmailAddress = ipcEmailAddress.
        END.
    END METHOD.

    /* Add a "CC:" recipient (with Real Name) to the email */
    METHOD PUBLIC VOID addCCRecipient(INPUT ipcEmailAddress AS CHARACTER,
                                      INPUT ipcRealName     AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttCCRecipients
                        WHERE ttCCRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttCCRecipients.
            ASSIGN ttCCRecipients.cEmailAddress = ipcEmailAddress
                   ttToRecipients.cRealName     = ipcRealName.
        END.
    END METHOD.

    /* Add a "BCC:" recipient to the email */
    METHOD PUBLIC VOID addBCCRecipient(INPUT ipcEmailAddress AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttBCCRecipients
                        WHERE ttBCCRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttBCCRecipients.
            ASSIGN ttBCCRecipients.cEmailAddress = ipcEmailAddress.
        END.
    END METHOD.

    /* Add a "BCC:" recipient (with Real Name) to the email */
    METHOD PUBLIC VOID addBCCRecipient(INPUT ipcEmailAddress AS CHARACTER,
                                       INPUT ipcRealName     AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttBCCRecipients
                        WHERE ttBCCRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttBCCRecipients.
            ASSIGN ttBCCRecipients.cEmailAddress = ipcEmailAddress
                   ttToRecipients.cRealName      = ipcRealName.
        END.
    END METHOD.

    /* Add a reply-to recipient to the email */
    METHOD PUBLIC VOID addReplyToRecipient(INPUT ipcEmailAddress AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttReplyToRecipients
                        WHERE ttReplyToRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttReplyToRecipients.
            ASSIGN ttReplyToRecipients.cEmailAddress = ipcEmailAddress.
        END.
    END METHOD.

    /* Add a  reply-to recipient (with Real Name) to the email */
    METHOD PUBLIC VOID addReplyToRecipient(INPUT ipcEmailAddress AS CHARACTER,
                                           INPUT ipcRealName     AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttReplyToRecipients
                        WHERE ttReplyToRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttReplyToRecipients.
            ASSIGN ttReplyToRecipients.cEmailAddress = ipcEmailAddress
                   ttReplyToRecipients.cRealName     = ipcRealName.
        END.
    END METHOD.

    /* Add a delivery receipt recipient to the email */
    METHOD PUBLIC VOID addDeliveryReceiptRecipient(INPUT ipcEmailAddress AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttDeliveryReceiptRecipients
                        WHERE ttDeliveryReceiptRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttDeliveryReceiptRecipients.
            ASSIGN ttDeliveryReceiptRecipients.cEmailAddress = ipcEmailAddress.
        END.
    END METHOD.

    /* Add a delivery receipt recipient (with Real Name) to the email */
    METHOD PUBLIC VOID addDeliveryReceiptRecipient(INPUT ipcEmailAddress AS CHARACTER,
                                                   INPUT ipcRealName     AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttDeliveryReceiptRecipients
                        WHERE ttDeliveryReceiptRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttDeliveryReceiptRecipients.
            ASSIGN ttDeliveryReceiptRecipients.cEmailAddress = ipcEmailAddress
                   ttDeliveryReceiptRecipients.cRealName     = ipcRealName.
        END.
    END METHOD.

    /* Add a read receipt recipient to the email */
    METHOD PUBLIC VOID addReadReceiptRecipient(INPUT ipcEmailAddress AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttReadReceiptRecipients
                        WHERE ttReadReceiptRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttReadReceiptRecipients.
            ASSIGN ttReadReceiptRecipients.cEmailAddress = ipcEmailAddress.
        END.
    END METHOD.

    /* Add a read receipt recipient (with Real Name) to the email */
    METHOD PUBLIC VOID addReadReceiptRecipient(INPUT ipcEmailAddress AS CHARACTER,
                                               INPUT ipcRealName     AS CHARACTER):
        IF NOT CAN-FIND(FIRST ttReadReceiptRecipients
                        WHERE ttReadReceiptRecipients.cEmailAddress EQ ipcEmailAddress) THEN DO:
            CREATE ttReadReceiptRecipients.
            ASSIGN ttReadReceiptRecipients.cEmailAddress = ipcEmailAddress
                   ttReadReceiptRecipients.cRealName     = ipcRealName.
        END.
    END METHOD.

    /* Set the subject of the email */
    METHOD PUBLIC VOID setSubject(INPUT ipcSubject AS CHARACTER):
        ASSIGN cSubject = ipcSubject.
    END METHOD.

    /* Set the importance of the email. H = High, L = Low, anything else = Medium/None */
    METHOD PUBLIC VOID setImportance(INPUT ipcImportance AS CHARACTER):
        ASSIGN cImportance = ipcImportance.
    END METHOD.

    /* Set the sensitivity of the email. */
    /* Possible values (from RFC 2156): "Personal", "Private", or "Company confidential" ("Company-confidential") */
    METHOD PUBLIC VOID setSensitivity(INPUT ipcSensitivity AS CHARACTER):
        ASSIGN cSensitivity = ipcSensitivity.
    END METHOD.

    /* Set the priority of the email (to affect transmission speed and delivery) */
    /* Possible values (from RFC 2156): "normal", "urgent", or "non-urgent" */
    METHOD PUBLIC VOID setPriority(INPUT ipcPriority AS CHARACTER):
        ASSIGN cPriority = ipcPriority.
    END METHOD.

    /* Set the date/time the email was sent */
    METHOD PUBLIC VOID setSentDate(INPUT ipdttmtzSentDate AS DATETIME-TZ):
        ASSIGN dttmtzSentDate = ipdttmtzSentDate.
    END METHOD.

    /* Set the date/time recipient(s) should reply by */
    METHOD PUBLIC VOID setReplyByDate(INPUT ipdttmtzReplyByDate AS DATETIME-TZ):
        ASSIGN dttmtzReplyByDate = ipdttmtzReplyByDate.
    END METHOD.

    /* Set the date/time the message expires */
    METHOD PUBLIC VOID setExpireDate(INPUT ipdttmtzExpireDate AS DATETIME-TZ):
        ASSIGN dttmtzExpireDate = ipdttmtzExpireDate.
    END METHOD.

    /* If send email algorithm not set in constructor, you must set it using this method before the email can be sent */
    METHOD PUBLIC VOID setSendEmailAlgorithm(INPUT ipobjSendEmailAlgorithm AS email.SendEmailAlgorithm):
        ASSIGN objSendEmailAlgorithm = ipobjSendEmailAlgorithm.
    END METHOD.

    METHOD PUBLIC VOID setBodyText(INPUT ipcBodyText AS CHARACTER):
        ASSIGN lcBody = ipcBodyText.
    END METHOD.

    METHOD PUBLIC VOID setBodyText(INPUT iplcBodyText AS LONGCHAR):
        ASSIGN lcBody = iplcBodyText.
    END METHOD.

    /* Set the body by reading in an external file */
    METHOD PUBLIC CHARACTER setBodyFile(INPUT ipcBodyFile AS CHARACTER):
        FILE-INFO:FILE-NAME = ipcBodyFile.
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            RETURN "Cannot locate file '" + ipcBodyFile + "' in the filesystem!".
        IF INDEX(FILE-INFO:FILE-TYPE, "R") EQ 0 THEN
            RETURN "File '" + FILE-INFO:FULL-PATHNAME + "' exists but is not readable!".
        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO OBJECT lcBody NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN "Error copying from file: " + ERROR-STATUS:GET-MESSAGE(1).
        RETURN "". /* Success */
    END METHOD.

    /* Body defaults to base64 encoding, but can be manually disabled */
    METHOD PUBLIC VOID setBodyEncoding(INPUT iplBase64Encode AS LOGICAL):
        ASSIGN lBodyIsBase64 = iplBase64Encode.
    END METHOD.

    /* Add a non-encoded file attachment to the email */
    METHOD PUBLIC CHARACTER addTextAttachment(INPUT ipcFileName AS CHARACTER):
        DEFINE VARIABLE lcTemp AS LONGCHAR NO-UNDO.
        FILE-INFO:FILE-NAME = ipcFileName.
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            RETURN "Cannot locate file '" + ipcFileName + "' in the filesystem!".
        IF INDEX(FILE-INFO:FILE-TYPE, "R") EQ 0 THEN
            RETURN "File '" + FILE-INFO:FULL-PATHNAME + "' exists but is not readable!".
        /* Load file into memory */
        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO OBJECT lcTemp NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN "Error copying from file: " + ERROR-STATUS:GET-MESSAGE(1).
        CREATE ttAttachments.
        ASSIGN ttAttachments.cFileName     = ipcFileName
               ttAttachments.lcData        = NEW email.LongcharWrapper(lcTemp)
               ttAttachments.lBase64Encode = FALSE.
        RETURN "". /* Success */
    END.

    /* Add a file attachment to the email; it defaults to base-64 encoding */
    METHOD PUBLIC CHARACTER addAttachment(INPUT ipcFileName AS CHARACTER):
        DEFINE VARIABLE lcTemp AS LONGCHAR NO-UNDO.
        FILE-INFO:FILE-NAME = ipcFileName.
        IF FILE-INFO:FULL-PATHNAME EQ ? THEN
            RETURN "Cannot locate file '" + ipcFileName + "' in the filesystem!".
        IF INDEX(FILE-INFO:FILE-TYPE, "R") EQ 0 THEN
            RETURN "File '" + FILE-INFO:FULL-PATHNAME + "' exists but is not readable!".
        /* Load file into memory */
        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO OBJECT lcTemp NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN "Error copying from file: " + ERROR-STATUS:GET-MESSAGE(1).
        CREATE ttAttachments.
        ASSIGN ttAttachments.cFileName     = ipcFileName
               ttAttachments.lcData        = NEW email.LongcharWrapper(EmailClient.Util:ConvertDataToBase64(lcTemp))
               ttAttachments.lBase64Encode = TRUE.
        RETURN "". /* Success */
    END.

    /* Override default MIME boundary */
    METHOD PUBLIC VOID setMimeBoundary(INPUT ipcMimeBoundary AS CHARACTER):
        ASSIGN cMimeBoundary = ipcMimeBoundary.
    END METHOD.

    /* Return a concatenated list of To:, CC:, and BCC: recipients */
    METHOD PUBLIC CHARACTER getRecipients():
        DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.

        FOR EACH ttToRecipients
          BREAK BY ttToRecipients.cEmailAddress:
            ASSIGN cRecipients = cRecipients + ttToRecipients.cEmailAddress.
            IF NOT LAST(ttToRecipients.cEmailAddress) THEN DO:
                ASSIGN cRecipients = cRecipients + ", ".
            END.
        END.
        FOR EACH ttCCRecipients
          BREAK BY ttCCRecipients.cEmailAddress:
            IF FIRST(ttCCRecipients.cEmailAddress) AND
               cRecipients NE "" THEN
                ASSIGN cRecipients = cRecipients + ", ".
            ASSIGN cRecipients = cRecipients + ttCCRecipients.cEmailAddress.
            IF NOT LAST(ttCCRecipients.cEmailAddress) THEN
                ASSIGN cRecipients = cRecipients + ttCCRecipients.cEmailAddress.
        END.
        FOR EACH ttBCCRecipients
          BREAK BY ttBCCRecipients.cEmailAddress:
            IF FIRST(ttBCCRecipients.cEmailAddress) AND
               cRecipients NE "" THEN
                ASSIGN cRecipients = cRecipients + ", ".
            ASSIGN cRecipients = cRecipients + ttBCCRecipients.cEmailAddress.
            IF NOT LAST(ttBCCRecipients.cEmailAddress) THEN
                ASSIGN cRecipients = cRecipients + ttBCCRecipients.cEmailAddress.
        END.
        RETURN cRecipients.
    END METHOD.

    /* Dumps all email message headers to CHAR */
    METHOD PUBLIC CHARACTER getHeaders():
        DEFINE VARIABLE cReturnData AS CHARACTER NO-UNDO.

        /* Write the "From:" header */
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "From:".
        FOR EACH ttSenders
          BREAK BY ttSenders.cEmailAddress:
            IF ttSenders.cRealName NE ? THEN
                ASSIGN cReturnData = cReturnData + ttSenders.cRealName + " <" + ttSenders.cEmailAddress + ">".
            ELSE
                ASSIGN cReturnData = cReturnData + ttSenders.cEmailAddress.
            IF NOT LAST(ttSenders.cEmailAddress) THEN
                ASSIGN cReturnData = cReturnData + ", ".
        END.
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "\n".
        /* Write the "To:" header */
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "To:".
        FOR EACH ttToRecipients
          BREAK BY ttToRecipients.cEmailAddress:
            IF ttToRecipients.cRealName NE ? THEN
                ASSIGN cReturnData = cReturnData + ttToRecipients.cRealName + " <" + ttToRecipients.cEmailAddress + ">".
            ELSE
                ASSIGN cReturnData = cReturnData + ttToRecipients.cEmailAddress.
            IF NOT LAST(ttToRecipients.cEmailAddress) THEN
                ASSIGN cReturnData = cReturnData + ", ".
        END.
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "\n".
        /* Write the "Reply-To:" header */
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "Reply-To:".
        IF TEMP-TABLE ttReplyToRecipients:HAS-RECORDS THEN DO:
            /* Use manually-overridden reply-to addresses */
            FOR EACH ttReplyToRecipients
              BREAK BY ttReplyToRecipients.cEmailAddress:
                IF ttReplyToRecipients.cRealName NE ? THEN
                    ASSIGN cReturnData = cReturnData + ttReplyToRecipients.cRealName + " <" + ttReplyToRecipients.cEmailAddress + ">".
                ELSE
                    ASSIGN cReturnData = cReturnData + ttReplyToRecipients.cEmailAddress.
                IF NOT LAST(ttReplyToRecipients.cEmailAddress) THEN
                    ASSIGN cReturnData = cReturnData + ", ".
            END. /* FOR EACH ttReplyToRecipients ... */
        END. /* IF TEMP-TABLE ttReplyToRecipients:HAS-RECORDS */
        ELSE DO:
            /* Write reply-to using sender addresses if reply-to addresses not manually overriddden */
            FOR EACH ttSenders
              BREAK BY ttSenders.cEmailAddress:
                IF ttSenders.cRealName NE ? THEN
                    ASSIGN cReturnData = cReturnData + ttSenders.cRealName + " <" + ttSenders.cEmailAddress + ">".
                ELSE
                    ASSIGN cReturnData = cReturnData + ttSenders.cEmailAddress.
                IF NOT LAST(ttSenders.cEmailAddress) THEN
                    ASSIGN cReturnData = cReturnData + ", ".
            END.
        END. /* ELSE / IF TEMP-TABLE ttReplyToRecipients:HAS-RECORDS */
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "\n".
        /* Write the "Cc:" header */
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "Cc:".
        FOR EACH ttCCRecipients
          BREAK BY ttCCRecipients.cEmailAddress:
            IF ttCCRecipients.cRealName NE ? THEN
                ASSIGN cReturnData = cReturnData + ttCCRecipients.cRealName + " <" + ttCCRecipients.cEmailAddress + ">".
            ELSE
                ASSIGN cReturnData = cReturnData + ttCCRecipients.cEmailAddress.
            IF NOT LAST(ttCCRecipients.cEmailAddress) THEN
                ASSIGN cReturnData = cReturnData + ", ".
        END.
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "\n".
        /* Write the "Bcc:" header */
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "Bcc:".
        FOR EACH ttBCCRecipients
          BREAK BY ttBCCRecipients.cEmailAddress:
            IF ttBCCRecipients.cRealName NE ? THEN
                ASSIGN cReturnData = cReturnData + ttBCCRecipients.cRealName + " <" + ttBCCRecipients.cEmailAddress + ">".
            ELSE
                ASSIGN cReturnData = cReturnData + ttBCCRecipients.cEmailAddress.
            IF NOT LAST(ttBCCRecipients.cEmailAddress) THEN
                ASSIGN cReturnData = cReturnData + ", ".
        END.
        ASSIGN cReturnData = cReturnData + {&QUOTES} + "\n".
        /* If delivery recipients specified, write each recipient out */
        IF TEMP-TABLE ttDeliveryReceiptRecipients:HAS-RECORDS THEN DO:
            ASSIGN cReturnData = cReturnData + {&QUOTES} + "Return-Receipt-To:".
            FOR EACH ttDeliveryReceiptRecipients
              BREAK BY ttDeliveryReceiptRecipients.cEmailAddress:
                IF ttDeliveryReceiptRecipients.cRealName NE ? THEN
                    ASSIGN cReturnData = cReturnData + ttDeliveryReceiptRecipients.cRealName + " <" + ttDeliveryReceiptRecipients.cEmailAddress + ">".
                ELSE
                    ASSIGN cReturnData = cReturnData + ttDeliveryReceiptRecipients.cEmailAddress.
                IF NOT LAST(ttDeliveryReceiptRecipients.cEmailAddress) THEN
                    ASSIGN cReturnData = cReturnData + ", ".
            END. /* FOR EACH ttDeliveryReceiptRecipients */
            ASSIGN cReturnData = cReturnData + {&QUOTES}.
        END. /* IF TEMP-TABLE ttDeliveryReceiptRecipients:HAS-RECORDS */
        /* If read recipients specified, write each recipient out */
        IF TEMP-TABLE ttReadReceiptRecipients:HAS-RECORDS THEN DO:
            ASSIGN cReturnData = cReturnData + {&QUOTES} + "Disposition-Notification-To:".
            FOR EACH ttReadReceiptRecipients
              BREAK BY ttReadReceiptRecipients.cEmailAddress:
                IF ttReadReceiptRecipients.cRealName NE ? THEN
                    ASSIGN cReturnData = cReturnData + ttReadReceiptRecipients.cRealName + " <" + ttReadReceiptRecipients.cEmailAddress + ">".
                ELSE
                    ASSIGN cReturnData = cReturnData + ttReadReceiptRecipients.cEmailAddress.
                IF NOT LAST(ttReadReceiptRecipients.cEmailAddress) THEN
                    ASSIGN cReturnData = cReturnData + ", ".
            END. /* FOR EACH ttReadReceiptRecipients */
            ASSIGN cReturnData = cReturnData + {&QUOTES}.
        END. /* IF TEMP-TABLE ttReadReceiptRecipients:HAS-RECORDS */
        /* Write the "Subject:" header */
        ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Subject:" + cSubject + {&QUOTES}.
        /* Write the "Importance:" header */
        IF cImportance BEGINS "H" THEN
            ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Importance:High" + {&QUOTES}.
        ELSE IF cImportance BEGINS "L" THEN
            ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Importance:Low" + {&QUOTES}.
        /* Write the "Sensitivity" header */
        IF cSensitivity NE "" THEN
            ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Sensitivity:" + cSensitivity + {&QUOTES}.
        /* Write the "Priority" header */
        IF cPriority NE "" THEN
            ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Priority:" + cPriority + {&QUOTES}.
        /* Write the "Date" (sent date) header */
        IF dttmtzSentDate NE ? THEN
            ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Date:" + email.Util:ABLDateTimeToEmail(dttmtzSentDate) + {&QUOTES}.
        IF dttmtzReplyByDate NE ? THEN
            ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Reply-By:" + email.Util:ABLDateTimeToEmail(dttmtzReplyByDate) + {&QUOTES}.
        /* Write the "Expiry-Date" header */
        IF dttmtzExpireDate NE ? THEN
            ASSIGN cReturnData = cReturnData + "\n" + {&QUOTES} + "Expiry-Date:" + email.Util:ABLDateTimeToEmail(dttmtzExpireDate) + {&QUOTES}.
        RETURN cReturnData.
    END METHOD.

    /* Dumps all email message payload data (body and attachments) to LONGCHAR */
    METHOD PUBLIC LONGCHAR getPayload():
        DEFINE VARIABLE lcReturnData AS LONGCHAR NO-UNDO.

        /* If no body and no text, then return empty string ("") */
        IF lcBody EQ "" AND NOT TEMP-TABLE ttAttachments:HAS-RECORDS THEN
            RETURN lcReturnData.

        /* Write payload header */
        ASSIGN lcReturnData = "Mime-Version: 1.0" + cNewLine +
                              "Content-Type: multipart/mixed; boundary=" + cMimeBoundary + cNewLine + cNewLine.

        /* Write out the email body, if it exists */
        IF lcBody NE "" THEN DO:
            ASSIGN lcReturnData = lcReturnData + "--" + cMimeBoundary + cNewLine +
                                  "Content-Type: text/plain; charset=~"us-ascii~"" + cNewLine.
            IF lBodyIsBase64 THEN DO:
                ASSIGN lcReturnData = lcReturnData + "Content-Transfer-Encoding: base64" + cNewLine +
                                      cNewLine +
                                      email.Util:ConvertDataToBase64(lcBody) + cNewLine.
            END.
            ELSE DO:
                ASSIGN lcReturnData = lcReturnData + "Content-Transfer-Encoding: 7bit"   + cNewLine +
                                      cNewLine +
                                      lcBody + cNewLine.
            END.
        END.

        /* Write out each email attachment */
        FOR EACH ttAttachments:
            ASSIGN lcReturnData = lcReturnData + "--" + cMimeBoundary + cNewLine.
            IF ttAttachments.lBase64Encode THEN DO:
                ASSIGN lcReturnData = lcReturnData + "Content-Type: application/octet-stream" + cNewLine +
                                      "Content-Disposition: attachment; filename=~"" + ttAttachments.cFileName + "~"" + cNewLine +
                                      "Content-Transfer-Encoding: base64" + cNewLine + cNewLine +
                                      CAST(ttAttachments.lcData, email.LongcharWrapper):getLongchar() + cNewLine.
            END.
            ELSE DO:
                ASSIGN lcReturnData = lcReturnData + "Content-Type: text/plain; charset=~"us-ascii~"" + cNewLine +
                                      "Content-Disposition: attachment; filename=~"" + ttAttachments.cFileName + "~"" + cNewLine +
                                      "Content-Transfer-Encoding: 7bit" + cNewLine + cNewLine +
                                      CAST(ttAttachments.lcData, email.LongcharWrapper):getLongchar() + cNewLine.
            END.
        END.

        /* Write payload footer */
        ASSIGN lcReturnData = lcReturnData + "--" + cMimeBoundary + "--" + cNewLine.

        RETURN lcReturnData.
    END METHOD.

    METHOD PUBLIC CHARACTER send():
        RETURN objSendEmailAlgorithm:sendEmail(INPUT THIS-OBJECT).
    END METHOD.

END CLASS.
