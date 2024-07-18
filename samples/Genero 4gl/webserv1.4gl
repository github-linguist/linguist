-- Testing the web service:
--
-- Define FGLAPPSERVER to a free TCP port:
--   $ export FGLAPPSERVER=8089
--
-- Compile and start the server program:
--   $ fglcomp WebService.4gl
--   $ fglrun WebService.42m
--
-- Open a browser, and fetch the OpenAPI doc with:
--
--     http://localhost:8089/api?openapi.json
--

OPTIONS SHORT CIRCUIT

IMPORT com
IMPORT FGL webserv1_api

MAIN

    DEFER INTERRUPT
    CALL startServer()

END MAIN

PRIVATE FUNCTION startServer() RETURNS()
    DEFINE returnCode INTEGER

    CALL com.WebServiceEngine.RegisterRestService("webserv_api", "api")

    DISPLAY "Server started"

    CALL com.WebServiceEngine.Start()

    LET int_flag = FALSE
    WHILE TRUE
        LET returnCode = com.WebServiceEngine.ProcessServices(-1)
        CASE returnCode
            WHEN 0
                DISPLAY "Request processed."
            WHEN -1
                DISPLAY "Timeout reached."
            WHEN -2
                DISPLAY "Disconnected from application server."
                # the application server has closed the connection
                EXIT PROGRAM
            WHEN -3
                DISPLAY "Client connection lost."
            WHEN -4
                DISPLAY "Server interrupted with ctrl-c."
            WHEN -9
                DISPLAY "Unsupported operation."
            WHEN -10
                DISPLAY "Internal server error."
            WHEN -23
                DISPLAY "Deserialization error."
            WHEN -35
                DISPLAY "No such REST operation found."
            WHEN -36
                DISPLAY "Missing REST parameter."
            OTHERWISE
                DISPLAY SFMT("Unexpected server error: %1",returnCode)
                EXIT WHILE
        END CASE

        IF int_flag THEN
            EXIT WHILE
        END IF

    END WHILE

    DISPLAY "Server stopped"

END FUNCTION
