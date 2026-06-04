OPTIONS SHORT CIRCUIT

IMPORT com

PUBLIC TYPE UserAccount RECORD
    id INTEGER,
    name STRING,
    dob DATE,
    isActive BOOLEAN
END RECORD

PUBLIC DEFINE userError RECORD ATTRIBUTE(WSError = "User error")
    message STRING
END RECORD

PUBLIC FUNCTION getNumberUsers()
    ATTRIBUTES(WSGet,
        WSPath = "/users/count",
        WSDescription = "Returns a count of users.")
    RETURNS INTEGER

    DEFINE returnCount INTEGER
    SELECT COUNT(*) INTO returnCount FROM UserAccounts
    RETURN returnCount

END FUNCTION

PUBLIC FUNCTION createUser(newUser UserAccount)
    ATTRIBUTES(WSPost,
        WSPath = "/users",
        WSDescription = "Create a user profile",
        WSThrows = "400:@userError")
    RETURNS STRING

    DEFINE returnMessage STRING
    TRY
        INSERT INTO UserAccounts VALUES (newUser.*)
        LET returnMessage = SFMT("Created user: %1", newUser.name)
    CATCH
        LET userError.message = SFMT("SQL error:%1 [%2]", sqlca.sqlcode, SQLERRMESSAGE)
        CALL com.WebServiceEngine.SetRestError(400, userError)
    END TRY
    RETURN returnMessage

END FUNCTION
