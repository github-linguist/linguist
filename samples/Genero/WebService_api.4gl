

options short circuit

import com

public type UserAccount record
  id integer,
  name string,
  dob date,
  isActive boolean
end record

public define userError record attribute(WSError="User error")
  message string
end record

public function getNumberUsers()
attributes(WSGet, WSPath="/users/count", WSDescription="Returns a count of users.")
returns (integer)
  define
    returnCount integer

  SELECT COUNT(*) INTO returnCount FROM UserAccounts

  return returnCount
end function

public function createUser(newUser UserAccount)
attributes(WSPost, WSPath="/users", WSDescription="Create a user profile", WSThrows="400:@userError")
returns string
  define
    returnMessage string

  whenever error continue
  INSERT INTO UserAccounts VALUES (newUser.*)
  whenever error stop

  case
    when sqlca.sqlcode == 0
      let returnMessage = sfmt("Created user: %1", newUser.name)
    otherwise
      let userError.message = sfmt("SQL error:% 1 [%2]", sqlca.sqlcode, SQLERRMESSAGE)
      call com.WebServiceEngine.SetRestError(400, userError)
  end case

  return returnMessage
end function