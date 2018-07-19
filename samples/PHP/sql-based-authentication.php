function connect_db($database, $db_user, $db_password, $host = 'localhost', $port = NULL, $die = false) {
	// Returns a MySQL link identifier (handle) on success
	// Returns false or dies() on error depending on the setting of parameter $die
	// Parameter $die configures error handling, setting it any non-false value will die() on error
	// Parameters $host, $port and $die have sensible defaults and are not usually required

	if(!$db_handle = @mysql_connect($host.($port ? ':'.$port : ''), $db_user, $db_password)) {
		if($die)
			die("Can't connect to MySQL server:\r\n".mysql_error());
		else
			return false;
	}
	if(!@mysql_select_db($database, $db_handle)) {
		if($die)
			die("Can't select database '$database':\r\n".mysql_error());
		else
			return false;
	}
	return $db_handle;
}

function create_user($username, $password, $db_handle) {
	// Returns the record ID on success or false on failure
	// Username limit is 32 characters (part of spec)
	if(strlen($username) > 32)
		return false;
	
	// Salt limited to ASCII 32 thru 254 (not part of spec)
	$salt = '';
	do {
		$salt .= chr(mt_rand(32, 254));
	} while(strlen($salt) < 16);
	
	// Create pass_md5
	$pass_md5 = md5($salt.$password);
	
	// Make it all binary safe
	$username = mysql_real_escape_string($username);
	$salt = mysql_real_escape_string($salt);
	
	// Try to insert it into the table - Return false on failure
	if(!@mysql_query("INSERT INTO users (username,pass_salt,pass_md5) VALUES('$username','$salt','$pass_md5')", $db_handle))
		return false;
	
	// Return the record ID
	return mysql_insert_id($db_handle);
}

function authenticate_user($username, $password, $db_handle) {
	// Checks a username/password combination against the database
	// Returns false on failure or the record ID on success

	// Make the username parmeter binary-safe
	$safe_username = mysql_real_escape_string($username);
	
	// Grab the record (if it exists) - Return false on failure
	if(!$result = @mysql_query("SELECT * FROM users WHERE username='$safe_username'", $db_handle))
		return false;

	// Grab the row
	$row = @mysql_fetch_assoc($result);
	
	// Check the password and return false if incorrect
	if(md5($row['pass_salt'].$password) != $row['pass_md5'])
		return false;
	
	// Return the record ID
	return $row['userid'];
}
