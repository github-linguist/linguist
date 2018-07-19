$updatePlayers = "UPDATE `players` SET `name` = ?, `score` = ?, `active` = ?\n".
		"WHERE `jerseyNum` = ?";
$dbh = new PDO( "mysql:dbname=db;host=localhost", "username", "password" );

$updateStatement = $dbh->prepare( $updatePlayers );

$updateStatement->bindValue( 1, "Smith, Steve", PDO::PARAM_STR );
$updateStatement->bindValue( 2, 42, PDO::PARAM_INT );
$updateStatement->bindValue( 3, 1, PDO::PARAM_INT );
$updateStatement->bindValue( 4, 99, PDO::PARAM_INT );

$updateStatement->execute();

// alternatively pass parameters as an array to the execute method
$updateStatement = $dbh->prepare( $updatePlayers );
$updateStatement->execute( array( "Smith, Steve", 42, 1, 99 ) );
