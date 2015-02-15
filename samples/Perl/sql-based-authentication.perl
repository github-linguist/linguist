use DBI;

 # returns a database handle configured to throw an exception on query errors
sub connect_db {
    my ($dbname, $host, $user, $pass) = @_;
    my $db = DBI->connect("dbi:mysql:$dbname:$host", $user, $pass)
        or die $DBI::errstr;
    $db->{RaiseError} = 1;
    $db
}

 # if the user was successfully created, returns its user id.
 # if the name was already in use, returns undef.
sub create_user {
    my ($db, $user, $pass) = @_;
    my $salt = pack "C*", map {int rand 256} 1..16;
    $db->do("INSERT IGNORE INTO users (username, pass_salt, pass_md5)
        VALUES (?, ?, unhex(md5(concat(pass_salt, ?))))",
        undef, $user, $salt, $pass)
      and $db->{mysql_insertid} or undef
}

 # if the user is authentic, returns its user id.  otherwise returns undef.
sub authenticate_user {
    my ($db, $user, $pass) = @_;
    my $userid = $db->selectrow_array("SELECT userid FROM users WHERE
        username=? AND pass_md5=unhex(md5(concat(pass_salt, ?)))",
        undef, $user, $pass);
    $userid
}
