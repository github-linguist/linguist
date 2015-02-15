use DBI;

my $db = DBI->connect('DBI:mysql:mydatabase:host','login','password');

$statment = $db->prepare("UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?");

$rows_affected = $statment->execute("Smith, Steve",42,'true',99);
