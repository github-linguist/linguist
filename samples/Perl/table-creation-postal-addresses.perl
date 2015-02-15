use DBI;

my $db = DBI->connect('DBI:mysql:database:server','login','password');

my $statment = <<EOF;
CREATE TABLE `Address` (
    `addrID`       int(11)     NOT NULL   auto_increment,
    `addrStreet`   varchar(50) NOT NULL   default '',
    `addrCity`     varchar(25) NOT NULL   default '',
    `addrState`    char(2)     NOT NULL   default '',
    `addrZIP`      char(10)    NOT NULL   default '',
    PRIMARY KEY (`addrID`)
);
EOF

my $exec = $db->prepare($statment);
$exec->execute;
