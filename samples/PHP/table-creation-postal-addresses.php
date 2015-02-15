<?php
$db = new SQLite3(':memory:');
$db->exec("
    CREATE TABLE address (
        addrID     INTEGER PRIMARY KEY AUTOINCREMENT,
        addrStreet TEXT NOT NULL,
        addrCity   TEXT NOT NULL,
        addrState  TEXT NOT NULL,
        addrZIP    TEXT NOT NULL
    )
");
?>
