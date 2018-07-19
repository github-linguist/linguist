#!/usr/bin/php
<?php
if ($argc > 1)
    file_put_contents(
        'notes.txt',
        date('r')."\n\t".implode(' ', array_slice($argv, 1))."\n",
        FILE_APPEND
    );
else
    @readfile('notes.txt');
