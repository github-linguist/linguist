$fh = tmpfile();
// do stuff with $fh
fclose($fh);
// file removed when closed

// or:
$filename = tempnam('/tmp', 'prefix');
echo "$filename\n";
// open $filename and do stuff with it
