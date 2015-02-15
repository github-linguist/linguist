# Open a file for writing, and truncate it to 1234 bytes.
open FOO, ">>file" or die;
truncate(FOO, 1234);
close FOO;

# Truncate a file to 567 bytes.
truncate("file", 567);
