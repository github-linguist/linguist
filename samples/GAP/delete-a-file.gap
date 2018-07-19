# Apparently GAP can only remove a file, not a directory
RemoveFile("input.txt");
# true
RemoveFile("docs");
# fail
