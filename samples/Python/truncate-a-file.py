def truncate_file(fname, size):
    "Open a file for writing, and truncate it to size bytes."
    with open(fname, "ab") as f:
        f.truncate(size)
