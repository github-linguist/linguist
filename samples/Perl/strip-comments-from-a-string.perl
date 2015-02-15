while (<>)
  {
    s/[#;].*$//s; # remove comment
    s/^\s+//;     # remove leading whitespace
    s/\s+$//;     # remove trailing whitespace
    print
  }
