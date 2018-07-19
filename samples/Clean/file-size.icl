import StdEnv

fileSize fileName world
    # (ok, file, world) = fopen fileName FReadData world
    | not ok = abort "Cannot open file"
    # (ok, file) = fseek file 0 FSeekEnd
    | not ok = abort "Cannot seek file"
    # (size, file) = fposition file
      (_, world) = fclose file world
    = (size, world)

Start world = fileSize "input.txt" world
