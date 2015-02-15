let is_dir_empty d =
  Sys.readdir d = [| |]
