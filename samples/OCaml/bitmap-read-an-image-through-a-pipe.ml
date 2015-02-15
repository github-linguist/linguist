let read_image ~filename =
  if not(Sys.file_exists filename)
  then failwith(Printf.sprintf "the file %s does not exist" filename);
  let cmd = Printf.sprintf "convert \"%s\" ppm:-" filename in
  let ic, oc = Unix.open_process cmd in
  let img = read_ppm ~ic in
  (img)
;;
