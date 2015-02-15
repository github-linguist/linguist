#load "unix.cma"

let record bytes =
  let buf = String.make bytes '\000' in
  let ic = open_in "/dev/dsp" in
  let chunk = 4096 in
  for i = 0 to pred (bytes / chunk) do
    ignore (input ic buf (i * chunk) chunk)
  done;
  close_in ic;
  (buf)

let play buf len =
  let oc = open_out "/dev/dsp" in
  output_string oc buf;
  close_out oc

let () =
  let bytes = 65536 in
  let p = record bytes in
  play p bytes
