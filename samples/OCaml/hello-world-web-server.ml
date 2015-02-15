let try_finalise f x finally y =
  let res = try f x with e -> finally y; raise e in
  finally y;
  res

let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

let double_fork_treatment server service (client_descr, _ as client) =
  let treat () =
    match Unix.fork () with
    | 0 ->
        if Unix.fork () <> 0 then exit 0;
        Unix.close server; service client; exit 0
    | k ->
        ignore (restart_on_EINTR (Unix.waitpid []) k)
  in
  try_finalise treat () Unix.close client_descr

let install_tcp_server_socket addr =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  try
    Unix.bind s addr;
    Unix.listen s 10;
    s
  with e -> Unix.close s; raise e

let tcp_server treat_connection addr =
  ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
  let server_sock = install_tcp_server_socket addr in
  while true do
    let client = restart_on_EINTR Unix.accept server_sock in
    treat_connection server_sock client
  done

let server () =
  let port = 8080 in
  let host = (Unix.gethostbyname (Unix.gethostname())).Unix.h_addr_list.(0) in
  let addr = Unix.ADDR_INET (host, port) in
  let treat sock (client_sock, client_addr as client) =
    let service (s, _) =
      let response = "\
        HTTP/1.1 200 OK\r\n\
        Content-Type: text/html; charset=UTF-8\r\n\r\n\
        <html><head><title>Goodbye, world!</title>\
        <style>body { background-color: #0FF }\
        h1 { font-size:3em; color: black; }</style></head>\
        <body><h1>Goodbye, world!</h1></body></html>\r\n"
      in
      Unix.write s response 0 (String.length response);
    in
    double_fork_treatment sock service client
  in
  tcp_server treat addr

let _ =
  Unix.handle_unix_error server ()
