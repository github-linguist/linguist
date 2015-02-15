let dns_query ~host ~ai_family =
  let opts = [
    Unix.AI_FAMILY ai_family;
    Unix.AI_SOCKTYPE Unix.SOCK_DGRAM;
  ] in
  let addr_infos = Unix.getaddrinfo host "" opts in
  match addr_infos with
  | [] -> failwith "dns_query"
  | ai :: _ ->
    match ai.Unix.ai_addr with
    | Unix.ADDR_INET (addr, _) -> (Unix.string_of_inet_addr addr)
    | Unix.ADDR_UNIX addr -> failwith "addr_unix"

let () =
  let host = "www.kame.net" in
  Printf.printf "primary addresses of %s are:\n" host;

  Printf.printf " IPv4 address: %s\n" (dns_query host Unix.PF_INET);
  Printf.printf " IPv6 address: %s\n" (dns_query host Unix.PF_INET6);
;;
