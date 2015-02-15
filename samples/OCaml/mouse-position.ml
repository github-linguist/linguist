open Xlib

let () =
  let d = xOpenDisplay "" in

  (* ask for active window (no error check);
     the client must be freedesktop compliant *)
  let _, _, _, _, props =
    xGetWindowProperty_window d
        (xDefaultRootWindow d)
        (xInternAtom d "_NET_ACTIVE_WINDOW" true)
        0 1 false AnyPropertyType
  in

  let _, _, _, child, _ = xQueryPointer d props in
  begin match child with
  | Some(_, childx, childy) ->
      Printf.printf "relative to active window: %d,%d\n%!" childx childy;
  | None ->
      print_endline "the pointer is not on the same screen as the specified window"
  end;

  xCloseDisplay d;
;;
