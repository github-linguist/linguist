open Dlffi

let get_int = function Int v -> v | _ -> failwith "get_int"
let get_ptr = function Ptr v -> v | _ -> failwith "get_ptr"
let get_float = function Float v -> v | _ -> failwith "get_float"
let get_double = function Double v -> v | _ -> failwith "get_double"
let get_string = function String v -> v | _ -> failwith "get_string"

let () =
  (* load the library *)
  let xlib = dlopen "/usr/lib/libX11.so" [RTLD_LAZY] in
  (* load the functions *)
  let _open_display = dlsym xlib "XOpenDisplay"
  and _default_screen = dlsym xlib "XDefaultScreen"
  and _display_width = dlsym xlib  "XDisplayWidth"
  and _display_height = dlsym xlib "XDisplayHeight"
  in
  (* wrap functions to provide a higher level interface *)
  let open_display ~name = get_ptr(fficall _open_display [| String name |] Return_ptr)
  and default_screen ~dpy = get_int(fficall _default_screen [| (Ptr dpy) |] Return_int)
  and display_width ~dpy ~scr = get_int(fficall _display_width [| (Ptr dpy); (Int scr) |] Return_int)
  and display_height ~dpy ~scr = get_int(fficall _display_height [| (Ptr dpy); (Int scr) |] Return_int)
  in
  (* use our functions *)
  let dpy = open_display ~name:":0" in
  let screen_number = default_screen ~dpy in
  let width = display_width ~dpy ~scr:screen_number
  and height = display_height ~dpy ~scr:screen_number in
  Printf.printf "# Screen dimensions are: %d x %d pixels\n" width height;
  dlclose xlib;
;;
