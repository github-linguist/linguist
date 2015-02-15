open Xlib

let () =
  (* open connection with the server *)
  let d = xOpenDisplay "" in
  let s = xDefaultScreen d in

  Random.self_init();

  (* create window *)
  let w = xCreateSimpleWindow d (xRootWindow d s) 10 10 300 200 1
                                (xBlackPixel d s) (xWhitePixel d s) in

  (* set window name *)
  xStoreName d w Sys.argv.(0);

  (* select kind of events we are interested in *)
  xSelectInput d w [ExposureMask; KeyPressMask; ButtonPressMask];

  (* map (show) the window *)
  xMapWindow d w;
  xFlush d;

  let dbl = w in
  let gc = xDefaultGC d s in

  (* connect the close button in the window handle *)
  let wm_delete_window = xInternAtom d "WM_DELETE_WINDOW" true in
  xSetWMProtocols d w wm_delete_window 1;

  (* event loop *)
  let e = new_xEvent() in
  try while true do
    xNextEvent d e;

    (* draw or redraw the window *)
    match xEventKind e with
    | XExposeEvent _ ->
        xDrawString d dbl gc 10 20 "Clic in the window to generate";
        xDrawString d dbl gc 10 35 "a key press event";

    | XButtonPressedEvent event ->
        let dat = xButtonEvent_datas event in
        (*
        Printf.printf "button x,y : %d %d\n%!"
            dat.button_x
            dat.button_y;
        *)
        let xKeyEvent_contents = {
            key_serial     = dat.button_serial;
            key_send_event = dat.button_send_event;
            key_display    = dat.button_display;
            key_window     = dat.button_window;
            key_root       = dat.button_root;
            key_subwindow  = dat.button_subwindow;
            key_time       = dat.button_time;
            key_x          = dat.button_x;
            key_y          = dat.button_y;
            key_x_root     = dat.button_x_root;
            key_y_root     = dat.button_y_root;

            key_state = [ShiftMask];
            key_keycode = (24 + Random.int 33);
            key_same_screen = true;
        } in
        let propagate = true in
        let event_mask = KeyPressMask in
        xSendEvent d w propagate event_mask (XKeyPressedEvCnt xKeyEvent_contents);

    (* delete window event *)
    | XClientMessageEvent xclient ->
        let atom = xEvent_xclient_data xclient in
        if atom = wm_delete_window then
          raise Exit

    (* handle key press *)
    | XKeyPressedEvent event ->
        print_endline "Key Pressed Event";
        begin
          let d = xKeyEvent_datas event in
          Printf.printf "keycode: %d\n%!" d.key_keycode;
        end;
        (* exit if q or escape are pressed *)
        let keysym = xLookupKeysym event 0 in
        if keysym = Keysym.xK_q ||
           keysym = Keysym.xK_Escape then
          raise Exit
        else
          let printable, c =
            let buf = "  " in
            let n, _ = xLookupString event buf in
            if (n = 1)
            then (true, buf.[0])
            else (false, '\000')
          in
          if printable then
            Printf.printf "Key '%c' pressed\n%!" c;

    | _ -> ()
  done with
  | Exit ->
      xDestroyWindow d w;
      (* close connection to server *)
      xCloseDisplay d;
;;
