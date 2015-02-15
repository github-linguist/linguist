open Xlib

let () =
  let d = xOpenDisplay "" in
  let s = xDefaultScreen d in
  let w = xCreateSimpleWindow d (xRootWindow d s) 10 10 100 100 1
                                (xBlackPixel d s) (xWhitePixel d s) in
  xSelectInput d w [ExposureMask; KeyPressMask];
  xMapWindow d w;

  let msg = "Hello, World!" in

  let rec main_loop() =
    match xEventType(xNextEventFun d) with
    | Expose ->
        xFillRectangle d w (xDefaultGC d s) 20 20 10 10;
        xDrawString d w (xDefaultGC d s) 10 50 msg;
        main_loop()
    | KeyPress -> ()  (* exit main loop *)
    | _ -> main_loop()
  in
  main_loop();
  xCloseDisplay d;
;;
