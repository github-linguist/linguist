let remove x = List.filter (fun y -> y <> x)
let buttons_string b =
  String.concat " " (List.map string_of_int b)

let position app x y =
  let view = SFRenderWindow.getView app in
  let width, height = SFView.getSize view in
  let hw = width /. 2.0 in
  let hh = height /. 2.0 in
  (hw +. ((x /. 100.0) *. hw),
   hh +. ((y /. 100.0) *. hh))

let cross =
  [|  1.0,   1.0;  10.0,   1.0;  10.0, -1.0;    1.0, -1.0;
      1.0, -10.0;  -1.0, -10.0;  -1.0, -1.0;  -10.0, -1.0;
    -10.0,   1.0;  -1.0,   1.0;  -1.0, 10.0;    1.0, 10.0; |]

let () =
  let app = SFRenderWindow.make (800, 600) "Joystick Position" in
  let text = SFText.make "" in
  let shape = SFShape.create cross in
  SFShape.setFillColor shape SFColor.white;
  SFShape.setOutlineColor shape SFColor.white;
  SFShape.setOutlineThickness shape 1.0;
  let rec display ((x, y), b) =
    SFText.setString text (buttons_string b);
    let x, y = position app x y in
    SFShape.setPosition shape x y;
    SFRenderWindow.clear app SFColor.black;
    SFRenderWindow.drawText app text ();
    SFRenderWindow.drawShape app shape ();
    SFRenderWindow.display app;
  and loop joyd =
    let get_joystick (((x, y), b) as joyd) = function
    | SFEvent.JoystickButtonPressed (0, button) -> ((x, y), button::b)
    | SFEvent.JoystickButtonReleased (0, button) -> ((x, y), remove button b)
    | SFEvent.JoystickMoved (0, SFEvent.JoystickX, av) -> ((av, y), b)
    | SFEvent.JoystickMoved (0, SFEvent.JoystickY, av) -> ((x, av), b)
    | _ -> joyd
    in
    let rec proc_event joyd =
      match SFRenderWindow.pollEvent app with
      | Some SFEvent.KeyPressed (SFKey.Escape,_,_,_,_)
      | Some SFEvent.Closed -> ()
      | Some event ->
          let joyd = get_joystick joyd event in
          proc_event joyd
      | None ->
          display joyd;
          loop joyd
    in
    proc_event joyd
  in
  loop ((0.0, 0.0), [])
