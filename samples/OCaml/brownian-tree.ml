let world_width = 400
let world_height = 400
let num_particles = 20_000

let () =
  assert(num_particles > 0);
  assert(world_width * world_height > num_particles);
;;

let dla ~world =
  (* put the tree seed *)
  world.(world_height / 2).(world_width / 2) <- 1;

  for i = 1 to num_particles do
    (* looping helper function *)
    let rec aux px py =
      (* randomly choose a direction *)
      let dx = (Random.int 3) - 1  (* offsets *)
      and dy = (Random.int 3) - 1 in

      if dx + px < 0 || dx + px >= world_width ||
         dy + py < 0 || dy + py >= world_height then
        (* plop the particle into some other random location *)
        aux (Random.int world_width) (Random.int world_height)
      else if world.(py + dy).(px + dx) <> 0 then
        (* bumped into something, particle set *)
        world.(py).(px) <- 1
      else
        aux (px + dx) (py + dy)
    in
    (* set particle's initial position *)
    aux (Random.int world_width) (Random.int world_height)
  done

let to_pbm ~world =
  print_endline "P1";  (* Type=Portable bitmap, Encoding=ASCII *)
  Printf.printf "%d %d\n" world_width world_height;
  Array.iter (fun line ->
    Array.iter print_int line;
    print_newline()
  ) world

let () =
  Random.self_init();
  let world = Array.make_matrix world_width world_height 0 in
  dla ~world;
  to_pbm ~world;
;;
