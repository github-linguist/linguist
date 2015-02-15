open Sem

let () =
  let oflags = [Unix.O_CREAT;
                Unix.O_EXCL] in
  let sem = sem_open "MyUniqueName" ~oflags () in
  (* here the real code of the app *)
  Unix.sleep 20;
  (* end of the app *)
  sem_unlink "MyUniqueName";
  sem_close sem
