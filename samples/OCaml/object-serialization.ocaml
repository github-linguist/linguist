type entity = { name : string }

let create_entity () = { name = "Entity" }
let print_entity x = print_endline x.name
let create_person () = { name = "Cletus" }

let instance1 = create_person ()
let instance2 = create_entity ()

(* Serialize *)
let out_chan = open_out_bin "objects.dat";;
output_value out_chan instance1;;
output_value out_chan instance2;;
close_out out_chan;;

(* Deserialize *)
let in_chan = open_in_bin "objects.dat";;
let result1 : entity = input_value in_chan;;
let result2 : entity = input_value in_chan;;
close_in in_chan;;

print_entity result1;;
print_entity result2;;
