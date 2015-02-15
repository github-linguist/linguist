Run["tput cub1"]                  (* one position to the left *)
Run["tput cuf1" ]                 (* one position to the right *)
Run["tput cuu1" ]                 (* up one line *)
Run["tput cud1"]                  (* down one line *)
Run["tput cr"]                    (* beginning of line *)
Run["tput home"]                  (* top left corner *)


WIDTH=RunThrough["tput cols", ""];
HEIGHT=RunThrough["tput lines", ""];

Run["tput hpa "<>WIDTH]            (* end of line *)
Run["tput cup "<>HEIGHT<>"  "<> WIDTH]    (* bottom right corner *)
