Run["tput smcup"]    (* Save the display *)
Run["echo Hello"]
Pause[5]       (* Wait five seconds *)
Run["tput rmcup"]
