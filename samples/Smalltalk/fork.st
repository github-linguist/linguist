'Here I am' displayNl.
|a|
a := [
  (Delay forSeconds: 2) wait .
  1 to: 100 do: [ :i | i displayNl ]
] fork.
'Child will start after 2 seconds' displayNl.
"wait to avoid terminating first the parent;
 a better way should use semaphores"
(Delay forSeconds: 10) wait.
