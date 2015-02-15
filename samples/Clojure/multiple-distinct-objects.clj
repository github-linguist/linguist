user> (take 3 (repeat (rand))) ; repeating the same random number three times
(0.2787011365537204 0.2787011365537204 0.2787011365537204)
user> (take 3 (repeatedly rand)) ; creating three different random number
(0.8334795669220695 0.08405601245793926 0.5795448744634744)
user>
