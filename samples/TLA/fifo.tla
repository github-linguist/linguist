-------------------------------- MODULE fifo --------------------------------
EXTENDS Naturals, Sequences
CONSTANT Message
VARIABLES in, out, q

InChan == INSTANCE AsyncInterface WITH Data <- Message, chan <- in
OutChan == INSTANCE AsyncInterface WITH Data <- Message, chan <- out

-----------------------------------------------------------------------------

Init == /\ InChan!Init
        /\ OutChan!Init
        /\ q = <<>>
        
TypeInvariant == /\ InChan!TypeInvariant
                 /\ OutChan!TypeInvariant
                 /\ q \in Seq(Message)
                 /\ Len(q) <= 10

SSend(msg) == /\ InChan!Send(msg) \* Send msg on channel in
              /\ UNCHANGED <<out, q>>
              
BufRcv == /\ InChan!Rcv
          /\ Len(q) < 10
          /\ q' = Append(q, in.val)
          /\ UNCHANGED out

BufSend == /\ q # <<>>
           /\ OutChan!Send(Head(q))
           /\ q' = Tail(q)
           /\ UNCHANGED in
           
RRcv == /\ OutChan!Rcv
        /\ UNCHANGED <<in, q>>
        
Next == \/ \E msg \in Message : SSend(msg)
        \/ BufRcv
        \/ BufSend
        \/ RRcv
        
Spec == Init /\ [][Next]_<<in, out, q>>

-----------------------------------------------------------------------------

THEOREM Spec => []TypeInvariant

=============================================================================