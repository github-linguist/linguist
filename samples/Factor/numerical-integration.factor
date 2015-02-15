USE: math.functions
IN: scratchpad 0 1 [ 3 ^ ] integrate-simpson .
1/4
IN: scratchpad 1000 num-steps set-global
IN: scratchpad 1.0 100 [ -1 ^ ] integrate-simpson .
4.605173316272971
IN: scratchpad 5000000 num-steps set-global
IN: scratchpad 0 5000 [ ] integrate-simpson .
12500000
IN: scratchpad 6000000 num-steps set-global
IN: scratchpad 0 6000 [ ] integrate-simpson .
18000000
