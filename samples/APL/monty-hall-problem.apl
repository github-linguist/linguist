     ∇ Run runs;doors;i;chosen;cars;goats;swap;stay;ix;prices
[1]   ⍝0: Monthy Hall problem
[2]   ⍝1: http://rosettacode.org/wiki/Monty_Hall_problem
[3]
[4]    (⎕IO ⎕ML)←0 1
[5]    prices←0 0 1               ⍝ 0=Goat, 1=Car
[6]
[7]    ix←⊃,/{3?3}¨⍳runs          ⍝ random indexes of doors (placement of car)
[8]    doors←(runs 3)⍴prices[ix]  ⍝ matrix of doors
[9]    stay←+⌿doors[;?3]          ⍝ chose randomly one door - is it a car?
[10]   swap←runs-stay             ⍝ If not, then the other one is!
[11]
[12]   ⎕←'Swap: ',(2⍕100×(swap÷runs)),'% it''s a car'
[13]   ⎕←'Stay: ',(2⍕100×(stay÷runs)),'% it''s a car'
     ∇
