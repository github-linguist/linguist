\ Model Multiobj
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize multi-objectives
  Set0: Priority=3 Weight=1 AbsTol=1 RelTol=0.01
   El0 + El1 + El2 + El3 + El4 + El5 + El6 + El7 + El8 + El9
  Set1: Priority=2 Weight=0.25 AbsTol=2 RelTol=0.01
   El5 + El6 + El7 + El8 + El9 + El15 + El16 + El17 + El18 + El19
  Set2: Priority=2 Weight=1.25 AbsTol=3 RelTol=0.01
   El3 + El4 + El6 + El7 + El13 + El14 + El16 + El17
  Set3: Priority=1 Weight=1 AbsTol=4 RelTol=0.01
   El3 + El4 + El5 + El9 + El10 + El11 + El15 + El16 + El17
Subject To
 Budget: El0 + El1 + El2 + El3 + El4 + El5 + El6 + El7 + El8 + El9 + El10
   + El11 + El12 + El13 + El14 + El15 + El16 + El17 + El18 + El19 <= 12
Bounds
Binaries
 El0 El1 El2 El3 El4 El5 El6 El7 El8 El9 El10 El11 El12 El13 El14 El15 El16
 El17 El18 El19
End