modInv[a_, m_] :=
 Block[{x,k}, x /. FindInstance[a x == 1 + k m, {x, k}, Integers]]
