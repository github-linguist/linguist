Permanent[m_List] :=
    With[{v = Array[x, Length[m]]},
      Coefficient[Times @@ (m.v), Times @@ v]
  ]
