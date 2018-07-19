declare
  PosInf = 1./0.
  NegInf = ~1./0.
in
  {Show PosInf}
  {Show NegInf}

  %% some assertion
  42. / PosInf = 0.
  42. / NegInf = 0.
  PosInf * PosInf = PosInf
  PosInf * NegInf = NegInf
  NegInf * NegInf = PosInf
