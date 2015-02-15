Multifactorial[n_, m_] := Abs[ Apply[ Times, Range[-n, -1, m]]]
Table[ Multifactorial[j, i], {i, 5}, {j, 10}] // TableForm
