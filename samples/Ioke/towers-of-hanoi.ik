 = method(n, f, u, t,
  if(n < 2,
    "#{f} --> #{t}" println,

    H(n - 1, f, t, u)
    "#{f} --> #{t}" println
    H(n - 1, u, f, t)
  )
)

hanoi = method(n,
  H(n, 1, 2, 3)
)
