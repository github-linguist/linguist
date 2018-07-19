NB. pad the first n dimensions of an array with zeros
NB. (increasing all dimensions by 1 less than the kernel size)
pad=: adverb define
  adj1=: <.m%2
  adj2=: m-1
  (-@(adj2 + ]) {. (adj1 + ]) {. [) (#m) {. $
)

kernel_filter=: adverb define
  [: ,/"(-#$m) ($m) +/@(,/^:(_1+#$m))@:*&m;._3  ($m)pad
)
