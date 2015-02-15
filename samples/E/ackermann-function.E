def A(m, n) {
    return if (m <=> 0)          { n+1              } \
      else if (m > 0 && n <=> 0) { A(m-1, 1)        } \
      else                       { A(m-1, A(m,n-1)) }
}
