class Main
{
  Int dot_product (Int[] a, Int[] b)
  {
    a[0]*b[0] + a[1]*b[1] + a[2]*b[2]
  }

  Int[] cross_product (Int[] a, Int[] b)
  {
    [a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1]-a[1]*b[0]]
  }

  Int scalar_triple_product (Int[] a, Int[] b, Int[] c)
  {
    dot_product (a, cross_product (b, c))
  }

  Int[] vector_triple_product (Int[] a, Int[] b, Int[] c)
  {
    cross_product (a, cross_product (b, c))
  }

  Void main ()
  {
    a := [3, 4, 5]
    b := [4, 3, 5]
    c := [-5, -12, -13]

    echo ("a . b = " + dot_product (a, b))
    echo ("a x b = [" + cross_product(a, b).join (", ") + "]")
    echo ("a . (b x c) = " + scalar_triple_product (a, b, c))
    echo ("a x (b x c) = [" + vector_triple_product(a, b, c).join (", ") + "]")
  }
}
