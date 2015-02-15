import scala.language.postfixOps
object BinaryDigits {
  (5 toBinaryString).reverse.padTo(14, '0').reverse
                                                  //> res0: String = 00000000000101
  (50 toBinaryString).reverse.padTo(14, '0').reverse
                                                  //> res1: String = 00000000110010
  (9000 toBinaryString).reverse.padTo(14, '0').reverse
                                                  //> res2: String = 10001100101000
}
