object LIFCI extends App {
  List(List(1, 34, 3, 98, 9, 76, 45, 4), List(54, 546, 548, 60)).map{l=>l.permutations.toList
    .map{l=>(l.map{_.toString}:\"")(_+_).toLong}.sorted.last}.foreach{println}
}
