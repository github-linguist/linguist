  val s = "hello"                                 //> s  : String = hello
  val s2 = s + " world"                           //> s2  : String = hello world
  val f2 = () =>  " !"                            //> f2  : () => String = <function0>

  println(s2 + f2())                              //> hello world !
