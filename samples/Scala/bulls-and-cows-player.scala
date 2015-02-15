    def allCombinations: Seq[List[Byte]] = {
      (0 to 9).map(_.byteValue).toList.combinations(4).toList.flatMap(_.permutations)
    }

    def nextGuess(possible: Seq[List[Byte]]): List[Byte] = possible match {
      case Nil => throw new IllegalStateException
      case List(only) => only
      case _ => possible(Random.nextInt(possible.size))
    }

    def doGuess(guess: List[Byte]): Pair[Int, Int] = {
      println("My guess is " + guess);
      val arr = readLine().split(' ').map(Integer.valueOf(_))
      (arr(0), arr(1))
    }

    def testGuess(alt: List[Byte], guess: List[Byte]): Pair[Int, Int] = {
      val bulls =alt.zip(guess).filter(p => p._1 == p._2).size
      val cows = guess.filter(alt.contains(_)).size - bulls
      (bulls, cows)
    }

    def play(possible: Seq[List[Byte]]): List[Byte] = {
      val curGuess = nextGuess(possible)
      val bc = doGuess(curGuess)
      if (bc._1 == 4) { println("Ye-haw!"); curGuess }
      else
        play(possible.filter(p => testGuess(p, curGuess) == bc))

    }

    def main(args: Array[String]) {
      play(allCombinations)
    }
