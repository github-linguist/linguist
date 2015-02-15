import scala.util.Random

object BullCow {
   def main(args: Array[String]): Unit = {
      val number=chooseNumber
      var guessed=false
      var guesses=0

      while(!guessed){
         Console.print("Guess a 4-digit number with no duplicate digits: ")
         val input=Console.readInt
         val digits=input.toString.map(_.asDigit).toList
         if(input>=1111 && input<=9999 && !hasDups(digits)){
            guesses+=1
            var bulls, cows=0
            for(i <- 0 to 3)
               if(number(i)==digits(i))
                  bulls+=1
               else if(number.contains(digits(i)))
                  cows+=1

            if(bulls==4)
               guessed=true
            else
               println("%d Cows and %d Bulls.".format(cows, bulls))
         }
      }
      println("You won after "+guesses+" guesses!");
   }

   def chooseNumber={
      var digits=List[Int]()
      while(digits.size<4){
         val d=Random.nextInt(9)+1
         if (!digits.contains(d))
            digits=digits:+d
      }
      digits
   }

   def hasDups(input:List[Int])=input.size!=input.distinct.size
}
