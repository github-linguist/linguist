def perfectInt(input: Int) = ((2 to sqrt(input).toInt).collect {case x if input % x == 0 => x + input / x}).sum == input - 1
