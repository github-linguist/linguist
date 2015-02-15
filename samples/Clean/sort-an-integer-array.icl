import StdEnv

sortArray :: (a e) -> a e | Array a e & Ord e
sortArray array = {y \\ y <- sort [x \\ x <-: array]}

Start :: {#Int}
Start = sortArray {2, 4, 3, 1, 2}
