import io.Source.fromFile

def letterFrequencies(filename: String) =
  fromFile(filename).mkString groupBy (c => c) mapValues (_.length)
