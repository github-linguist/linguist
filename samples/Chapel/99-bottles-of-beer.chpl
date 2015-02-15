/***********************************************************************
 * Chapel implementation of "99 bottles of beer"
 *
 * by Brad Chamberlain and Steve Deitz
 * 07/13/2006 in Knoxville airport while waiting for flight home from
 *            HPLS workshop
 * compiles and runs with chpl compiler version 1.7.0
 * for more information, contact: chapel_info@cray.com
 *
 *
 * Notes:
 * o as in all good parallel computations, boundary conditions
 *   constitute the vast bulk of complexity in this code (invite Brad to
 *   tell you about his zany boundary condition simplification scheme)
 * o uses type inference for variables, arguments
 * o relies on integer->string coercions
 * o uses named argument passing (for documentation purposes only)
 ***********************************************************************/

// allow executable command-line specification of number of bottles
// (e.g., ./a.out -snumBottles=999999)
config const numBottles = 99;
const numVerses = numBottles+1;

// a domain to describe the space of lyrics
var LyricsSpace: domain(1) = {1..numVerses};

// array of lyrics
var Lyrics: [LyricsSpace] string;

// parallel computation of lyrics array
[verse in LyricsSpace] Lyrics(verse) = computeLyric(verse);

// as in any good parallel language, I/O to stdout is serialized.
// (Note that I/O to a file could be parallelized using a parallel
// prefix computation on the verse strings' lengths with file seeking)
writeln(Lyrics);


// HELPER FUNCTIONS:

proc computeLyric(verseNum) {
  var bottleNum = numBottles - (verseNum - 1);
  var nextBottle = (bottleNum + numVerses - 1)%numVerses;
  return "\n" // disguise space used to separate elements in array I/O
       + describeBottles(bottleNum, startOfVerse=true) + " on the wall, "
       + describeBottles(bottleNum) + ".\n"
       + computeAction(bottleNum)
       + describeBottles(nextBottle) + " on the wall.\n";
}


proc describeBottles(bottleNum, startOfVerse:bool = false) {
// NOTE: bool should not be necessary here (^^^^); working around bug
  var bottleDescription = if (bottleNum) then bottleNum:string
                                         else (if startOfVerse then "N"
                                                               else "n")
                                              + "o more";
  return bottleDescription
       + " bottle" + (if (bottleNum == 1) then "" else "s")
       + " of beer";
}


proc computeAction(bottleNum) {
  return if (bottleNum == 0) then "Go to the store and buy some more, "
                             else "Take one down and pass it around, ";
}
