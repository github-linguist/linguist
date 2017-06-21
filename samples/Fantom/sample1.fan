/*
 * Author: Robert Koeninger
 * License: WTFPL (http://www.wtfpl.net/)
 */

class Spelling {

  ** Load sample text and offer corrections for input
  static Void main(Str[] args) {
    text := File.os("big.txt").readAllStr
    counts := Str:Int[:] { def = 0 }
    text.split.each |word| { counts[word] += 1 }
    args.each |arg| { echo(correction(counts, arg)) }
  }

  static const Range letters := Range.makeInclusive(97, 122)

  ** Most probable spelling correction for `word`.
  static Str correction(Str:Int counts, Str word) {
    candidates(counts, word).max |x, y| { counts[x] <=> counts[y] }
  }

  ** Generate possible spelling corrections for `word`.
  static Str[] candidates(Str:Int counts, Str word) {
    result := known(counts, Str[word])
    if (result.size > 0) return result

    result = known(counts, edits1(word))
    if (result.size > 0) return result

    result = known(counts, edits2(word))
    if (result.size > 0) return result

    return Str[word]
  }

  ** The subset of `words` that appear in the map of `counts`.
  static Str[] known(Str:Int counts, Str[] words) {
    words.findAll |word, i| { counts[word] > 0 }.unique
  }

  ** All edits that are one edit away from `word`.
  static Str[] edits1(Str word) {
    edits := Str[,]

    for (i := 0; i < word.size; ++i) {
      edits.add(delete(word, i))

      if (i < word.size - 2) {
        edits.add(transpose(word, i))
      }

      edits.addAll(replace(word, i))
      edits.addAll(insert(word, i))
    }

    edits = edits.unique
    edits.remove(word)
    return edits
  }

  ** Word with `i`th letter removed.
  static Str delete(Str word, Int i) {
    left := word.getRange(Range.makeExclusive(0, i))
    right := word.getRange(Range.makeExclusive(i + 1, word.size))
    return left + right
  }

  ** Word with `i`th and `i+1`st letter swapped.
  static Str transpose(Str word, Int i) {
    left := word.getRange(Range.makeExclusive(0, i))
    right := word.getRange(Range.makeExclusive(i, word.size))
    first := right.get(0).toChar
    second := right.get(1).toChar
    rest := right.getRange(Range.makeExclusive(2, right.size))
    return left + second + first + rest
  }

  ** Word with `i`th letter replaced with every other letter.
  static Str[] replace(Str word, Int i) {
    left := word.getRange(Range.makeExclusive(0, i))
    right := word.getRange(Range.makeExclusive(i + 1, word.size))
    return letters.map |ch| { left + ch.toChar + right }
  }

  ** Word with each letter inserted at `i`.
  static Str[] insert(Str word, Int i) {
    left := word.getRange(Range.makeExclusive(0, i))
    right := word.getRange(Range.makeExclusive(i, word.size))
    return letters.map |ch| { left + ch.toChar + right }
  }

  ** All edits that are two edits away from `word`.
  static Str[] edits2(Str word) {
    (Str[])(edits1(word).map |w| { edits1(w) }.flatten)
  }
}
