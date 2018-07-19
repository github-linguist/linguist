function hq9plus(code) {
  var out = '';
  var acc = 0;

  for (var i=0; i<code.length; i++) {
    switch (code.charAt(i)) {
      case 'H': out += "hello, world\n"; break;
      case 'Q': out += code + "\n"; break;
      case '9':
        for (var j=99; j>1; j--) {
          out += j + " bottles of beer on the wall, " + j + " bottles of beer.\n";
          out += "Take one down and pass it around, " + (j-1) + " bottles of beer.\n\n";
        }
        out += "1 bottle of beer on the wall, 1 bottle of beer.\n" +
            "Take one down and pass it around, no more bottles of beer on the wall.\n\n" +
            "No more bottles of beer on the wall, no more bottles of beer.\n" +
            "Go to the store and buy some more, 99 bottles of beer on the wall.\n";
        break;
      case '+': acc++; break;
    }
  }
  return out;
}
