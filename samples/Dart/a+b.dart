import 'dart:io';

// a little helper function that checks if the string only contains
// digits and an optional minus sign at the front
bool isAnInteger(String str) => str.contains(new RegExp(r'^-?\d+$'));

void main() {
  while(true) {
    String input = stdin.readLineSync();
    var chunks = input.split(new RegExp(r'[ ]+')); // split on 1 or more spaces
    if(!chunks.every(isAnInteger)) {
      print("not an integer!");
    } else if(chunks.length > 2) {
      print("too many numbers!");
    } else if(chunks.length < 2) {
      print('not enough numbers!');
    } else {
      // parse the strings into integers
      var nums = chunks.map((String s) => int.parse(s));
      if(nums.any((num) => num < -1000 || num > 1000)) {
        print("between -1000 and 1000 please!");
      } else {
        print(nums.reduce((a, b) => a + b));
      }
    }
  }
}
