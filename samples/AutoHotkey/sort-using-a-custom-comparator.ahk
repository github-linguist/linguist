numbers = 5,3,7,9,1,13,999,-4
strings = Here,are,some,sample,strings,to,be,sorted
Sort, numbers, F IntegerSort D,
Sort, strings, F StringLengthSort D,
msgbox % numbers
msgbox % strings

IntegerSort(a1, a2) {
return a2 - a1
}

StringLengthSort(a1, a2){
return strlen(a1) - strlen(a2)
}
