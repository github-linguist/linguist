function stripchars(string, chars) {
  return string.replace(RegExp('['+chars+']','g'), '');
}
