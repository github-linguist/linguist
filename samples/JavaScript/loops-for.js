var i, j;
for (i = 1; i <= 5; i += 1) {
  s = '';
  for (j = 0; j < i; j += 1)
    s += '*';
  document.write(s + '<br>');
}
