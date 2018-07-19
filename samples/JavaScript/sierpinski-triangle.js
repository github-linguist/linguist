function triangle(o) {
  var n = 1<<o, line = new Array(2*n), i,j,t,u;
  for (i=0; i<line.length; ++i) line[i] = '&nbsp;';
  line[n] = '*';
  for (i=0; i<n; ++i) {
    document.write(line.join('')+"\n");
    u ='*';
    for(j=n-i; j<n+i+1; ++j) {
      t = (line[j-1] == line[j+1] ? '&nbsp;' : '*');
      line[j-1] = u;
      u = t;
    }
    line[n+i] = t;
    line[n+i+1] = '*';
  }
}
document.write("<pre>\n");
triangle(6);
document.write("</pre>");
