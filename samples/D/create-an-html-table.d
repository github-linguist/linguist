import std.stdio, std.random;

void main() {
  writeln(`<table style="text-align:center; border: 1px solid">`);
  writeln("<th></th><th>X</th><th>Y</th><th>Z</th>");
  foreach (i; 0 .. 4)
    writefln("<tr><th>%d</th><td>%d</td><td>%d</td><td>%d</td></tr>",
             i, uniform(0,1000), uniform(0,1000), uniform(0,1000));
  writeln("</table>");
}
