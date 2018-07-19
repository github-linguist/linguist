# Dirty implementation
# Only P3 format, an image is a list of 3 matrices (r, g, b)
# Max color is always 255
WriteImage := function(name, img)
  local f, r, g, b, i, j, maxcolor, nrow, ncol, dim;
  f := OutputTextFile(name, false);
  r := img[1];
  g := img[2];
  b := img[3];
  dim := DimensionsMat(r);
  nrow := dim[1];
  ncol := dim[2];
  maxcolor := 255;
  WriteLine(f, "P3");
  WriteLine(f, Concatenation(String(ncol), " ", String(nrow), " ", String(maxcolor)));
  for i in [1 .. nrow] do
    for j in [1 .. ncol] do
      WriteLine(f, Concatenation(String(r[i][j]), " ", String(g[i][j]), " ", String(b[i][j])));
    od;
  od;
  CloseStream(f);
end;

PutPixel := function(img, i, j, color)
  img[1][i][j] := color[1];
  img[2][i][j] := color[2];
  img[3][i][j] := color[3];
end;

GetPixel := function(img, i, j)
  return [img[1][i][j], img[2][i][j], img[3][i][j]];
end;

NewImage := function(nrow, ncol, color)
  local r, g, b;
  r := color[1] + NullMat(nrow, ncol);
  g := color[2] + NullMat(nrow, ncol);
  b := color[3] + NullMat(nrow, ncol);
  return [r, g, b];
end;

# Reproducing the example from Wikipedia
black := [ 0, 0, 0 ];
g := NewImage(2, 3, black);
PutPixel(g, 1, 1, [255, 0, 0]);
PutPixel(g, 1, 2, [0, 255, 0]);
PutPixel(g, 1, 3, [0, 0, 255]);
PutPixel(g, 2, 1, [255, 255, 0]);
PutPixel(g, 2, 2, [255, 255, 255]);
PutPixel(g, 2, 3, [0, 0, 0]);
WriteImage("example.ppm", g);
