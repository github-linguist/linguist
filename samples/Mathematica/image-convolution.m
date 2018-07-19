img = Import[NotebookDirectory[] <> "Lenna50.jpg"];
kernel = {{0, -1, 0}, {-1, 4, -1}, {0, -1, 0}};
ImageConvolve[img, kernel]
ImageConvolve[img, GaussianMatrix[35] ]
ImageConvolve[img, BoxMatrix[1] ]
