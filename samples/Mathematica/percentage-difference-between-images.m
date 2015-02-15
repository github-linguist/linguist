img50 = ImageData@Import[NotebookDirectory[] <> "Lenna50.jpg"];
img100 = ImageData@Import[NotebookDirectory[] <> "Lenna100.jpg"];
diff = img50 - img100;
Print["Total Difference between both Lenas = ",
 Total@Abs@Flatten@diff/Times @@ Dimensions@img50*100, "%"]
