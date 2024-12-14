// Part of linguist/samples

import Shapes;
import "some/graphics/library"; // This does not exist (read below)

fn main() -> i32 {
  // This sample does not show the usage of a real graphics library since there is no major usable graphics library for Carbon.
  // As the language grows, some UI library will emerge for it and this sample can be updated.

  var newGraphicRectangle: Shapes.Rectangle = {Width = 500, Height = 1250 };

  var graphicsWindow: auto = Graphics.Window(newGraphicRectangle, "Window Name");
  graphicsWindow.Show(true);

  return 0;
}
