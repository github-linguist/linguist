// This macro demonstrates the use of the Roi.* functions.

  requires("1.48h");
  run("Blobs (25K)");
  makePolygon(72,78,155,43,205,111,143,156,74,138);
  print("type: "+Roi.getType);
  print("name: \""+Roi.getName()+"\"");
  print("stroke color: "+Roi.getStrokeColor);
  print("fill color: "+Roi.getFillColor);;
  Roi.getBounds(x,y,w,h);
  print("bounds: ",x,y,w,h);
  Roi.getCoordinates(x, y);
  print("coordinates");
  for (i=0; i<x.length; i++)
     print("  ", i, x[i], y[i]);
  Roi.setProperty("test", "Test Property");
  print("property (test): "+Roi.getProperty("test"));
  Roi.setName("Polygon Selection");
  print("new name: \""+Roi.getName()+"\"");
  Roi.setStrokeWidth(8);
  Roi.setStrokeColor("red");
  print("new stroke color: "+Roi.getStrokeColor);;
  wait(2000);
  Roi.setFillColor("550000ff");
  wait(2000);
  Roi.move(10, 10);
  print("new fill color: "+Roi.getFillColor);
