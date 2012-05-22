class Point {
  Point(this.x, this.y);
  distanceTo(Point other) {
    var dx = x - other.x;
    var dy = y - other.y;
    return Math.sqrt(dx * dx + dy * dy);
  }
  var x, y;
}

main() {
  Point p = new Point(2, 3);
  Point q = new Point(3, 4);
  print('distance from p to q = ${p.distanceTo(q)}');
}
