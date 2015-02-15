/* create new Point in one of these ways:
 *    var p = new Point(x,y);
 *    var p = new Point(a_point);
 * default value for x,y is 0
 */
function Point() {
    var arg1 = arguments[0];
    var arg2 = arguments[1];

    if (arg1 instanceof Point) {
        this.x = arg1.x;
        this.y = arg1.y;
    }
    else {
        this.x = arg1 == null ? 0 : arg1;
        this.y = arg2 == null ? 0 : arg1;
    }

    this.set_x = function(_x) {this.x = _x;}
    this.set_y = function(_y) {this.y = _y;}
}

Point.prototype.print = function() {
    var out = "Point(" + this.x + "," + this.y + ")";
    print(out);
}

/* create new Circle in one of these ways:
 *    var c = new Circle(x,y,r);
 *    var c = new Circle(a_circle);
 *    var c = new Circle(a_point,r);
 * default value for x,y,r is 0
 */
function Circle() {
    var arg1 = arguments[0];
    var arg2 = arguments[1];
    var arg3 = arguments[2];

    if (arg1 instanceof Circle) {
        this.x = arg1.x;
        this.y = arg1.y;
        this.r = arg1.r;
    }
    else if (arg1 instanceof Point) {
        this.x = arg1.x;
        this.y = arg1.y;
        this.r = arg2 == null ? 0 : arg2;
    }
    else {
        this.x = arg1 == null ? 0 : arg1;
        this.y = arg2 == null ? 0 : arg2;
        this.r = arg3 == null ? 0 : arg3;
    }

    this.set_x = function(_x) {this.x = _x;}
    this.set_y = function(_y) {this.y = _y;}
    this.set_r = function(_r) {this.r = _r;}
}

Circle.prototype.print = function() {
    var out = "Circle(" + this.x + "," + this.y + "," + this.r + ")";
    print(out);
}
