import std.stdio: writeln;

class Point {
    private int x, y;
    this(int x_=0, int y_=0) { x = x_; y = y_; }
    this(Point p_) { x = p_.getX(); y = p_.getY(); }
    int getX() { return x; }
    void setX(int x_) { this.x = x_; }
    int getY() { return y; }
    void setY(int y_) { this.y = y_; }
}

class Circle : Point {
    private int r;
    this(int x_=0, int y_=0, int r_=0) {
        super(x_, y_);
        r = r_;
    }
    this(Point p, int r_=0) {
        super(p);
        r = r_;
    }
    this(Circle c_) {
        super(c_.getX(), c_.getY());
        r = c_.getR();
    }
    int getR() { return r; }
    void setR(int r0) { this.r = r0; }
}

void main() {
    auto p = new Point();
    auto c = new Circle();
    writeln(p);
    writeln(c);
}
