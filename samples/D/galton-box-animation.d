import std.stdio, std.algorithm, std.random, std.array;

enum int boxW = 41, boxH = 37; // Galton box width and height.
enum int pinsBaseW = 19;       // Pins triangle base size.
enum int nMaxBalls = 55;       // Number of balls.

static assert(boxW >= 2 && boxH >= 2);
static assert((boxW - 4) >= (pinsBaseW * 2 - 1));
static assert((boxH - 3) >= pinsBaseW);
enum centerH = pinsBaseW + (boxW - (pinsBaseW * 2 - 1)) / 2 - 1;

alias CellBaseType = char;
enum Cell : CellBaseType { empty  = ' ',
                           ball   = 'o',
                           wall   = '|',
                           corner = '+',
                           floor  = '-',
                           pin    = '.' }

Cell[boxW][boxH] box; // Galton box. Will be printed upside-down.

struct Ball {
    int x, y; // Position.

    this(in int x_, in int y_) nothrow
    in {
        assert(box[y_][x_] == Cell.empty);
    } body {
        this.x = x_;
        this.y = y_;
        box[y][x] = Cell.ball;
    }

    nothrow const invariant() {
        assert(x >= 0 && x < boxW && y >= 0 && y < boxH);
        assert(box[y][x] == Cell.ball);
    }

    void doStep() {
        if (y <= 0)
            return; // Reached the bottom of the box.

        with (Cell) {
            final switch (box[y - 1][x]) {
                case empty:
                    box[y][x] = Cell.empty;
                    y--;
                    box[y][x] = Cell.ball;
                    break;
                case ball, wall, corner, floor:
                    // It's frozen. (It always piles on other balls).
                    break;
                case pin:
                    box[y][x] = Cell.empty;
                    y--;
                    if (box[y][x - 1] == Cell.empty &&
                        box[y][x + 1] == Cell.empty) {
                        x += uniform(0, 2) * 2 - 1;
                        box[y][x] = Cell.ball;
                        return;
                    } else if (box[y][x - 1] == Cell.empty)
                        x++;
                    else
                        x--;
                    box[y][x] = Cell.ball;
                    break;
            }
        }
    }
}

void initializeBox() {
    // Set ceiling and floor:
    box[0][] = (Cell.corner ~ [Cell.floor].replicate(boxW - 2)
                ~ Cell.corner)[];
    box[$ - 1][] = box[0][];

    // Set walls:
    foreach (r; 1 .. boxH - 1)
        box[r][0] = box[r][$ - 1] = Cell.wall;

    // Set pins:
    foreach (immutable nPins; 1 .. pinsBaseW + 1)
        foreach (pin; 0 .. nPins)
            box[boxH - 2 - nPins][centerH + 1 - nPins + pin * 2]
                = Cell.pin;
}

void drawBox() {
    foreach_reverse (ref row; box)
        writeln(cast(CellBaseType[])row);
}

void main() {
    initializeBox;
    Ball[] balls;

    foreach (const i; 0 .. nMaxBalls + boxH) {
        writefln("\nStep %d:", i);
        if (i < nMaxBalls)
            balls ~= Ball(centerH, boxH - 2); // Add ball.
        drawBox();

        // Next step for the simulation.
        // Frozen balls are kept in balls array for simplicity.
        foreach (ref b; balls)
            b.doStep;
    }
}
