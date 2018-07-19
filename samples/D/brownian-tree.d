import core.stdc.stdio, std.random, grayscale_image;

enum uint side = 600; // Square world side.
enum uint num_particles = 1_000;
static assert(side > 2 && num_particles < (side ^^ 2 * 0.7));

void main() {
    auto rng = Xorshift(unpredictableSeed);
    ubyte[side][side] W; // World.
    W[side / 2][side / 2] = 1; // Set tree root.

    foreach (immutable _; 0 .. num_particles) {
        // Random initial particle position.
        OVER: uint x, y;
        do {
            x = uniform(1, side - 1, rng);
            y = uniform(1, side - 1, rng);
        } while (W[y][x]); // Assure the chosen cell is empty.

        while (W[y-1][x-1] + W[y-1][x] + W[y-1][x+1] +
               W[y][x-1]               + W[y][x+1] +
               W[y+1][x-1] + W[y+1][x] + W[y+1][x+1] == 0) {
            // Randomly choose a direction (Moore neighborhood).
            uint dxy = uniform(0, 8, rng);
            if (dxy > 3) dxy++; // To avoid the center.
            x += (dxy % 3) - 1;
            y += (dxy / 3) - 1;
            if (x < 1 || x >= side - 1 || y < 1 || y >= side - 1)
                goto OVER;
        }

        W[y][x] = 1; // Touched, set the cell.
    }

    ubyte[] data = (&W[0][0])[0 .. side ^^ 2]; // Flat view.
    data[] += 255;
    Image!ubyte.fromData(data, side,side).savePGM("brownian_tree.pgm");
}
