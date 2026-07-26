/* https://github.com/bext-lang/b/blob/main/examples/game_of_life.b */
width;
height;
W;
board1;
board2;
board;
next;

mod(n, b) return ((n%b + b)%b);

get(b, x, y) {
	x = mod(x, width);
	y = mod(y, height);
	return (b[x+y*width]);
}
set(b, x, y, v) {
	x = mod(x, width);
	y = mod(y, height);
	b[x+y*width] = v;
}

count_neighbours(b, x, y) {
	auto count; count = 0;
	auto dy; dy = -1; while (dy <= 1) {
		auto dx; dx = -1; while (dx <= 1) {
			if (dx != 0 | dy != 0) count += get(b, x+dx, y+dy);
			dx += 1;
		}
		dy += 1;
	}
	return (count);
}

print() {
	extrn printf;
	auto x, y;

	x = 0; while (x <= width) {
		printf("##");
		x += 1;
	}
	printf("\n");

	y = 0; while (y < height) {
		printf("#");
		x = 0; while (x < width) {
			printf(get(*board, x,y) ? "██" : "  ");
			x += 1;
		}
		printf("#\n");
		y += 1;
	}

	x = 0; while (x <= width) {
		printf("##");
		x += 1;
	}
	printf("\n");
}

step() {
	extrn printf;
	auto y; y = 0; while (y < height) {
		auto x; x = 0; while (x < width) {
			auto a, n, r;
			n = count_neighbours(*board, x, y);
			a = get(*board, x,y);
			r = a ? n == 2 | n == 3 : n==3;
			set(*next, x, y, r);
			x += 1;
		}
		y += 1;
	}

	auto tmp;
	tmp = board;
	board = next;
	next = tmp;
}

main() {
	extrn malloc, memset, printf, usleep;
	auto size;
	width  = 25;
	height = 15;
	size = width*height*(&0[1]);

	board1 = malloc(size);
	board2 = malloc(size);
	memset(board1, 0, size);
	memset(board2,  0, size);
	board = &board1;
	next = &board2;


	set(*board, 3, 2, 1);
	set(*board, 4, 3, 1);
	set(*board, 2, 4, 1);
	set(*board, 3, 4, 1);
	set(*board, 4, 4, 1);

	set(*board, 11, 1, 1);
	set(*board, 13, 1, 1);
	set(*board, 12, 2, 1);
	set(*board, 13, 2, 1);
	set(*board, 12, 3, 1);

	set(*board, 6, 12, 1);
	set(*board, 6, 13, 1);
	set(*board, 4, 13, 1);
	set(*board, 5, 14, 1);
	set(*board, 6, 14, 1);

	set(*board, 16, 8, 1);
	set(*board, 18, 9, 1);
	set(*board, 17, 9, 1);
	set(*board, 16, 10, 1);
	set(*board, 17, 10, 1);

	while (1) {
		print();
		step();
		printf("%c[%dA", 27, height+2);
        // TODO: does not work on Windows
		usleep(150000);
	}
}
