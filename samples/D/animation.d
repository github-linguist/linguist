module test26;

import qd, SDL_ttf, tools.time;

void main() {
  screen(320, 200);
  auto last = sec();
  string text = "Hello World! ";
  auto speed = 0.2;
  int dir = true;
  while (true) {
    cls;
    print(10, 10, Bottom|Right, text);
    if (sec() - last > speed) {
      last = sec();
      if (dir == 0) text = text[$-1] ~ text[0 .. $-1];
      else text = text[1 .. $] ~ text[0];
    }
    flip; events;
    if (mouse.clicked
      && mouse.pos in display.select(10, 10, 100, 20)
    ) dir = !dir;
  }
}
