#include <ggi/ggi.h>
#include <set>
#include <map>
#include <utility>
#include <iostream>
#include <fstream>
#include <string>

#include <unistd.h> // for usleep

enum cell_type { none, wire, head, tail };

// *****************
// * display class *
// *****************

// this is just a small wrapper for the ggi interface

class display
{
public:
  display(int sizex, int sizey, int pixsizex, int pixsizey,
          ggi_color* colors);
  ~display()
  {
    ggiClose(visual);
    ggiExit();
  }

  void flush();
  bool keypressed() { return ggiKbhit(visual); }
  void clear();
  void putpixel(int x, int y, cell_type c);
private:
  ggi_visual_t visual;
  int size_x, size_y;
  int pixel_size_x, pixel_size_y;
  ggi_pixel pixels[4];
};

display::display(int sizex, int sizey, int pixsizex, int pixsizey,
                 ggi_color* colors):
  pixel_size_x(pixsizex),
  pixel_size_y(pixsizey)
{
  if (ggiInit() < 0)
  {
    std::cerr << "couldn't open ggi\n";
    exit(1);
  }

  visual = ggiOpen(NULL);
  if (!visual)
  {
    ggiPanic("couldn't open visual\n");
  }

  ggi_mode mode;
  if (ggiCheckGraphMode(visual, sizex, sizey,
                        GGI_AUTO, GGI_AUTO, GT_4BIT,
                        &mode) != 0)
  {
    if (GT_DEPTH(mode.graphtype) < 2) // we need 4 colors!
      ggiPanic("low-color displays are not supported!\n");
  }
  if (ggiSetMode(visual, &mode) != 0)
  {
    ggiPanic("couldn't set graph mode\n");
  }
  ggiAddFlags(visual, GGIFLAG_ASYNC);

  size_x = mode.virt.x;
  size_y = mode.virt.y;

  for (int i = 0; i < 4; ++i)
    pixels[i] = ggiMapColor(visual, colors+i);
}

void display::flush()
{
  // set the current display frame to the one we have drawn to
  ggiSetDisplayFrame(visual, ggiGetWriteFrame(visual));

  // flush the current visual
  ggiFlush(visual);

  // try to set a different frame for drawing (errors are ignored; if
  // setting the new frame fails, the current one will be drawn upon,
  // with the only adverse effect being some flickering).
  ggiSetWriteFrame(visual, 1-ggiGetDisplayFrame(visual));
}

void display::clear()
{
  ggiSetGCForeground(visual, pixels[0]);
  ggiDrawBox(visual, 0, 0, size_x, size_y);
}

void display::putpixel(int x, int y, cell_type cell)
{
  // this draws a logical pixel (i.e. a rectangle of size pixel_size_x
  // times pixel_size_y), not a physical pixel
  ggiSetGCForeground(visual, pixels[cell]);
  ggiDrawBox(visual,
             x*pixel_size_x, y*pixel_size_y,
             pixel_size_x, pixel_size_y);
}

// *****************
// * the wireworld *
// *****************

// initialized to an empty wireworld
class wireworld
{
public:
  void set(int posx, int posy, cell_type type);
  void draw(display& destination);
  void step();
private:
  typedef std::pair<int, int> position;
  typedef std::set<position> position_set;
  typedef position_set::iterator positer;
  position_set wires, heads, tails;
};

void wireworld::set(int posx, int posy, cell_type type)
{
  position p(posx, posy);
  wires.erase(p);
  heads.erase(p);
  tails.erase(p);
  switch(type)
  {
  case head:
    heads.insert(p);
    break;
  case tail:
    tails.insert(p);
    break;
  case wire:
    wires.insert(p);
    break;
  }
}

void wireworld::draw(display& destination)
{
  destination.clear();
  for (positer i = heads.begin(); i != heads.end(); ++i)
    destination.putpixel(i->first, i->second, head);
  for (positer i = tails.begin(); i != tails.end(); ++i)
    destination.putpixel(i->first, i->second, tail);
  for (positer i = wires.begin(); i != wires.end(); ++i)
    destination.putpixel(i->first, i->second, wire);
  destination.flush();
}

void wireworld::step()
{
  std::map<position, int> new_heads;
  for (positer i = heads.begin(); i != heads.end(); ++i)
    for (int dx = -1; dx <= 1; ++dx)
      for (int dy = -1; dy <= 1; ++dy)
      {
        position pos(i->first + dx, i->second + dy);
        if (wires.count(pos))
          new_heads[pos]++;
      }
  wires.insert(tails.begin(), tails.end());
  tails.swap(heads);
  heads.clear();
  for (std::map<position, int>::iterator i = new_heads.begin();
       i != new_heads.end();
       ++i)
  {
//     std::cout << i->second;
    if (i->second < 3)
    {
      wires.erase(i->first);
      heads.insert(i->first);
    }
  }
}

ggi_color colors[4] =
  {{ 0x0000, 0x0000, 0x0000 },  // background: black
   { 0x8000, 0x8000, 0x8000 },  // wire: white
   { 0xffff, 0xffff, 0x0000 },  // electron head: yellow
   { 0xffff, 0x0000, 0x0000 }}; // electron tail: red

int main(int argc, char* argv[])
{
  int display_x = 800;
  int display_y = 600;
  int pixel_x = 5;
  int pixel_y = 5;

  if (argc < 2)
  {
    std::cerr << "No file name given!\n";
    return 1;
  }

  // assume that the first argument is the name of a file to parse
  std::ifstream f(argv[1]);
  wireworld w;
  std::string line;
  int line_number = 0;
  while (std::getline(f, line))
  {
    for (int col = 0; col < line.size(); ++col)
    {
      switch (line[col])
      {
      case 'h': case 'H':
        w.set(col, line_number, head);
        break;
      case 't': case 'T':
        w.set(col, line_number, tail);
        break;
      case 'w': case 'W': case '.':
        w.set(col, line_number, wire);
        break;
      default:
        std::cerr << "unrecognized character: " << line[col] << "\n";
        return 1;
      case ' ':
        ; // no need to explicitly set this, so do nothing
      }
    }
    ++line_number;
  }

  display d(display_x, display_y, pixel_x, pixel_y, colors);

  w.draw(d);

  while (!d.keypressed())
  {
    usleep(100000);
    w.step();
    w.draw(d);
  }
  std::cout << std::endl;
}
