
#ifndef GLOBAL_H

#define GLOBAL_H

#include <mudlib.h>
#include "../area.h"

#define SETWRAP set_wrap_long(1)
#define WRAP(x) wrap_text(x)
#define AREAPATH "/d/area/"
#define ROOMPATH AREAPATH "rooms/"
#define TO this_object()

#endif