inherit "room/room";

#include "globals.h"

void reset(int arg) {
  ::reset(arg);
  
  SETWRAP;

  if(!arg) {

    set_light(1);
    no_castle_flag = 1;
    
    short_desc = "Along the edge of a field.";
    long_desc = 
      "You are standing along the western edge of the fields where the " +
      "city grows its crops.  " +
      "A long, straight path heading east has been cut deep in to the middle " +
      "of the crops.  " +
      "Far off to the west you see the tree line of a forest."
      ;

    dest_dir = ({ 
      "room/crop", "east",
      ROOMPATH + "treeline.c", "west"
    });

    items = ({
      ({"crops", "crop","field"}), 
        WRAP(
        "A large field of crops that extends east as far as the eye can see. "+
        "There is a path heading east, in to the field."
        ),
      ({"tree", "trees", "tree line"}),
        "The edge of a forest made up of some species of pine tree."
    });

    search_items = ({
      ({"crop", "crops", "field"}), "It looks like corn.",
    });

    sounds =
    ({
      "", "You can hear the breeze rustling through the stalks in the field.",
    });

    if (!present("bulletin board")) {
      move_object(AREAPATH+"obj/board.c", TO);
    }
  }
}

