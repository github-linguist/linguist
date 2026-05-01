
function getPlayersByLevel(int minLevel) {
    object* players = users();
    return filter(players, (: $1->query_level() >= minLevel :));
}

/** Schedule a shutdown for the near future. */
void slow_shut_down (int minutes) {
    filter(users(), #'tell_object,
      "Game driver shouts: The memory is getting low !\n");
    "obj/shut"->shut(minutes);
}
