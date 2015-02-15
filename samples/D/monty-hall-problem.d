import std.stdio, std.random;

void main() {
  int switchWins, stayWins;

  while (switchWins + stayWins < 100_000) {
    immutable carPos = uniform(0, 3);  // Which door is car behind?
    immutable pickPos = uniform(0, 3);  // Contestant's initial pick.
    int openPos;  // Which door is opened by Monty Hall?

    // Monty can't open the door you picked or the one with the car
    // behind it.
    do {
      openPos = uniform(0, 3);
    } while(openPos == pickPos || openPos == carPos);

    int switchPos;
    // Find position that's not currently picked by contestant and
    // was not opened by Monty already.
    for (; pickPos==switchPos || openPos==switchPos; switchPos++) {}

    if (pickPos == carPos)
      stayWins++;
    else if (switchPos == carPos)
      switchWins++;
    else
      assert(0);  // Can't happen.
  }

  writefln("Switching/Staying wins: %d %d", switchWins, stayWins);
}
