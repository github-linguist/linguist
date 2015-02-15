# See section 7.5 of reference manual

# GAP has assertions levels. An assertion is tested if its level
# is less then the global level.

# Set global level
SetAssertionLevel(10);

a := 1;
Assert(20, a > 1, "a should be greater than one");
# nothing happens

a := 1;
Assert(4, a > 1, "a should be greater than one");
# error

# Show current global level
AssertionLevel();
# 10
