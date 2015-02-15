#include <stdarg.h>

void logObjects(id firstObject, ...) // <-- there is always at least one arg, "nil", so this is valid, even for "empty" list
{
  va_list args;
  va_start(args, firstObject);
  id obj;
  for (obj = firstObject; obj != nil; obj = va_arg(args, id))
    NSLog(@"%@", obj);
  va_end(args);
}

// This function can be called with any number or type of objects, as long as you terminate it with "nil":
logObjects(@"Rosetta", @"Code", @"Is", @"Awesome!", nil);
logObjects(@4, @3, @"foo", nil);
