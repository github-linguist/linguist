-- Due to Haskell's module system, textual includes are rarely needed. In
-- general, one will import a module, like so:
import SomeModule
-- For actual textual inclusion, alternate methods are available. The Glasgow
-- Haskell Compiler runs the C preprocessor on source code, so #include may be
-- used:
#include "SomeModule.hs"
