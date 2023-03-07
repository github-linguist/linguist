export module A; // declares the primary module interface unit for named module 'A'
 
// hello() will be visible by translations units importing 'A'
export char const* hello() { return "hello"; } 
 
// world() will NOT be visible.
char const* world() { return "world"; }
 
// Both one() and zero() will be visible.
export
{
    int one()  { return 1; }
    int zero() { return 0; }
}
 
// Exporting namespaces also works: hi::english() and hi::french() will be visible.
export namespace hi
{
    char const* english() { return "Hi!"; }
    char const* french()  { return "Salut!"; }
}
