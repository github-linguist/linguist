import std.algorithm, std.range, std.string;

auto firstIndex(R, T)(R hay, T needle) {
  auto i = countUntil(hay, needle);
  if (i == -1)
    throw new Exception("No needle found in haystack");
  return i;
}

auto lastIndex(R, T)(R hay, T needle) {
  return walkLength(hay) - firstIndex(retro(hay), needle) - 1;
}

void main() {
  auto h = split("Zig Zag Wally Ronald Bush Krusty Charlie Bush Bozo");
  assert(firstIndex(h, "Bush") == 4);
  assert(lastIndex(h, "Bush") == 7);
}
