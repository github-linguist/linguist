void main() {
    import std.stdio, std.algorithm, std.range, std.random;

    enum uint w = 14, h = 10;
    auto vis = new bool[][](h, w),
         hor = iota(h + 1).map!(_ => ["+---"].replicate(w)).array,
         ver = h.iota.map!(_ => ["|   "].replicate(w) ~ "|").array;

    void walk(in uint x, in uint y) /*nothrow*/ {
        vis[y][x] = true;
        foreach (p; [[x-1,y], [x,y+1], [x+1,y], [x,y-1]].randomCover) {
            if (p[0] >= w || p[1] >= h || vis[p[1]][p[0]]) continue;
            if (p[0] == x) hor[max(y, p[1])][x] = "+   ";
            if (p[1] == y) ver[y][max(x, p[0])] = "    ";
            walk(p[0], p[1]);
        }
    }
    walk(uniform(0, w), uniform(0, h));
    foreach (const a, const b; hor.zip(ver ~ []))
        join(a ~ ["+\n"] ~ b).writeln;
}
