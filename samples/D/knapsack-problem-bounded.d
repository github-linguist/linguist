import std.stdio, std.typecons, std.functional;

immutable struct Item {
    string name;
    int weight, value, quantity;
}

immutable Item[] items = [
    {"map",           9, 150, 1}, {"compass",      13,  35, 1},
    {"water",       153, 200, 3}, {"sandwich",     50,  60, 2},
    {"glucose",      15,  60, 2}, {"tin",          68,  45, 3},
    {"banana",       27,  60, 3}, {"apple",        39,  40, 3},
    {"cheese",       23,  30, 1}, {"beer",         52,  10, 3},
    {"suntan cream", 11,  70, 1}, {"camera",       32,  30, 1},
    {"t-shirt",      24,  15, 2}, {"trousers",     48,  10, 2},
    {"umbrella",     73,  40, 1}, {"w-trousers",   42,  70, 1},
    {"w-overcoat",   43,  75, 1}, {"note-case",    22,  80, 1},
    {"sunglasses",    7,  20, 1}, {"towel",        18,  12, 2},
    {"socks",         4,  50, 1}, {"book",         30,  10, 2}];

Tuple!(int,const(int)[]) chooseItem(in int iWeight, in int idx)
nothrow {
    alias memoize!chooseItem memoChooseItem;
    if (idx < 0)
        return typeof(return).init;

    int bestV;
    const(int)[] bestList;
    with (items[idx])
        foreach (i; 0 .. quantity + 1) {
            immutable wlim = iWeight - i * weight;
            if (wlim < 0)
                break;

            //const (val, taken) = memoChooseItem(wlim, idx - 1);
            const val_taken = memoChooseItem(wlim, idx - 1);
            if (val_taken[0] + i * value > bestV) {
                bestV = val_taken[0] + i * value;
                bestList = val_taken[1] ~ i;
            }
        }

    return tuple(bestV, bestList);
}

void main() {
    // const (v, lst) = chooseItem(400, items.length - 1);
    const v_lst = chooseItem(400, items.length - 1);

    int w;
    foreach (i, cnt; v_lst[1])
        if (cnt > 0) {
            writeln(cnt, " ", items[i].name);
            w += items[i].weight * cnt;
        }

    writeln("Total weight: ", w, " Value: ", v_lst[0]);
}
