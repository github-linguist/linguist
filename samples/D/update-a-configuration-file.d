import std.stdio, std.file, std.string, std.regex, std.path,
       std.typecons;

final class Config {
    enum EntryType { empty, enabled, disabled, comment, ignore }

    static protected struct Entry {
        EntryType type;
        string name, value;
    }
    protected Entry[] entries;
    protected string path;

    this(in string path) {
        if (!isValidPath(path) || (exists(path) && !isFile(path)))
            throw new Exception("Invalid filename");

        this.path = path;
        if (!exists(path))
            return;

        auto r = regex(r"^(;*)\s*([A-Z0-9]+)\s*([A-Z0-9]*)", "i");
        auto f = File(path, "r");
        foreach (const buf; f.byLine()) {
            auto line = buf.strip().idup;
            if (!line.length)
                entries ~= Entry(EntryType.empty);
            else if (line[0] == '#')
                entries ~= Entry(EntryType.comment, line);
            else {
                line = line.removechars("^a-zA-Z0-9\x20;");
                auto m = match(line, r);
                if (!m.empty && m.front[2].length) {
                    EntryType t = EntryType.enabled;
                    if (m.front[1].length)
                        t = EntryType.disabled;
                    addOption(m.front[2], m.front[3], t);
                }
            }
        }
    }

    void enableOption(in string name) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].type = EntryType.enabled;
    }

    void disableOption(in string name) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].type = EntryType.disabled;
    }

    void setOption(in string name, in string value) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].value = value;
    }

    void addOption(in string name, in string val,
                   in EntryType t = EntryType.enabled) pure {
        entries ~= Entry(t, name.toUpper(), val);
    }

    void removeOption(in string name) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].type = EntryType.ignore;
    }

    Nullable!size_t getOptionIndex(in string name) const pure {
        foreach (immutable i, const ref e; entries) {
            if (e.type != EntryType.enabled &&
                e.type != EntryType.disabled)
                continue;
            if (e.name == name.toUpper())
                return typeof(return)(i);
        }
        return typeof(return).init;
    }

    void store() {
        auto f = File(path, "w+");
        foreach (immutable e; entries) {
            final switch (e.type) {
                case EntryType.empty:
                    f.writeln();
                    break;
                case EntryType.enabled:
                    f.writefln("%s %s", e.name, e.value);
                    break;
                case EntryType.disabled:
                    f.writefln("; %s %s", e.name, e.value);
                    break;
                case EntryType.comment:
                    f.writeln(e.name);
                    break;
                case EntryType.ignore:
                    continue;
            }
        }
    }
}

void main() {
    auto cfg = new Config("config.txt");
    cfg.enableOption("seedsremoved");
    cfg.disableOption("needspeeling");
    cfg.setOption("numberofbananas", "1024");
    cfg.addOption("numberofstrawberries", "62000");
    cfg.store();
}
