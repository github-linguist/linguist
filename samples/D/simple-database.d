import std.stdio, std.algorithm, std.string, std.conv, std.array,
       std.file, std.csv, std.datetime;

private {
    immutable filename = "simdb.csv";

    struct Item {
        string name, date, category;
    }

    void addItem(in string[] item) {
        if (item.length < 3)
            return printUsage();
        auto db = load();
        const date = text(cast(DateTime)Clock.currTime);
        const cat = (item.length == 4) ? item[3] : "none";
        db ~= Item(item[2], date, cat);
        store(db);
    }

    void printLatest(in string[] a) {
       auto db = load();
       if (db.empty)
           return writeln("No entries in database.");
       db.sort!q{ a.date > b.date };
       if (a.length == 3) {
           foreach (item; db)
               if (item.category == a[2])
                   writefln("%s, %s, %s", item.tupleof);
       } else {
           writefln("%s, %s, %s", db[0].tupleof);
       }
    }

    void printAll() {
        auto db = load();
        if (db.empty)
            return writeln("No entries in database.");
        db.sort!q{ a.date < b.date };
        foreach (item; db)
            writefln("%s, %s, %s", item.tupleof);
    }

    Item[] load() {
        Item[] db;
        if (filename.exists && filename.isFile) {
            try {
                const text = filename.readText;
                if (!text.empty)
                    db = csvReader!Item(text).array;
            } catch (CSVException e) {
                writeln(e.msg);
            }
        }
        return db;
    }

    void store(in Item[] db) {
        auto f = File(filename, "w+");
        foreach (item; db)
            f.writefln("%s,%s,%s", item.tupleof);
    }

    void printUsage() {
        writeln(
`Usage:
  simdb cmd [categoryName]

  add     add item, followed by optional category
  latest  print last added item(s), followed by optional category
  all     print all

  For instance: add "some item name" "some category name"`);
    }
}

void main(in string[] args) {
    if (args.length < 2 || args.length > 4)
        return printUsage();

    switch (args[1].toLower) {
        case "add":    addItem(args);     break;
        case "latest": printLatest(args); break;
        case "all":    printAll();        break;
        default:       printUsage();      break;
    }
}
