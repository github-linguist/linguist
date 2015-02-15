mapping db = ([]);

mapping make_episode(string series, string title, string episode, array date)
{
    return ([ "series":series, "episode":episode, "title":title, "date":date ]);
}

void print_episode(mapping episode)
{
    write("  %-30s %10s %-30s (%{%d.%})\n",
          episode->series, episode->episode, episode->title, episode->date);
}

void print_series(mapping series)
{
    write("%-30s %-10s\n", series->series, series->status);
    map(series->episodes, print_episode);
}

void dump_db(mapping database)
{
    foreach(database; string name; mapping series)
    {
        print_series(series);
    }
}

array get_latest(mapping database)
{
    array latest = ({});
    foreach(database; string name; mapping series)
    {
       latest += ({ series->episodes[0] });
    }
    return latest;
}

int(0..1) compare_date(array a, array b)
{
    if (!arrayp(a) && !arrayp(b)) return false;
    if (!arrayp(a) || !sizeof(a)) return arrayp(b) && sizeof(b);
    if (!arrayp(b) || !sizeof(b)) return arrayp(a) && sizeof(a);
    if (a[0] == b[0]) return compare_date(a[1..], b[1..]);
    return a[0] < b[0];
}

int(0..1) compare_by_date(mapping a, mapping b)
{
    return compare_date(reverse(a->date), reverse(b->date));
}

void watch_list(mapping database)
{
    map(Array.sort_array(get_latest(database), compare_by_date),
        print_episode);
}

string prompt_read(string prompt)
{
    write("%s: ", prompt);
    return Stdio.stdin.gets();
}

array parse_date(string date)
{
    return (array(int))(date/".");
}

mapping prompt_for_episode()
{
    return make_episode(prompt_read("Series"),
                        prompt_read("Title"),
                        prompt_read("Episode"),
                        parse_date(prompt_read("Date watched")));
}

// pike offers encode_value() and decode_value() as standard ways
// to save and read data, but that is not a human readable format.
// therefore we are instead printing the structure as debug-output
// which is a readable form as long as it only contains integers,
// strings, mappings, arrays and multisets this format can be read by pike.
// to read it we are creating a class that contains the data as a value,
// which is then compiled and instantiated to allow us to pull the data out.
void save_db(string filename, mapping database)
{
    Stdio.write_file(filename, sprintf("%O", database));
}

void watch_save()
{
    save_db("pwatch", db);
}

mapping load_db(string filename)
{
    if (file_stat(filename))
        return compile_string("mixed data = " +
                              Stdio.read_file(filename) + ";")()->data;
    else return ([]);
}

mapping get_series(string name, mapping database)
{
    return database[name];
}

array get_episode_list(string series, mapping database)
{
    return database[series]->episodes;
}

void watch_new_series(string name, string status, mapping database)
{
    database[name] = (["series":name, "status":status, "episodes":({}) ]);
}

mapping get_or_add_series(string name, mapping database)
{
    if (!database[name])
    {
        string answer = prompt_read("Add new series? [y/n]: ");
        if (answer == "y")
            watch_new_series(name, "active", database);
    }
    return database[name];
}

void watch_add(mapping database)
{
    mapping episode = prompt_for_episode();
    string series_name = episode->series;
    mapping series = get_or_add_series(series_name, database);
    if (!series)
        watch_add(database);
    else
        series->episodes = Array.unshift(series->episodes, episode);
}

void watch_load()
{
    db = load_db("pwatch");
}

int main(int argc, array argv)
{
    watch_load();
    if (argc>1 && argv[1] == "add")
    {
        watch_add(db);
        watch_save();
    }
    else
        watch_list(db);
}
