#!/bin/env pike

int main(int argc, array(string) argv)
{
    object cal = Calendar;
    object year;
    string region = "us";

    array date = argv[1..];
    if (sizeof(date) && objectp(Calendar[date[0]]) && Calendar[date[0]]->Day)
    {
        cal = Calendar[date[0]];
        date = Array.shift(date)[1];
    }

    if (sizeof(date) && (int)date[0])
    {
        year = cal.Year((int)date[0]);
        date = Array.shift(date)[1];
    }

    if (sizeof(date))
        region = date[0];

    if (!year)
        year = cal.Year();

    print_year(year, region);
}

array make_month(object month, int field_width, void|string region)
{
    array out =({});
    mapping holidays = ([]);
    object today = Calendar.Day();

    if (region)
        holidays = Calendar.Events.find_region(region)->scan_events(month);

    array weekday_names =
        sprintf("%*.*s", field_width, field_width, month->week()->days()->week_day_shortname()[*]);

    out += ({ ({ month->month_name(), month->month_no(), month->year_name() }) });
    out += ({ weekday_names });
    out += showday(month->weeks()->days()[*][*], month, today, holidays, field_width);

    out += ({ ({ " "*field_width })*sizeof(weekday_names) });

    return out;
}

string print_month(object _month, void|int field_width, void|string region)
{
    if (!field_width)
        field_width = 2;
    array month = make_month(_month, field_width, region);
    string out = "";

    out += sprintf("%|*s\n", (field_width+1)*sizeof(month[1])-1, sprintf("%s", month[0][0]));
    out += sprintf((month[1..][*]*" ")*"\n");
    return out;
}

string print_year(object year, void|string region)
{
        array output = ({});
        int day_width = 2;
        int columns = Stdio.stdout.tcgetattr()->columns;
        int month_width = sizeof(make_month(year->month(), day_width)[1]) * (day_width+1) - 1;
        if (columns < month_width)
            columns = month_width;

        // try to find an optimal good looking solution to spread the months
        // across the terminal width
        // for the common calendar of 12 months this is easy but we need to
        // account for caledars that have more than 12 months
        float max_width = (float)((columns+2)/(month_width+2));
        float max_height = ceil(year->number_of_months()/max_width);
        float w = max_width;

        while(ceil(year->number_of_months()/(w-1)) == max_height)
            w--;

        foreach(print_month(year->months()[*], day_width, region)/w;; array row)
        {
            array rows = row[*]/"\n";
            int l = max(@sizeof(rows[*]));
            foreach(rows; int i;)
            {
                // the last line of each month is an empty line.
                // repeat the line as many times as needed to make the months equally long
                rows[i]+=({ rows[i][-1] })*(l-sizeof(rows[i]));
            }
            rows = Array.transpose(rows);
            output += rows[*]*"  ";
        }
        write("%*|s\n", sizeof(output[1]), year->format_nice());
        write(output * "\n");
        write("\n");
}

string showday(object day, object month, object today, mapping holidays, int field_width)
{
    string dayname;
    if (day->month() == month)
    {
        dayname = (string)day->month_day();
        dayname = " "*(sizeof((string)month->number_of_days())-sizeof(dayname))+dayname;
        if (day == today)
            dayname = sprintf("%|*.*s", field_width, field_width, dayname);
        else
            dayname = sprintf("%|*.*s", field_width, field_width, dayname);
        if (holidays[day])
            dayname = sprintf("%|s", dayname);
    }
    else
        dayname = " "*field_width;
    return dayname;
}
