int(0..1) last_friday(object day)
{
   return day->week_day() == 5 &&
          day->month_day() > day->month()->number_of_days()-7;
}

int main(int argc, array argv)
{
    array days = filter(Calendar.Year((int)argv[1])->months()->days()[*], last_friday);
    write("%{%s\n%}", days->format_ymd());
    return 0;
}
