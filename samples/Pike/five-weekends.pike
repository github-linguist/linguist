int(0..1) weekends(object day)
{
    return (<5,6,7>)[day->week_day()];
}

int(0..1) has5(object month)
{
    return sizeof(filter(month->days(), weekends))==15;
}

object range = Calendar.Year(1900)->distance(Calendar.Year(2101));
array have5 = filter(range->months(), has5);

write("found %d months:\n%{%s\n%}...\n%{%s\n%}",
       sizeof(have5), have5[..4]->format_nice(), have5[<4..]->format_nice());

array rest = range->years() - have5->year();
write("%d years without any 5 weekend month:\n %{%d,%}\n", sizeof(rest), rest->year_no());
