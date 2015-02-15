> Calendar.Discordian.now()->format_ext_ymd();
 Result: "Pungenday, 59 Bureaucracy 3177"
> Calendar.Discordian.Day(Calendar.Day(2011,11,11))->format_ext_ymd();
 Result: "Setting Orange, 23 The Aftermath 3177"
> Calendar.Discordian.Day(Calendar.Badi.Day(168,13,9))->format_ext_ymd();
 Result: "Setting Orange, 23 The Aftermath 3177"
> Calendar.Day((Calendar.Discordian.Month()+1)->day(1));
 Result: Day(Thu 20 Oct 2011)
