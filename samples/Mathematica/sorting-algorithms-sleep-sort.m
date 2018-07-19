SleepSort = RunScheduledTask[Print@#, {#, 1}] & /@ # &;
SleepSort@{1, 9, 8, 7, 6, 5, 3, 4, 5, 2, 0};
