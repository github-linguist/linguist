{Show {OS.time}}      %% posix time (seconds since 1970-01-01)
{Show {OS.gmTime}}    %% current UTC as a record
{Show {OS.localTime}} %% current local time as record

%% Also interesting: undocumented module OsTime
%% When did posix time reach 1 billion?
{Show {OsTime.gmtime 1000000000}}
{Show {OsTime.localtime 1000000000}}
