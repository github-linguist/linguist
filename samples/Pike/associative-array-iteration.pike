mapping(string:string) m = ([ "A":"a", "B":"b", "C":"c" ]);
foreach(m; string key; string value)
{
    write(key+value);
}
Result: BbAaCc

// only keys
foreach(m; string key;)
{
    write(key);
}
Result: BAC

// only values
foreach(m;; string value)
{
    write(value);
}
Result: bac
