class CSV
{
    mapping variables = ([]);

    mixed `->(string name)
    {
        return variables[name];
    }

    void `->=(string name, mixed value)
    {
        variables[name] = value;
    }

    array _indices()
    {
        return indices(variables);
    }
}

object csv = CSV();
csv->greeting = "hello world";
csv->count = 1;
csv->lang = "Pike";

indices(csv);
Result: ({ /* 3 elements */
              "lang",
              "count",
              "greeting"
         })
