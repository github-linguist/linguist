#!/usr/bin/pike

Stdio.Readline readln = Stdio.Readline();

void print_help()
{
    write(#"Write a Story.

Names or objects in the story can be made variable by
referencing them as <person> <object>, etc.
End the story with an empty line.

Type show to read the story. You will be asked to fill the variables,
and the the story will be shown.

Type help to see this message again.
Type exit to quit.

");
}

void add_line(string input)
{
    array variables = parse_for_variables(input);
    write("Found variables: %{\"%s\" %}\n", variables);
    story += input+"\n";
}

array parse_for_variables(string input)
{
    array vars = Array.flatten(array_sscanf(input, "%*[^<>]%{<%[^<>]>%*[^<>]%}%*[^<>]"));
    return Array.uniq(vars);
}

mapping fill_variables(string story)
{
    array vars = parse_for_variables(story);
    mapping variables = ([]);
    foreach(vars;; string name)
    {
        string value = readln->read(sprintf("Please name a%s %s: ", (<'a','e','i','o','u'>)[name[1]]?"":"n", name));
        if (value != "")
            variables["<"+name+">"] = value;
    }
    return variables;
}

void show_story(string story)
{
    mapping variables = fill_variables(story);
    write("\n"+replace(story, variables));
}

void do_exit()
{
    exit(0);
}

mapping functions = ([ "help":print_help,
                       "show":show_story,
                       "exit":do_exit,
                     ]);
string story = "";

void main()
{
    Stdio.Readline.History readline_history = Stdio.Readline.History(512);
    readln->enable_history(readline_history);

    string prompt="> ";

    print_help();
    while(1)
    {
        string input=readln->read(prompt);
        if(!input)
            exit(0);
        if(input == "")
            show_story(story);
        else if (functions[input])
            functions[input]();
        else add_line(input);
    }
}
