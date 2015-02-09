
new object $location: $physical, $command_cache;

var $command_cache modules = [];
var $described prose = [];
var $has_commands shortcuts = #[];
var $has_name name = ['uniq, "Generic Container Object", "the Generic Container Object"];
var $location contents = [];
var $root created_on = 796268969;
var $root flags = ['methods, 'code, 'fertile, 'variables, 'core];
var $root inited = 1;
var $root managed = [$location];
var $root manager = $location;

public method .add_frob_to_contents() {
    arg frob;
    
    if (!(sender().is($thing_frob)))
        throw(~perm, "Caller is not $thing_frob.");
    if (type(frob) != 'frob)
        throw(~type, "Argument is not a frob.");
    if ((frob.location()) != this())
        throw(~location, "Sorry, but you're not here.");
    contents = (.contents()).setadd(frob);
};

public method .add_sender_to_contents(): nooverride  {
    if (caller() != $located)
        throw(~perm, "Caller is not $located.");
    if ((sender().location()) != this())
        throw(~location, "Sorry, but you're not here.");
    contents = setadd(contents, sender());
};

public method .add_to_contents(): nooverride  {
    arg what;
    
    if (caller() != $located)
        throw(~perm, "Caller is not $located.");
};

public method .announce() {
    arg str, @except;
    var obj, part, s;
    
    if ((type(str) == 'frob) && ((class(str) == $message_frob) && (this() in (str.parts()))))
        str = str.change_entry(this(), "general");
    s = sender();
    for obj in (.contents()) {
        if (!(obj in except))
            (| obj.tell(str, s) |);
    }
};

public method .contains() {
    arg obj;
    
    return (obj in (.contents())) ? 1 : 0;
};

public method .contents() {
    return contents || [];
};

public method .contents_accept_mail() {
    return 1;
};

public method .del_frob_from_contents() {
    arg frob;
    
    (sender().is($thing_frob)) || ((caller() != $foundation) && (> .perms(sender()) <));
    if (type(frob) != 'frob)
        throw(~type, "Argument not a frob.");
    contents = contents.setremove(frob);
};

public method .del_sender_from_contents() {
    if (caller() != $located)
        throw(~perm, "Caller not an agent of located protocol.");
    contents = setremove(contents, sender());
};

public method .did_arrive() {
    arg mover, place;
    
    if (caller() != $located)
        throw(~perm, "Caller is not $located.");
};

public method .did_leave() {
    arg mover, place;
    
    if (caller() != $located)
        throw(~perm, "Caller is not $located.");
};

public method .environment() {
    return [this()] + contents;
};

public method .find_in_contents() {
    arg str;
    var obj;
    
    for obj in (.contents()) {
        if (obj.match_name(str))
            return obj;
    }
    return 0;
};

root method .init_location() {
    contents = [];
};

public method .realm() {
    arg @args;
    var loc;
    
    loc = "";
    if ((| .location() |))
        loc = (.location()).realm();
    return ((loc + "[") + (.name())) + "]";
};

public method .realm_name() {
    return "";
};

root method .uninit_location() {
    var obj;
    
    for obj in (contents)
        (| obj.eject() |);
};

public method .validate_contents() {
    var obj, newcont;
    
    if (!(.is_writable_by(sender())))
        throw(~perm, "Must be a writer to validate contents");
    newcont = [];
    for obj in (contents) {
        if (valid(obj) && ((obj.has_ancestor($located)) && ((obj.location()) == this())))
            newcont = newcont.setadd(obj);
    }
    contents = newcont;
};

public method .will_arrive() {
    arg mover, old_place;
    
    if (caller() != $located)
        throw(~perm, "Caller is not $located.");
};

public method .will_leave() {
    arg mover, place;
    
    if (caller() != $located)
        throw(~perm, "Caller is not $located.");
};


