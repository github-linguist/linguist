
new object $located: $physical;

var $described prose = [];
var $has_name name = ['uniq, "Generic Located Object", "the Generic Located Object"];
var $located inited = 0;
var $located location = $nowhere;
var $located obvious = 1;
var $root created_on = 796268969;
var $root flags = ['methods, 'code, 'variables, 'core];
var $root managed = [$located];
var $root manager = $located;

protected method .did_move() {
    arg mover, old_place;
    
    // cleanup events  
    .unhook_events('move, old_place);
    old_place.send_event('movement, 'leave, location);
    location.send_event('movement, 'arrive, old_place);
    .hook_events('move);
};

public method .eject(): nooverride  {
    arg @args;
    var old, mover, place;
    
    [(place ?= $nowhere)] = args;
    if (!($sys.is_system(sender()))) {
        if (sender() == location)
            place = (.has_ancestor($body)) ? ($world.starting_place()) : $nowhere;
        else
            throw(~perm, ("Only system objects and " + (.name())) + "'s current location may call .eject()");
    }
    
    // Don't do anything if we're already here.
    if (place == location)
        return;
    if (!(place.has_ancestor($location)))
        throw(~type, (place.namef('ref)) + " is not a location.");
    if (place == this())
        throw(~move, "You cannot move something into itself.");
    if (!valid(location))
        location = $nowhere;
    mover = sender();
    
    // Set location.
    old = location;
    location = place;
    old.del_sender_from_contents();
    place.add_sender_to_contents();
    
    // Notify involved parties of completed move, in reverse order.
    place.did_arrive(mover, old);
    old.did_leave(mover, place);
    .did_move(mover, old);
};

public method .environment() {
    var env;
    
    env = setremove(location.environment(), this());
    return [this()] + env;
};

root method .init_located() {
    location = $void;
    location.add_sender_to_contents();
    obvious = 1;
};

public method .is_obvious_to() {
    arg whom;
    
    return .is_visible_to(whom);
};

public method .location() {
    return location || $void;
};

public method .match_environment() {
    arg str;
    var thing, matches;
    
    if (str == "here")
        return location;
    return (> pass(str) <);
};

public method .match_environment_all() {
    arg s;
    
    if (s == "here")
        return [location, @(> pass(@args) <)];
    else
        return (> pass(s) <);
};

public method .move_to(): nooverride  {
    arg place;
    var old, mover;
    
    // Don't do anything if we're already here.
    if (place == location)
        return;
    if (!(place.has_ancestor($location)))
        throw(~type, (place.namef('ref)) + " is not a location.");
    old = place;
    old = place;
    while (1) {
        if (old == this())
            throw(~move, "You cannot move something into itself.");
        old = (| old.location() |);
        if (!old)
            break;
    }
    if (!valid(location))
        location = $nowhere;
    mover = sender();
    (> .will_move(mover, place) <);
    (> location.will_leave(mover, place) <);
    (> place.will_arrive(mover, location) <);
    
    // Set location.
    old = location;
    location = place;
    old.del_sender_from_contents();
    place.add_sender_to_contents();
    
    // Notify involved parties of completed move, in reverse order.
    place.did_arrive(mover, old);
    old.did_leave(mover, place);
    .did_move(mover, old);
};

public method .obvious() {
    return obvious;
};

public method .realm() {
    return (.location()).realm();
};

public method .realm_name() {
    arg @args;
    
    return (.location()).realm_name(@args);
};

public method .set_obvious() {
    arg obv;
    
    .perms(sender());
    obvious = obv;
};

public method .uninit_guest_guest() {
    if (caller() != $root)
        throw(~perm, "Caller is not root.");
    location.del_sender_from_contents();
    location = 0;
};

root method .uninit_located() {
    var location;
    
    location = .location();
    if (valid(location)) {
        location.del_sender_from_contents();
        location.did_leave(this(), $nowhere);
    }
};

public method .will_move() {
    arg mover, place;
    
};


