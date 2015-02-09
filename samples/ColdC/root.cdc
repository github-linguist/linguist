
object $root;

var $root child_index = 0;
var $root created_on = 0;
var $root credit = 0;
var $root defined_settings = #[["help-node", #[['get, ['help_node]], ['set, ['set_help_node_setting]], ['parse, ['parse_help_node_setting]], ['format, ['format_help_node_setting]]]]];
var $root flags = ['methods, 'code, 'core, 'variables];
var $root help_node = 0;
var $root inited = 0;
var $root managed = [];
var $root manager = $root;
var $root quota = 75000;
var $root quota_exempt = 0;
var $root semaphores = 0;
var $root settings = 0;
var $root trusted = [];
var $root trusted_by = 0;
var $root writers = [$root];
var $root writes = 0;

protected method ._all_defined_settings(): nooverride  {
    var a, all, d;
    
    all = #[];
    for a in (ancestors()) {
        catch any {
            for d in (dict_keys(a.defined_settings()))
                all = dict_add(all, d, a);
        }
    }
    return all;
};

private method ._clean_root() {
    arg v, m;
    var obj, value;
    
    if ((value = get_var(v))) {
        value = value.valid_objects();
        for obj in (value) {
            if (!(this() in obj.(m)()))
                value = value.setremove(obj);
            refresh();
        }
        if (value)
            set_var(v, value);
        else
            clear_var(v);
    }
};

private method ._clear_setting(): nooverride  {
    arg name;
    
    if (settings && dict_contains(settings, name)) {
        settings = dict_del(settings, name);
        if (!settings)
            clear_var('settings);
    }
};

public method ._display_ancestors() {
    arg space, checked, levels, maxlev;
    var c, anc, list, id, perms;
    
    id = ((space + this()) + " ") + ($object_lib.see_perms(this()));
    for anc in (dict_keys(checked)) {
        if (.has_ancestor(anc))
            return [];
    
        // [id + "  (above)"];
    }
    if (((.parents()).length()) > 1)
        id += " (MI)";
    list = [id];
    space += "  ";
    levels++;
    
    // check parents
    if ((!maxlev) || (maxlev != levels)) {
        for c in (.parents()) {
            list += c._display_ancestors(space, checked, levels, maxlev);
            checked = dict_add(checked, c, 1);
            pause();
        }
    }
    return list;
};

public method ._display_descendants() {
    arg space, checked, levels, maxlev;
    var c, anc, list, id, perms;
    
    id = ((space + this()) + " ") + ($object_lib.see_perms(this()));
    for anc in (dict_keys(checked)) {
        if (.has_ancestor(anc))
            return [];
    
        // [id + "  (above)"];
    }
    if (((.parents()).length()) > 1)
        id += " (MI)";
    list = [id];
    space += "  ";
    levels++;
    
    // check children
    if ((!maxlev) || (maxlev != levels)) {
        for c in (.children()) {
            list += c._display_descendants(space, checked, levels, maxlev);
            checked = dict_add(checked, c, 1);
            pause();
        }
    }
    return list;
};

public method ._get_method_info(): nooverride  {
    arg anc, method;
    var code, lines, dis_flag, meth_args, flags, first_comment;
    
    code = anc.list_method(method);
    lines = code.length();
    if (lines > 5)
        code = code.subrange(1, 5);
    flags = anc.method_flags(method);
    if (code) {
        meth_args = regexp(code[1], "arg ([^;]);");
        if (meth_args) {
            meth_args = meth_args[1];
            code = code.delete(1);
        } else {
            meth_args = "";
        }
        if (code && ((!(code[1])) || (((code[1])[1]) == "v")))
            code = code.delete(1);
        if (code && ((!(code[1])) || (((code[1])[1]) == "v")))
            code = code.delete(1);
        first_comment = code ? ((code[1]) + " ") : " ";
        first_comment = (((first_comment[1]) == "/") || ((first_comment[1]) == "r")) ? first_comment : "";
    } else {
        meth_args = "";
        first_comment = "";
    }
    return [anc, method, meth_args, flags, lines, first_comment];
};

public method .add_flag(): nooverride  {
    arg flag;
    
    (> .perms(sender(), 'manager) <);
    (> $sys.touch('coreok) <);
    if ((flag == 'core) && (!($sys.is_system(sender()))))
        throw(~perm, "Only system objects can set the 'core flag.");
    (flag == 'fertile) && (> .perms(sender(), 'manager) <);
    
    // let them add any flag they want
    flags = (flags || []).setadd(flag);
};

root method .add_managed_obj(): nooverride  {
    managed = (managed || []).setadd(sender());
};

public method .add_method() {
    arg code, name, @evalonly;
    var l, m, line, errs;
    
    (> .perms(sender()) <);
    if (evalonly && (caller() in [$root, $sys, $programmer]))
        (> $sys.touch('lockok) <);
    else
        (> $sys.touch() <);
    
    // check for a few things only admins can do
    if (!(sender().is($admin))) {
        for l in [1 .. listlen(code)] {
            line = code[l];
            if ((m = line.match_regexp("[^a-z0-9_.]anticipate_assignment\("))) {
                (> sender().tell($code_lib.point_to_line("ERROR: call to anticipate_assignment()", ((m[1])[1]) + 2, ((m[1])[2]) - 2, l, line)) <);
                throw(~perm, "anticipate_assignment() may only be used by an administrator.");
            }
        }
    }
    errs = (> add_method(code, name) <);
    if ('core in (.flags()))
        $changelog.log((((("ADD-METHOD: " + this()) + ".") + name) + " by ") + sender());
    for l in [1 .. listlen(errs)] {
        line = errs[l];
        if (match_regexp(line, "Line [0-9]+: Unknown function length.")) {
            line = substr(line, 1, strlen(line) - 1);
            errs = replace(errs, l, line + "(), try listlen(), strlen() or buflen()");
        }
    }
    return errs;
};

public method .add_parent() {
    arg parent;
    
    (> .perms(sender(), 'manager) <);
    (> $sys.touch() <);
    (> parent.will_inherit(sender()) <);
    if (.has_ancestor(parent))
        throw(~perm, ((this() + " already has ") + parent) + " as an ancestor.");
    .change_parents(parents() + [parent]);
};

public method .add_trusted(): nooverride  {
    arg obj;
    
    (caller() == definer()) || (> .perms(sender(), 'manager) <);
    (> $sys.touch('coreok) <);
    trusted = (trusted || []).setadd(obj);
    obj.add_trusted_obj();
};

root method .add_trusted_obj(): nooverride  {
    trusted_by = (trusted_by || []).setadd(sender());
};

public method .add_var(): nooverride  {
    arg name, @args;
    var tmp, kid;
    
    (> .perms(sender()) <);
    (> $sys.touch() <);
    if (!(tostr(name).valid_ident()))
        throw(~invident, name + " is not a valid ident.");
    (> add_var(name) <);
    if ('core in (.flags()))
        $changelog.log((((("ADD-VAR: " + this()) + ",") + name) + " by ") + sender());
    
    // The following code is a kludge as we can't send a default value  
    // to the primitive.   
    if (args) {
        tmp = tosym((("__set_" + tostr(name)) + "_") + time());
        catch any {
            add_method([((tostr(name) + " = ") + toliteral(args[1])) + ";"], tmp);
            .(tmp)();
            if (((args.length()) > 1) && ((args[2]) == 'inherit)) {
                for kid in (.descendants())
                    kid.(tmp)();
            }
            del_method(tmp);
        } with {
            del_method(tmp);
            rethrow(error());
        }
    }
};

root method .add_writable_obj(): nooverride  {
    writes = (writes || []).setadd(sender());
};

public method .add_writer(): nooverride  {
    arg obj;
    
    (> .perms(sender(), 'manager) <);
    (> $sys.touch('coreok) <);
    writers = (writers || []).setadd(obj);
    obj.add_writable_obj();
};

public method .all_defined_settings(): nooverride  {
    if (!(.has_flag('variables, sender())))
        throw(~perm, ((sender().namef('ref)) + " does not have permission to view settings on ") + (.name('ref)));
    return ._all_defined_settings();
};

public method .anc2(): nooverride  {
    var i, c, parent, d;
    
    d = #[];
    for parent in (parents())
        d = dict_add(d, parent, 1);
    while ((| (c = dict_keys(d)[++i]) |)) {
        for parent in (c.parents()) {
            pause();
            d = dict_add(d, parent, 1);
        }
        pause();
    }
    return dict_keys(d);
};

public method .ancestors(): nooverride  {
    arg @args;
    
    return ancestors(@args);
};

public method .ancestors_descending() {
    arg obj;
    var anc;
    
    return filter anc in (.ancestors()) where (anc.has_ancestor(obj));
};

public method .ancestors_to(): nooverride  {
    arg checked, levels, maxlev;
    var c, list;
    
    list = [this()];
    levels++;
    
    // check parents
    if ((!maxlev) || (maxlev != levels)) {
        for c in (.parents()) {
            list += [c.ancestors_to(checked, levels, maxlev)];
            checked = checked.setadd(c);
        }
    }
    return list;
};

public method .ancestry(): nooverride  {
    arg gen;
    var i, out;
    
    out = [];
    if (type(gen) == 'objnum) {
        for i in (.ancestors()) {
            if (i.has_ancestor(gen))
                out += [i];
        }
        return out;
    }
    if (gen != 0) {
        for i in (.parents())
            out = union(out, i.ancestry(gen - 1));
    }
    return out;
};

public method .as_this_run() {
    arg obj, method, args;
    
    (caller().is($http_interface)) || (> .perms(caller(), $scheduler, $page_set) <);
    return (> obj.(method)(@args) <);
};

public method .build_name() {
    arg @args;
    var output, type, part, rval;
    
    output = "";
    for part in (args) {
        type = type(part);
        if (type == 'list)
            output += (| .(part[1])(@part.subrange(2)) |) || "";
        else if (type == 'string)
            output += part;
    }
    return output;
};

public method .change_manager(): nooverride  {
    arg new;
    var old;
    
    if ((caller() != definer()) && (!($sys.is_system(sender()))))
        throw(~perm, "You must have system privileges to change the manager.");
    (> $sys.touch() <);
    if (type(new) != 'objnum)
        throw(~invarg, "Managers must be given as a single dbref.");
    old = .manager();
    manager = new;
    old.del_managed_obj();
    new.add_managed_obj();
};

root method .change_parents(): nooverride  {
    arg parents;
    var old_a, old_p, a, obj, new_a, definer, branch, method, str;
    
    if (!parents)
        throw(~noparents, "Objects must have at least 1 parent");
    
    // remember a few things
    old_a = setremove(ancestors(), this());
    old_p = .parents();
    branch = (.descendants()) + [this()];
    
    // build the new ancestors list by hand, so we can figure out what is
    // changing, and uninitialize those ancestors that are going away
    new_a = [];
    for obj in (parents)
        new_a = union(new_a, obj.ancestors());
    
    // uninit any ancestors going away
    for a in (old_a.set_difference(new_a)) {
        // call this ancestor's uninit on the obj and all its children
        method = tosym("uninit_" + (a.objname()));
        if ((definer = (| find_method(method) |))) {
            if (definer != a) {
                // scream madly and run around--everybody should know this
                str = "UNINIT ERROR: uninit method for " + a;
                str += (" in wrong place (" + definer) + ")";
                (| (definer.manager()).tell(str) |);
                (| (a.manager()).tell(str) |);
                (| sender().tell(str) |);
                continue;
            }
    
            // call the uninit method on this object only
            catch any {
                .(method)();
            } with {
                // try and let somebody know they made a boo-boo somewhere
                str = ((("UNINIT ERROR " + obj) + "<") + this()) + ">:";
                (| (.manager()).tell(str) |);
                (| (.manager()).tell_traceback(traceback()) |);
                (| sender().tell(str) |);
                (| sender().tell_traceback(traceback()) |);
                (| sender().tell("Continuing chparent..") |);
            }
        }
    
        // cleanup any old obj vars left lying around
        // if (branch)
        //     $sys.clear_definer_vars(a, branch);
        $sys.clear_definer_vars(a, [this()]);
        refresh();
    }
    
    // make the change
    (> chparents(parents) <);
    if ('core in (.flags()))
        $changelog.log(((((("CHANGE-PARENTS: " + this()) + " from ") + old_p) + " to ") + parents()) + " by ");
    new_a = setremove(ancestors(), this());
    
    // init anybody new to the family
    for a in (new_a.set_difference(old_a)) {
        method = tosym("init_" + (a.objname()));
        if ((definer = (| find_method(method) |))) {
            if (definer != a) {
                // scream madly and run around--everybody should know this
                // (damn inlaws, can't ever get things right)
                str = "INIT ERROR: init method for " + a;
                str += (" in wrong place (" + definer) + ")";
                (| (definer.manager()).tell(str) |);
                (| (a.manager()).tell(str) |);
                (| sender().tell(str) |);
                continue;
            }
    
            // introduce the new ancestor
            catch any {
                .(method)();
            } with {
                // try and let somebody know they made a boo-boo somewhere
                str = ((("INIT ERROR " + obj) + "<") + this()) + ">:";
                (| (.manager()).tell(str) |);
                (| (.manager()).tell_traceback(traceback()) |);
                (| sender().tell(str) |);
                (| sender().tell_traceback(traceback()) |);
                (| sender().tell("Continuing chparent..") |);
            }
        }
        refresh();
    }
};

public method .children(): nooverride  {
    return children();
};

public method .chparents() {
    arg @parents;
    var parent, cur;
    
    if (!(| .perms(sender(), 'manager) |))
        (> .perms(caller(), $root, $sys) <);
    (> $sys.touch() <);
    if (!parents)
        throw(~noparents, "There must be at least 1 parent for each object.");
    
    // Notify new parents of impending change.
    cur = parents();
    for parent in (parents) {
        if (!(parent in cur))
            (> parent.will_inherit(sender()) <);
    }
    
    // Everything's okay, go ahead and try it.
    .change_parents(parents);
};

public method .clean_root() {
    var obj;
    
    // Called by $sys.clean_database()
    (> .perms(caller(), $sys) <);
    (| ._clean_root('trusted, 'trusted_by) |);
    (| ._clean_root('trusted_by, 'trusted) |);
    (| ._clean_root('writers, 'writes) |);
    (| ._clean_root('writes, 'writers) |);
    if (!manager) {
        manager = this();
        .change_manager($reaper);
    }
    if (managed) {
        managed = managed.valid_objects();
        for obj in (managed) {
            refresh();
            if ((obj.manager()) != this())
                managed = setremove(managed, obj);
        }
        if (!managed)
            clear_var('managed);
    }
};

public method .clear_setting(): nooverride  {
    arg name, definer;
    var info, args;
    
    (caller() == definer()) || (> .perms(sender()) <);
    info = (> definer.setting_info(name) <);
    if (dict_contains(info, 'clear)) {
        args = sublist(info['clear], 2);
        (> .((info['clear])[1])(name) <);
    } else if (settings && dict_contains(settings, name)) {
        settings = dict_del(settings, name);
        if (!settings)
            clear_var('settings);
    }
};

public method .clear_variable(): nooverride  {
    arg name;
    var n, obj;
    
    (> .perms(sender()) <);
    n = tosym("_clear_var_" + tostr(time()));
    catch any {
        .add_method([("clear_var(" + toliteral(name)) + ");"], n);
        for obj in (.descendants()) {
            (| obj.(n)() |);
            pause();
        }
        (| del_method(n) |);
    } with {
        (| del_method(n) |);
    }
};

root method .core_root() {
    (| clear_var('child_index) |) || (child_index = 0);
};

public method .corify(): nooverride  {
    var d;
    
    if (sender() != $sys)
        throw(~sysonly, "This should only be called by $sys.make_core().");
    
    // reverse engineer it--if coreify_<object> exists,
    // call it on this object and all of its descendants.
    for d in (((.descendants()).setremove($sys)) + [$sys]) {
        .corify_descendants_of(d);
        refresh();
    }
};

private method .corify_descendants_of(): nooverride  {
    arg obj;
    var d, name, l;
    
    name = (| tosym("core_" + (obj.objname())) |);
    catch ~methodnf {
        if ((> obj.find_method(name) <) != obj) {
            $sys.log(((("** Coremethod for " + obj) + " in wrong place (on ") + (obj.find_method(name))) + ") **");
            return;
        }
    } with {
        return;
    }
    for d in ([obj] + (obj.descendants())) {
        catch any {
            (> d.(name)() <);
        } with {
            $sys.log(((("** ERROR encountered in " + d) + ".") + name) + "():");
            for l in ($parse_lib.traceback(traceback()))
                $sys.log(l);
        }
        refresh();
    }
};

public method .created_on(): nooverride  {
    return created_on;
};

public method .credit() {
    var line;
    
    if ('core in (flags || [])) {
        line = "Created ";
        if ((.created_on()) > 0)
            line += ($time.format("%d-%h-%Y", .created_on())) + " ";
        line = [line + "as a part of ColdCore, see: @help Credit"];
    } else {
        line = [];
    }
    if (credit)
        return credit + line;
    return line;
};

public method .data(): nooverride  {
    arg @parent;
    var par, data, out;
    
    if (!(.has_flag('variables, sender())))
        throw(~perm, ((sender().namef('ref)) + " is not allowed to read variables on ") + (.namef('ref)));
    if (parent) {
        if (type(parent[1]) != 'objnum)
            throw(~type, (parent[1]) + " is not an object.");
        return (> data(parent[1]) <);
    } else {
        data = (> data() <);
        out = #[];
        for par in (data) {
            // if the parent doesn't exist anymore, just let them see the data.
            if ((!valid(par[1])) || ((par[1]).has_flag('variables, sender())))
                out = out.add(par[1], par[2]);
            else
                out = out.add(par[1], ["*** Permission Denied ***"]);
        }
        return out;
    }
};

public method .debug() {
    arg @stuff;
    var x, line, mngr, meth, stack, out, ref;
    
    stack = stack();
    meth = (| (((stack[2])[3]) + "() line ") + ((stack[2])[4]) |);
    
    // dont even bother
    mngr = .manager();
    if (!(| mngr.find_method('tell) |))
        return;
    if (sender() != caller())
        ref = ((sender() + "<") + caller()) + ">";
    else
        ref = sender();
    if (meth)
        line = ((("DEBUG " + ref) + ".") + meth) + ":";
    else
        line = ("DEBUG " + ref) + ":";
    mngr.tell(line);
    for x in (stuff) {
        refresh();
        mngr.tell(($data_lib.unparse_indent(x)).prefix("DEBUG    "));
    }
};

public method .define_setting(): nooverride  {
    arg name, @info;
    var i;
    
    (> .perms(sender()) <);
    if (info) {
        info = info[1];
        for i in (info) {
            if (!(> .valid_setting_attr(@i) <))
                info = dict_del(info, i[1]);
        }
    } else {
        info = #[];
    }
    if ((.all_defined_settings()).contains(name))
        throw(~setexists, ("Setting \"" + name) + "\" is already defined.");
    if (!($code_lib.valid_setting_id(name)))
        throw(~setbad, ("Setting name \"" + name) + "\" is unacceptable.");
    defined_settings = (.defined_settings()).add(name, info);
    return defined_settings[name];
};

public method .defined_settings(): nooverride  {
    return defined_settings || #[];
};

public method .del_flag(): nooverride  {
    arg flag;
    
    (> .perms(sender(), 'manager) <);
    (> $sys.touch('coreok) <);
    if ((flag == 'core) && (!($sys.writable_core())))
        throw(~perm, this() + " is a core object, and the core isn't writable.");
    
    // let them add any flag they want
    flags = (flags || []).setremove(flag);
};

root method .del_managed_obj(): nooverride  {
    managed = (managed || []).setremove(sender());
    if (!managed)
        clear_var('managed);
};

public method .del_method(): nooverride  {
    arg name;
    
    (> .perms(sender()) <);
    (> del_method(name) <);
    if ('core in (.flags()))
        $changelog.log((((("DEL-METHOD: " + this()) + ",") + name) + " by ") + sender());
};

public method .del_parent() {
    arg parent;
    var parents;
    
    (> .perms(sender(), 'manager) <);
    (> $sys.touch() <);
    if (!valid(parent))
        throw(~type, "Not a valid parent, must send a valid object.");
    parents = .parents();
    if (!(parent in parents))
        throw(~parentnf, ((parent + " is not a parent of ") + this()) + ".");
    parents = parents.setremove(parent);
    (> .change_parents(parents) <);
};

public method .del_trusted(): nooverride  {
    arg obj;
    
    (> .perms(sender(), 'manager) <);
    (> $sys.touch('coreok) <);
    trusted = (trusted || []).setremove(obj);
    if (!trusted)
        clear_var('trusted);
    obj.del_trusted_obj();
};

root method .del_trusted_obj(): nooverride  {
    trusted_by = (trusted_by || []).setremove(sender());
    if (!trusted_by)
        clear_var('trusted_by);
};

public method .del_var(): nooverride  {
    arg name;
    var n, obj, errs;
    
    (caller() == definer()) || (> .perms(sender()) <);
    (> $sys.touch() <);
    
    // try and clear the variable on all of the descendants, before 
    // deleting the variable...since we are root, dont use the standard  
    // hooks as perms may not be right
    n = tosym("_dclear_var_" + tostr(time()));
    catch any {
        add_method([("clear_var('" + name) + ");"], n);
        for obj in (.descendants()) {
            (| obj.(n)() |);
            pause();
        }
    }
    (| del_method(n) |);
    
    // now delete the variable
    (> del_var(name) <);
    if ('core in (.flags()))
        $changelog.log((((("DEL-VAR: " + this()) + ",") + name) + " by ") + sender());
};

root method .del_writable_obj(): nooverride  {
    writes = (writes || []).setremove(sender());
    if (!writes)
        clear_var('writes);
};

public method .del_writer(): nooverride  {
    arg obj;
    
    (caller() == definer()) || (> .perms(sender(), 'manager) <);
    writers = (writers || []).setremove(obj);
    (> $sys.touch('coreok) <);
    if (!writers)
        (| clear_var('writers) |);
    obj.del_writable_obj();
};

public method .descendants(): nooverride  {
    var kids, i, c, child, d;
    
    d = #[];
    for child in (children())
        d = dict_add(d, child, 1);
    while ((| (c = dict_keys(d)[++i]) |)) {
        for child in (c.children()) {
            pause();
            d = dict_add(d, child, 1);
        }
        pause();
    }
    return dict_keys(d);
};

public method .descendants_to(): nooverride  {
    arg checked, levels, maxlev;
    var c, list;
    
    list = [this()];
    levels++;
    
    // check parents
    if ((!maxlev) || (maxlev != levels)) {
        for c in (.children()) {
            list += [c.descendants_to(checked, levels, maxlev)];
            checked = checked.setadd(c);
        }
    }
    return list;
};

public method .destroy(): nooverride  {
    // This doesn't actually destroy us immediately, but we will go away when
    // nothing is holding onto us any more.
    (> .perms(sender(), 'manager) <);
    if (.has_flag('core))
        throw(~perm, "This object is a core object, and cannot be destroyed!");
    (| .uninitialize('destroy) |);
    destroy();
};

public method .eval(): nooverride  {
    arg code, @dest;
    var errors, result, method;
    
    dest = dest ? (dest[1]) : this();
    if (!(sender() in [$scheduler, $root]))
        (> .perms(sender()) <);
    
    // Compile the code.
    method = tosym("tmp_eval_" + tostr(time()));
    errors = .add_method(code, method);
    if (errors)
        return ['errors, errors, 0, 0];
    
    // Evaluate the expression.  Be sure to remove it afterwards, so that no
    // one else can call it.
    catch any {
        result = (> dest.(method)() <);
    } with {
        (| del_method(method) |);
        rethrow(error());
    }
    (| del_method(method) |);
    return ['result, result];
};

public method .examine() {
    return [];
};

public method .find_method(): nooverride  {
    arg name;
    
    if (!(.has_flag('methods, sender())))
        throw(~perm, (sender() + " doesn't have permission to find methods on ") + this());
    return (> find_method(name) <);
};

public method .find_next_method(): nooverride  {
    arg name, after;
    
    if (!(.has_flag('methods, sender())))
        throw(~perm, (sender() + " doesn't have permission to find methods on ") + this());
    return (> find_next_method(name, after) <);
};

public method .flags(): nooverride  {
    return flags || [];
};

public method .format_descendants() {
    arg ind, done, depth, max, yes, no, above;
    var c, anc, list, id, perms, f, show, myflags;
    
    myflags = .flags();
    show = 1;
    if (yes) {
        for f in (yes) {
            if (!(f in myflags)) {
                show = 0;
                break;
            }
        }
    }
    if (no) {
        for f in (myflags) {
            if (f in no) {
                show = 0;
                break;
            }
        }
    }
    if (show) {
        id = ((ind + this()) + " ") + ($object_lib.see_perms(this()));
        for anc in (dict_keys(done)) {
            if (has_ancestor(anc))
                return above ? [id + " (ABOVE)"] : [];
        }
        if (listlen(parents()) > 1)
            id += " (MI)";
        list = [id];
    } else {
        list = [];
    }
    ind += "  ";
    depth++;
    
    // check children
    if ((!max) || (max != depth)) {
        for c in (children()) {
            list += c.format_descendants(ind, done, depth, max, yes, no, above);
            done = dict_add(done, c, 1);
            pause();
        }
    }
    return list;
};

public method .format_help_node_setting() {
    arg node;
    
    if (node)
        return node.namef('ref);
    return "";
};

public method .format_setting(): nooverride  {
    arg name, definer, value;
    var i, args;
    
    i = definer.setting_info(name);
    if (dict_contains(i, 'format)) {
        args = sublist(i['format], 2);
        catch ~methodnf
            return (> .((i['format])[1])(value, @args) <);
        with
            return (> $settings.((i['format])[1])(value, @args) <);
    }
    if (type(value) == 'objnum)
        return value.namef('ref);
    else
        return "" + value;
};

public method .generations(): nooverride  {
    arg gen;
    var p, out;
    
    out = [this()];
    if (gen != 0) {
        for p in (.parents())
            out = union(out, p.generations(gen - 1));
    }
    return out;
};

public method .get_default_setting(): nooverride  {
    arg name;
    
    return settings[name];
};

public method .get_local_setting(): nooverride  {
    arg name, definer;
    var i;
    
    i = definer.setting_info(name);
    if (dict_contains(i, 'access))
        (> .((i['access])[1])(name, sender(), caller(), @sublist(i['access], 2)) <);
    return (| settings[name] |) || (> definer.get_default_setting(name) <);
};

public method .get_obj_suffix() {
    arg @suffix;
    var objname, tmp;
    
    // Figure out the suffix from the arguments and child index.
    [(suffix ?= 0)] = suffix;
    if (suffix) {
        // so they dont confuse child_index:
        if (suffix.is_numeric())
            throw(~perm, "You cannot specify a numeric suffix.");
    
        // so we get correct symbols & it is always lowercase:
        suffix = lowercase(strsed(suffix, "[^a-z0-9_]+", "", "g"));
    } else {
        // get the next valid objname
        objname = tostr((| .objname() |) || "unknown");
        tmp = tosym(objname);
        while ((| lookup(tmp) |)) {
            child_index++;
            tmp = tosym((objname + "_") + tostr(child_index));
        }
        suffix = tostr(child_index);
    }
    return suffix;
};

public method .get_quota(): nooverride  {
    arg @args;
    
    return quota;
};

public method .get_setting(): nooverride  {
    arg name, definer;
    var i;
    
    i = definer.setting_info(name);
    if (dict_contains(i, 'access))
        (> .((i['access])[1])(name, sender(), caller(), @sublist(i['access], 2)) <);
    if (dict_contains(i, 'get))
        return (> .((i['get])[1])(name, definer, @sublist(i['get], 2)) <);
    catch any
        return settings[name];
    with
        return (> definer.get_default_setting(name) <);
};

public method .get_setting_attr(): nooverride  {
    arg name, attr;
    
    return (defined_settings[name])[attr];
};

public method .has_ancestor(): nooverride  {
    arg obj;
    
    return has_ancestor(obj);
};

public method .has_flag(): nooverride  {
    arg flag, @sender;
    
    [(sender ?= sender())] = sender;
    if (flag == 'core)
        return flag in (.flags());
    return (flag in (.flags())) || (.trusts(sender));
};

public method .help_node() {
    arg @args;
    
    return help_node;
};

public method .hname() {
    arg @args;
    
    return ((("<a href=\"/bin/display?" + this()) + "\">") + this()) + "</a>";
};

root method .init_root(): nooverride  {
    .change_manager(this());
    flags = ['variables, 'methods, 'code];
    created_on = time();
};

public method .initialize(): nooverride  {
    var ancestors, pos, len, method, a, def, str, tb;
    
    // called by $sys.create() to create a new object.
    if ((caller() != $sys) && (sender() != this()))
        throw(~perm, "Caller is not $sys and sender is not this.");
    if (inited)
        throw(~perm, "Already initialized.");
    ancestors = ancestors().reverse();
    for a in (ancestors) {
        refresh();
        if (!(method = (| tosym("init_" + tostr(a.objname())) |)))
            continue;
        if ((def = (| find_method(method) |))) {
            if (def != a) {
                (| (def.manager()).tell(((("Initialization method for " + a) + " in wrong place (") + find_method(method)) + ")") |);
            } else {
                catch any {
                    .(method)();
                } with {
                    // try and let somebody know they made a boo-boo somewhere
                    str = ((("UNINIT ERROR " + this()) + "<") + def) + ">:";
                    tb = traceback().fmt_tb();
                    if (def) {
                        (| (def.manager()).tell(str) |);
                        (| (def.manager()).tell(tb) |);
                    }
                    if ((.manager()) != sender()) {
                        (| sender().tell(str) |);
                        (| sender().tell(tb) |);
                    }
                    (| sender().tell("Continuing init..") |);
                    (| $sys.log_traceback(tb, "** " + str) |);
                }
            }
        }
    }
    inited = 1;
};

public method .is(): nooverride  {
    arg @args;
    
    if (!args)
        throw(~invarg, "Must have one argument.");
    if (type(args[1]) == 'dictionary)
        return has_ancestor(args[2]);
    return has_ancestor(args[1]);
};

public method .is_of(): nooverride  {
    arg obj;
    
    return obj in ancestors();
};

public method .is_writable_by(): nooverride  {
    arg obj;
    var o;
    
    for o in (.writers()) {
        if (o.has_ancestor($group)) {
            if (o.includes(obj))
                return 1;
        } else if (o == obj) {
            return 1;
        }
    }
    return $sys.is_system(obj);
    
    // return (| obj in .writers() |) || $sys.is_system(obj);
};

public method .list_method(): nooverride  {
    arg @args;
    
    if (!(.has_flag('code, sender())))
        throw(~perm, (("Method code on " + (.namef('ref))) + " is not readable by ") + (sender().namef('ref)));
    return (> list_method(@args) <);
};

public method .list_methods() {
    arg gen, def, fltr, match;
    var ancs, a, m, i, methods;
    
    if (!(.has_flag('methods, sender())))
        throw(~perm, (sender() + " cannot view methods on ") + this());
    if (def)
        ancs = [def];
    else
        ancs = .(gen[1])(gen[2]) || [this()];
    methods = #[];
    for a in (ancs) {
        for m in (a.methods()) {
            if (tostr(m).(match)(fltr) != 0) {
                i = a.method_info(m);
                methods = methods.add_elem(i[5], [a, m, @i]);
            }
        }
    }
    return methods;
};

public method .managed(): nooverride  {
    return managed || [];
};

public method .manager(): nooverride  {
    return manager || $reaper;
};

public method .match_children() {
    arg string;
    var children, child_names, c;
    
    children = .children();
    child_names = children.mmap('name);
    
    // direct matches first.
    for c in (child_names) {
        if (c == string)
            return children[c in child_names];
    }
    
    // ok, try partial matches
    for c in (child_names) {
        if ($string.match_begin(c, string))
            return children[c in child_names];
    }
    return 0;
};

public method .match_descendants() {
    arg string;
    var match, child;
    
    match = .match_children(string);
    if (match)
        return match;
    for child in (.children()) {
        match = child.match_descendants(string);
        if (match)
            return match;
    }
    return 0;
};

public method .method_access(): nooverride  {
    arg method;
    
    if (!(.has_flag('methods, sender())))
        throw(~perm, (sender() + " doesn't have permission to find methods on ") + this());
    return (> method_access(method) <);
};

public method .method_bytecode() {
    arg method;
    
    return 'no;
    return (> method_bytecode(method) <);
};

public method .method_flags(): nooverride  {
    arg method;
    
    if (!(.has_flag('methods, sender())))
        throw(~perm, (sender() + " doesn't have permission to find methods on ") + this());
    return (> method_flags(method) <);
};

public method .method_info(): nooverride  {
    arg @args;
    
    return (> method_info(@args) <);
};

public method .methods(): nooverride  {
    if (!(.has_flag('methods, sender())))
        throw(~perm, (sender() + " doesn't have permission to find methods on ") + this());
    return methods();
};

public method .name() {
    arg @args;
    
    return tostr(this());
};

public method .namef() {
    arg type;
    
    return tostr(this());
};

public method .new() {
    arg @suffix;
    var obj, tmp, objname, mngr, na;
    
    // this difference between this and .spawn() is that you
    // can override this method to return a frob instead.
    (> .will_spawn(sender()) <);
    suffix = (> .get_obj_suffix(@suffix) <);
    return $sys.spawn_sender(suffix, sender());
};

public method .nop() {
    // the no operation, operation, woowoo
};

public method .objname(): nooverride  {
    return (> objname() <);
};

public method .objnum(): nooverride  {
    return objnum();
};

public method .parents(): nooverride  {
    return parents();
};

public method .parse_help_node_setting() {
    arg value, @args;
    
    if (!value)
        return 0;
    value = (> $object_lib.to_dbref(value) <);
    if (!(value.is($help_node)))
        throw(~perm, (value.namef('ref)) + " is not a $help_node.");
    return value;
};

root method .pass_test() {
    return "YYYYYYY";
};

public method .perms(): nooverride  {
    arg what, @args;
    var flag;
    
    if (!args)
        args = ['writer];
    if (type(args[1]) == 'symbol) {
        switch (args[1]) {
            case 'system:
                if (!($sys.is_system(what)))
                    throw(~perm, ("Permission Denied: " + what) + " is not of the system.", what);
            case 'manager:
                if (((.manager()) != what) && (!($sys.is_system(what))))
                    throw(~perm, ("Permission Denied: " + what) + " is not the manager.", what);
            case 'trusts:
                if (!(.trusts(what)))
                    throw(~perm, ("Permission Denied: " + what) + " is not a trustee.", what);
            case 'command:
                if ((what != $user) && (what != $body))
                    throw(~perm, what + " is not a valid command invoking object.");
            default:
                if (!(.is_writable_by(what)))
                    throw(~perm, ("Permission Denied: " + what) + " is not a writer.", what);
        }
    } else if (type(what) == 'objnum) {
        if (!(what in args))
            throw(~perm, (what + " is not one of: ") + ((args.mmap('namef, 'ref)).to_english()), what);
    }
};

public method .poll_semaphore(): nooverride  {
    arg type;
    var t;
    
    if (!(caller().has_flag('core)))
        return;
    while (1) {
        if (!semaphores) {
            semaphores = #[[type, task_id()]];
            return;
        }
        if ((type((| (t = semaphores[type]) |)) == 'error) || (!(t in tasks()))) {
            semaphores = dict_add(semaphores, type, task_id());
            return;
        }
        pause();
        pause();
        pause();
    }
};

public method .poll_semaphores() {
    arg type, @s;
    var i;
    
    if (!(caller().has_flag('core)))
        return;
    refresh();
    while (find i in (s) where (i.query_semaphore(type))) {
        pause();
        pause();
        pause();
    }
    for i in (s)
        i.poll_semaphore(type);
};

public method .query_semaphore() {
    arg type;
    var t;
    
    return (type((| (t = semaphores[type]) |)) != 'error) && ((t in tasks()) && (t != task_id()));
};

public method .quota(): nooverride  {
    return quota;
};

public method .quota_byte_usage(): nooverride  {
    var total, obj;
    
    for obj in (.managed()) {
        if (!valid(obj))
            continue;
        total += obj.size();
    }
    return total;
};

public method .quota_exempt(): nooverride  {
    return quota_exempt;
};

public method .quota_valid(): nooverride  {
    if (quota_exempt)
        return 1;
    if (!(.is($user)))
        return 0;
    return (.quota_byte_usage()) < (.quota());
};

public method .release_semaphore(): nooverride  {
    arg type;
    
    if ((| (semaphores[type]) == task_id() |))
        semaphores = dict_del(semaphores, type);
    if (!semaphores)
        clear_var('semaphores);
};

public method .rename_method() {
    arg now, new;
    
    (> .perms(sender()) <);
    (> $sys.touch() <);
    return (> rename_method(now, new) <);
};

public method .set_credit() {
    arg newcopy;
    var x;
    
    (> .perms(sender(), 'manager) <);
    if (newcopy == 0)
        return (| clear_var('credit) |);
    if ((type(newcopy) != 'list) || find x in (newcopy) where (type(x) != 'string))
        throw(~type, "You must submit a list of strings for the credit.");
    credit = newcopy;
};

public method .set_flags(): nooverride  {
    arg new_flags;
    
    (> .perms(sender(), 'manager) <);
    (> $sys.touch() <);
    if (type(new_flags) != 'list)
        throw(~invflags, "Flags must be submitted as a list of symbols.");
    if ((!new_flags) && flags)
        return clear_var('flags);
    if ((('core in new_flags) && (!('core in (.flags())))) && (!($sys.is_system(sender()))))
        throw(~perm, "Only system objects can set the 'core flag.");
    if (('fertile in new_flags) && (!('fertile in flags)))
        (> .perms(sender(), 'manager) <);
    flags = new_flags;
};

protected method .set_help_node_setting() {
    arg name, definer, value;
    
    if (!value)
        (| clear_var('help_node) |);
    else
        help_node = value;
};

public method .set_method_access(): nooverride  {
    arg method, state;
    
    if (!(.is_writable_by(sender())))
        throw(~perm, (sender() + " cannot write to ") + this());
    (> $sys.touch() <);
    return (> set_method_access(method, state) <);
};

public method .set_method_flags(): nooverride  {
    arg method, flags;
    var current;
    
    if (!(.is_writable_by(sender())))
        throw(~perm, (sender() + " cannot write to ") + this());
    (> $sys.touch() <);
    current = method_flags(method);
    if ((('locked in flags) && (!('locked in current))) && (!($sys.is_system(sender()))))
        throw(~perm, "Only administrators can set the locked method flag.");
    if ((('nooverride in flags) && (!('nooverride in current))) && (!($sys.is_system(sender()))))
        throw(~perm, "Only administrators can set the nooverride method flag.");
    return set_method_flags(method, flags);
};

public method .set_objname() {
    arg objname;
    
    (> .perms(sender()) <);
    if (.has_flag('core))
        throw(~perm, this() + " is a core object; you cannot change its object name!");
    (> $sys.touch() <);
    
    // Make sure first argument is a symbol.
    if (type(objname) != 'symbol)
        throw(~type, "New objname is not a symbol.");
    
    // Make sure everything is lowercase.
    objname = tosym(tostr(objname).lowercase());
    
    // Do nothing if objname isn't different.
    if (objname == (| objname() |))
        return;
    return (> set_objname(objname) <);
};

public method .set_quota(): nooverride  {
    arg value;
    
    (> .perms(caller(), $user, @$sys.system(), $root, $admin) <);
    quota = value;
};

public method .set_quota_exempt(): nooverride  {
    arg bool;
    
    (> .perms(sender(), $user, @$sys.system(), $root) <);
    if (bool)
        quota_exempt = 1;
    else
        (| clear_var('quota_exempt) |);
};

public method .set_setting(): nooverride  {
    arg name, definer, value;
    var i, args;
    
    (> .perms(sender()) <);
    i = (> definer.setting_info(name) <);
    if (dict_contains(i, 'parse)) {
        args = sublist(i['parse], 2);
        if ((| find_method((i['parse])[1]) |))
            value = (> .((i['parse])[1])(value, @args) <);
        else
            value = (> $settings.((i['parse])[1])(value, @args) <);
    }
    if (dict_contains(i, 'set))
        (> .((i['set])[1])(name, definer, value, @sublist(i['set], 2)) <);
    else
        settings = dict_add(settings || #[], name, value);
};

public method .set_setting_attr(): nooverride  {
    arg name, attr, value;
    var info;
    
    (> .perms(sender()) <);
    if ((!defined_settings) || (!dict_contains(defined_settings, name)))
        throw(~setnf, (("Setting \"" + name) + "\" is not defined on ") + this());
    if (value && (!(> .valid_setting_attr(attr, value) <)))
        return;
    info = defined_settings[name];
    if (!value)
        info = dict_del(info, attr);
    else
        info = dict_add(info, attr, value);
    defined_settings = dict_add(defined_settings, name, info);
};

public method .setting_definer(): nooverride  {
    arg name;
    var a;
    
    for a in (ancestors()) {
        if (dict_contains(a.defined_settings(), name))
            return a;
    }
    throw(~setnf, ("Setting \"" + name) + "\" is not defined.");
};

public method .setting_info(): nooverride  {
    arg setting;
    
    return defined_settings[setting];
};

public method .settings(): nooverride  {
    (> .perms(sender()) <);
    return settings || #[];
};

public method .size(): nooverride  {
    arg @args;
    
    [(args ?= 'int)] = args;
    switch (args) {
        case 'string:
            return tostr(size());
        case 'english:
            return size().to_english();
        default:
            return size();
    }
};

public method .spawn(): nooverride  {
    arg @suffix;
    var obj, tmp, objname, mngr, na;
    
    (> .will_spawn(sender()) <);
    suffix = (> .get_obj_suffix(@suffix) <);
    return $sys.spawn_sender(suffix, sender());
};

public method .trusted(): nooverride  {
    arg @literal;
    
    if (literal)
        return trusted || [];
    return (trusted || []) + (.writers());
};

public method .trusted_by(): nooverride  {
    return trusted_by || [];
};

public method .trusts(): nooverride  {
    arg obj;
    var o;
    
    for o in (.trusted()) {
        if (o.has_ancestor($group)) {
            if (o.includes(obj))
                return 1;
        } else if (o == obj) {
            return 1;
        }
    }
    return ($systrust_group.includes(obj)) || ($sys.is_system(obj));
};

public method .undefine_setting(): nooverride  {
    arg name;
    var d;
    
    (> .perms(sender()) <);
    if (!((.defined_settings()).contains(name)))
        throw(~setnf, (("Setting \"" + name) + "\" is not defined by ") + this());
    
    // clear it on all descendants, then us
    for d in ((.descendants()) + [this()]) {
        d._clear_setting(name);
        pause();
    }
    
    // bye bye
    defined_settings = dict_del(defined_settings, name);
    if (!defined_settings)
        clear_var('defined_settings);
};

root method .uninit_root(): nooverride  {
    var obj;
    
    (| manager.del_managed_obj() |);
    catch any {
        for obj in (.managed())
            obj.change_manager($reaper);
    }
    catch any {
        for obj in (.writers('literal))
            .del_writer(obj);
    }
    catch any {
        for obj in (.writes())
            obj.del_writer(this());
    }
    catch any {
        for obj in (.trusted('literal))
            .del_trusted(obj);
    }
    catch any {
        for obj in (.trusted_by())
            obj.del_trusted(this());
    }
    
    // tell $sys we are going away
    $sys.sender_going_away();
};

public method .uninitialize(): nooverride  {
    arg @destroyed;
    var a, v, d, definer, method, descendants, str, tb;
    
    (> .perms(caller(), $root, $sys) <);
    destroyed = destroyed ? 1 : 0;
    descendants = (.descendants()) + [this()];
    
    // call [ancestors...].uninit_ancestor() on the object being destroyed
    for a in (ancestors()) {
        method = tosym("uninit_" + (a.objname()));
        if ((definer = (| find_method(method) |))) {
            if (definer != a) {
                // scream madly and run around--everybody should know this
                str = "UNINIT ERROR: uninit method for " + a;
                str += (" in wrong place (" + definer) + ")";
                (| (definer.manager()).tell(str) |);
                (| (a.manager()).tell(str) |);
                (| sender().tell(str) |);
                continue;
            }
            catch any {
                .(method)();
            } with {
                // try and let somebody know they made a boo-boo somewhere
                tb = traceback().fmt_tb();
                str = ((("UNINIT ERROR " + this()) + "<") + definer) + ">:";
                if (definer) {
                    (| (definer.manager()).tell(str) |);
                    (| (definer.manager()).tell(tb) |);
                }
                (| sender().tell(str) |);
                (| sender().tell(tb) |);
                (| sender().tell("Continuing uninit..") |);
                (| $sys.log_traceback(tb, "** " + str) |);
            }
        }
        refresh();
    }
    
    // if we have descendants, clean anything from the object being dested
    method = tosym("uninit_" + (.objname()));
    if ((definer = (| find_method(method) |))) {
        if (definer != this()) {
            // scream madly and run around--everybody should know this
            str = "UNINIT ERROR: uninit method for " + a;
            str += (" in wrong place (" + definer) + ")";
            (| (definer.manager()).tell(str) |);
            (| (.manager()).tell(str) |);
            (| sender().tell(str) |);
        } else {
            for d in (descendants) {
                catch any {
                    d.(method)();
                } with {
                    // try and let somebody know they made a boo-boo somewhere
                    str = ((("UNINIT ERROR " + d) + "<") + this()) + ">:";
                    tb = traceback().fmt_tb();
                    (| (.manager()).tell(str) |);
                    (| (.manager()).tell(tb) |);
                    if ((.manager()) != sender()) {
                        (| sender().tell(str) |);
                        (| sender().tell(tb) |);
                    }
                    (| sender().tell("Continuing uninit..") |);
                    (| $sys.log_traceback(tb, "** " + str) |);
                }
                refresh();
            }
        }
    }
    
    // clear vars
    if ((!destroyed) && descendants)
        $sys.clear_definer_vars(this(), descendants);
};

public method .valid_setting_attr(): nooverride  {
    arg name, value;
    
    if (!(name in ['get, 'set, 'parse, 'clear, 'format, 'access]))
        throw(~setattr, "Invalid setting attribute '" + name);
    if (!value)
        return 0;
    else if (type(value) != 'list)
        throw(~setattr, "Setting attribute must be a list");
    else if (type(value[1]) != 'symbol)
        throw(~setattr, "Setting attribute[1] is not a symbol");
    return 1;
};

public method .variable_info() {
    arg gen, def, fltr, match;
    var ancs, data, pp, p;
    
    if (!(.has_flag('variables, sender())))
        throw(~perm, (sender() + " cannot read variables on ") + this());
    if (def)
        ancs = [def];
    else
        ancs = .(gen[1])(gen[2]) || [this()];
    data = [];
    for pp in ((data().to_list()).reverse()) {
        if (valid(pp[1]) && ((pp[1]) in ancs)) {
            for p in (pp[2]) {
                if (tostr(p[1]).(match)(fltr) != 0)
                    data += [[pp[1], @p]];
            }
        }
    }
    return data;
};

public method .variables(): nooverride  {
    if (!(.has_flag('variables, sender())))
        throw(~perm, (sender() + " doesn't have permission to find methods on ") + this());
    return variables();
};

public method .will_inherit() {
    arg who;
    
    if (this() in (sender().parents()))
        throw(~perm, ((sender() + " already has ") + this()) + " as a parent.");
    if ((!(.has_flag('fertile, sender()))) && (!(.trusts(who))))
        throw(~perm, ((this() + " refuses to be parent of ") + sender()) + ".");
};

public method .will_spawn(): nooverride  {
    arg who;
    
    if (!(.has_flag('fertile, who)))
        throw(~perm, (.namef('ref)) + " is not fertile or readable.");
    
    // available quota?
    if (!(who.quota_valid()))
        throw(~quota, "Sender does not have the available quota");
};

public method .write_check(): nooverride  {
    arg obj;
    
    if (.is($group))
        return .includes(obj);
    else
        return this() == obj;
};

public method .writers(): nooverride  {
    arg @literal;
    
    if (literal)
        return writers || [];
    if (manager != this())
        return [manager, this()] + (writers || []);
    return [manager] + (writers || []);
};

public method .writes(): nooverride  {
    return writes || [];
};


