#using scripts\shared\array_shared;
#using scripts\shared\callbacks_shared;
#using scripts\shared\flag_shared;
#using scripts\shared\util_shared;
#insert scripts\shared\shared.gsh;
#insert scripts\zm\array_override\array_override_common.gsh;

#namespace array_override;

#define NOTIFY_SELF(n_add) self notify("override_state_change",n_type,str_name,n_add,is_override_exluded_from_notif(str_name))

function register(str_name,n_type,func)
{
	switch(n_type)
	{
		case ARRAY_RANDOM:
			DEFAULT(self.array_random_override,array());
			self.array_random_override[str_name] = func;
			break;
		case ARRAY_RANDOMIZE:
			DEFAULT(self.array_randomize_override,array());
			self.array_randomize_override[str_name] = func;
			break;
	}
	NOTIFY_SELF(1);
}

function unregister(str_name,n_type)
{
	switch(n_type)
	{
		case ARRAY_RANDOM:
			ArrayRemoveIndex(self.array_random_override,str_name,1);
			break;
		case ARRAY_RANDOMIZE:
			ArrayRemoveIndex(self.array_randomize_override,str_name,1);
			break;
	}
	NOTIFY_SELF(-1);
}

function private reregister(str_name,n_type)
{
	switch(n_type)
	{
		case ARRAY_RANDOM:
			func = self.array_random_override[str_name];
			break;
		case ARRAY_RANDOMIZE:
			func = self.array_randomize_override[str_name];
			break;
	}
	if (!isdefined(func)) return;
	increment_exclude_from_notif(str_name);
	unregister(str_name,n_type);
	register(str_name,n_type,func);
	decrement_exclude_from_notif(str_name);
}

#define EXCLUDE_FROM_NOTIF_DEFAULTS MAKE_ARRAY(self.override_notif_exclude) DEFAULT(self.override_notif_exclude[str_name],0);
function increment_exclude_from_notif(str_name)
{
	EXCLUDE_FROM_NOTIF_DEFAULTS
	self.override_notif_exclude[str_name]++;
}

function decrement_exclude_from_notif(str_name)
{
	EXCLUDE_FROM_NOTIF_DEFAULTS
	self.override_notif_exclude[str_name]--;
	if (self.override_notif_exclude[str_name] <= 0) ArrayRemoveIndex(self.override_notif_exclude,str_name,1);
}

function private is_override_exluded_from_notif(str_name)
{
	MAKE_ARRAY(self.override_notif_exclude)
	return IS_TRUE(self.override_notif_exclude[str_name]);
}

function autoexec init_player_overrides()
{
	callback::on_connect(&on_player_connect);
}

function private on_player_connect()
{
	self.array_random_override = array();
	self.array_randomize_override = array();
}

function register_recursive(str_name,n_type,func,b_ex_notif = false)
{
	switch(n_type)
	{
		case ARRAY_RANDOM:
			DEFAULT(level.recursive_random_override,array());
			level.recursive_random_override[str_name] = SpawnStruct();
			struct = level.recursive_random_override[str_name];
			break;
		case ARRAY_RANDOMIZE:
			DEFAULT(level.recursive_randomize_override,array());
			level.recursive_randomize_override[str_name] = SpawnStruct();
			struct = level.recursive_randomize_override[str_name];
			break;
	}
	struct.func = func;
	struct.b_ex_notif = b_ex_notif;
	struct.count = 0;
	struct.a_flag = array();
}

#define RECURSIVE_SWITCHBLOCK switch(n_type){\
	case ARRAY_RANDOM: struct = level.recursive_random_override[str_name];break;\
	case ARRAY_RANDOMIZE: struct = level.recursive_randomize_override[str_name];break;}\
	if (!isdefined(struct)) return;

function increment_register(str_name,n_type)
{
	RECURSIVE_SWITCHBLOCK
	if (struct.count <= 0)
	{
		struct.count = 0;
		if (struct.b_ex_notif) level increment_exclude_from_notif(str_name);
		level register(str_name,n_type,struct.func);
	}
	struct.count++;
	thread update_recursive_flags(struct);
}

function decrement_register(str_name,n_type,b_unregister = false)
{
	RECURSIVE_SWITCHBLOCK
	struct.count--;
	if (b_unregister || struct.count <= 0)
	{
		level unregister(str_name,n_type);
		if (struct.b_ex_notif) level decrement_exclude_from_notif(str_name);
		struct.count = 0;
	}
	thread update_recursive_flags(struct);
}

function run_recursive_override_instance(str_name,n_type,func)
{
	//fix this
	id = self GetEntityNumber() + 1;
	wait(.05 * id);
	//
	increment_register(str_name,n_type);
	[[func]]();
	decrement_register(str_name,n_type);
}

function add_recursive_override_flag(str_name,n_type,a_flags)
{
	RECURSIVE_SWITCHBLOCK
	DEFAULT(struct.a_flag,array());
	foreach (flag in a_flags)
	{
		if (!IsArray(flag)) flag_array = array(flag);
		else flag_array = flag;
		str_flag = flag_array[0];
		if (!IsString(str_flag)) continue;
		n_count = VAL(flag_array[1],0);
		flag_struct = SpawnStruct();
		flag_struct.flag = str_flag;
		flag_struct.required_count = n_count;
		flag_struct.comparison = flag_array[2];
		struct.a_flag[struct.a_flag.size] = flag_struct;
		level flag::init(str_flag);
		update_specific_flag(flag_struct,struct.count);
	}
}

function update_recursive_flags(struct)
{
	foreach (flag_struct in struct.a_flag)
	{
		update_specific_flag(flag_struct,struct.count);
	}
}

function private update_specific_flag(flag_struct,n_count)
{
	if (IsFunctionPtr(flag_struct.required_count)) required_count = [[flag_struct.required_count]]();
	else required_count = flag_struct.required_count;

	if (IsFunctionPtr(flag_struct.comparison)) b_val = [[flag_struct.comparison]](n_count,required_count);
	else b_val = n_count === required_count;

	if (isdefined(b_val)) level flag::set_val(flag_struct.flag,b_val);
}

function link_to_recursive_flag(str_name,n_type,a_flag,b_waitForAll = false)
{
	level endon("end_game");
	if (!isdefined(a_flag)) return;
	if (!IsArray(a_flag)) a_flag = array(a_flag);
	if (b_waitForAll) {waitFunc = &flag::wait_till_all; waitClearFunc = &flag::wait_till_clear_all;}
	else {waitFunc = &flag::wait_till_any; waitClearFunc = &flag::wait_till_clear_all;}

	while(1)
	{
		level [[waitFunc]](a_flag);
		level [[waitClearFunc]](a_flag);
		wait .05;
		level reregister(str_name,n_type);
	}
}