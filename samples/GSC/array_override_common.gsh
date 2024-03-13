
#define NUM_TYPES 2
#define ARRAY_RANDOM 0
#define ARRAY_RANDOMIZE 1

#define REGISTER_OVERRIDE(str_name,n_type,func) array_override::register(str_name,n_type,func)
#define UNREGISTER_OVERRIDE(str_name,n_type) array_override::unregister(str_name,n_type)
#define NOTIF_EXLUDE(str_name) array_override::increment_exclude_from_notif(str_name)
#define REMOVE_NOTIF_EXCLUDE(str_name) array_override::decrement_exclude_from_notif(str_name)
#define REGISTER_OVERRIDE_EX_NOTIF(str_name,n_type,func) NOTIF_EXLUDE(str_name);REGISTER_OVERRIDE(str_name,n_type,func)
#define UNREGISTER_OVERRIDE_EX_NOTIF(str_name,n_type) UNREGISTER_OVERRIDE(str_name,n_type);REMOVE_NOTIF_EXCLUDE(str_name)

#define REGISTER_RECURSIVE(str_name,n_type,func) array_override::register_recursive(str_name,n_type,func)
#define REGISTER_RECURSIVE_EX_NOTIF(str_name,n_type,func) array_override::register_recursive(str_name,n_type,func,true)
#define INCREMENT_REGISTER(str_name,n_type) array_override::increment_register(str_name,n_type)
#define DECREMENT_REGISTER(str_name,n_type) array_override::decrement_register(str_name,n_type)
#define RESET_RECURSIVE(str_name,n_type) array_override::decrement_register(str_name,n_type,true)
#define RUN_RECURSIVE(str_name,n_type,func) array_override::run_recursive_override_instance(str_name,n_type,func)
#define ADD_RECURSIVE_FLAG(str_name,n_type,a_flag) array_override::add_recursive_override_flag(str_name,n_type,a_flag)
#define LINK_TO_RECURSIVE_FLAG(str_name,n_type,a_flag) thread array_override::link_to_recursive_flag(str_name,n_type,a_flag)
#define RECURSIVE_ENDON self endon("disconnect");

#define CALL_ONCE_FLAG(__name) if (isdefined(level.__name)) return; level.__name = 1;

#define IS_FIELD_OBJECT(_val) (isdefined(_val) && (IsEntity(_val) || (!IsInt(_val) && !IsFloat(_val) && !IsString(_val) && !IsArray(_val) && !IsFunctionPtr(_val) && !IsVec(_val))))
#define IF_KVP_MATCH(__key,__val,__ent) struct_or_ent = __ent; if (!IS_FIELD_OBJECT(struct_or_ent)) return; if (struct_or_ent.__key === __val)
#define IF_TARGETNAME_MATCH(__targetname,__ent) IF_KVP_MATCH(targetname,__targetname,__ent)
#define IF_NOTEWORTHY_MATCH(__noteworthy,__ent) IF_KVP_MATCH(script_noteworthy,__noteworthy,__ent)

#define RETURN_IF_ARRAY(__ret) if (IsArray(__ret)) return __ret;
#define ARRAY_SAFE_RETURN(__listName) i = self GetEntityNumber();\
		RETURN_IF_ARRAY(__listName[i])\
		RETURN_IF_ARRAY(__listName[0])\
		RETURN_IF_ARRAY(__listName)\
		return array();

// for entries which specify which category it should be valid for (spawn requests, forced drops, dig rewards)
#define BGB_CLASSIC 0
#define BGB_MEGA 1
#define BGB_ALL 2

#define MAP_ASSIGN_LIST(varname) switch(GetDvarString("mapname")){\
	case "zm_zod":varname = ZOD;break;\
	case "zm_factory":varname = FACTORY;break;\
	case "zm_castle":varname = CASTLE;break;\
	case "zm_island":varname = ISLAND;break;\
	case "zm_stalingrad":varname = STALINGRAD;break;\
	case "zm_genesis":varname = GENESIS;break;\
	case "zm_prototype":varname = PROTOTYPE;break;\
	case "zm_asylum":varname = ASYLUM;break;\
	case "zm_sumpf":varname = SUMPF;break;\
	case "zm_theater":varname = THEATER;break;\
	case "zm_cosmodrome":varname = COSMODROME;break;\
	case "zm_temple":varname = TEMPLE;break;\
	case "zm_moon":varname = MOON;break;\
	case "zm_tomb":varname = TOMB;break;\
	default:return;}

#define MAP_ASSIGN_LIST_RETURN(varname) MAP_ASSIGN_LIST(varname) if(!isdefined(varname)) return;

#define MAP_ASSIGN_LIST_2(varname1,varname2) switch(GetDvarString("mapname")){\
	case "zm_zod":varname1 = ZOD;varname2 = ZOD_2;break;\
	case "zm_factory":varname1 = FACTORY;varname2 = FACTORY_2;break;\
	case "zm_castle":varname1 = CASTLE;varname2 = CASTLE_2;break;\
	case "zm_island":varname1 = ISLAND;varname2 = ISLAND_2;break;\
	case "zm_stalingrad":varname1 = STALINGRAD;varname2 = STALINGRAD_2;break;\
	case "zm_genesis":varname1 = GENESIS;varname2 = GENESIS_2;break;\
	case "zm_prototype":varname1 = PROTOTYPE;varname2 = PROTOTYPE_2;break;\
	case "zm_asylum":varname1 = ASYLUM;varname2 = ASYLUM_2;break;\
	case "zm_sumpf":varname1 = SUMPF;varname2 = SUMPF_2;break;\
	case "zm_theater":varname1 = THEATER;varname2 = THEATER_2;break;\
	case "zm_cosmodrome":varname1 = COSMODROME;varname2 = COSMODROME_2;break;\
	case "zm_temple":varname1 = TEMPLE;varname2 = TEMPLE_2;break;\
	case "zm_moon":varname1 = MOON;varname2 = MOON_2;break;\
	case "zm_tomb":varname1 = TOMB;varname2 = TOMB_2;break;\
	default:return;}

#define MAP_ASSIGN_LIST_2_RETURN(varname1,varname2) MAP_ASSIGN_LIST_2(varname1,varname2) if(!isdefined(varname1)) return;

#define MAP_ASSIGN_FUNC(varname) switch(GetDvarString("mapname")){\
	case "zm_zod":varname = ZOD_FUNC;break;\
	case "zm_factory":varname = FACTORY_FUNC;break;\
	case "zm_castle":varname = CASTLE_FUNC;break;\
	case "zm_island":varname = ISLAND_FUNC;break;\
	case "zm_stalingrad":varname = STALINGRAD_FUNC;break;\
	case "zm_genesis":varname = GENESIS_FUNC;break;\
	case "zm_prototype":varname = PROTOTYPE_FUNC;break;\
	case "zm_asylum":varname = ASYLUM_FUNC;break;\
	case "zm_sumpf":varname = SUMPF_FUNC;break;\
	case "zm_theater":varname = THEATER_FUNC;break;\
	case "zm_cosmodrome":varname = COSMODROME_FUNC;break;\
	case "zm_temple":varname = TEMPLE_FUNC;break;\
	case "zm_moon":varname = MOON_FUNC;break;\
	case "zm_tomb":varname = TOMB_FUNC;break;\
	default:return;}

#define MAP_ASSIGN_FUNC_RETURN(varname) MAP_ASSIGN_FUNC(varname) if (!isdefined(varname)) return;

#define MAP_ASSIGN_FUNC_2(varname1,varname2) switch(GetDvarString("mapname")){\
	case "zm_zod":varname1 = ZOD_FUNC;varname2 = ZOD_FUNC_2;break;\
	case "zm_factory":varname1 = FACTORY_FUNC;varname2 = FACTORY_FUNC_2;break;\
	case "zm_castle":varname1 = CASTLE_FUNC;varname2 = CASTLE_FUNC_2;break;\
	case "zm_island":varname1 = ISLAND_FUNC;varname2 = ISLAND_FUNC_2;break;\
	case "zm_stalingrad":varname1 = STALINGRAD_FUNC;varname2 = STALINGRAD_FUNC_2;break;\
	case "zm_genesis":varname1 = GENESIS_FUNC;varname2 = GENESIS_FUNC_2;break;\
	case "zm_prototype":varname1 = PROTOTYPE_FUNC;varname2 = PROTOTYPE_FUNC_2;break;\
	case "zm_asylum":varname1 = ASYLUM_FUNC;varname2 = ASYLUM_FUNC_2;break;\
	case "zm_sumpf":varname1 = SUMPF_FUNC;varname2 = SUMPF_FUNC_2;break;\
	case "zm_theater":varname1 = THEATER_FUNC;varname2 = THEATER_FUNC_2;break;\
	case "zm_cosmodrome":varname1 = COSMODROME_FUNC;varname2 = COSMODROME_FUNC_2;break;\
	case "zm_temple":varname1 = TEMPLE_FUNC;varname2 = TEMPLE_FUNC_2;break;\
	case "zm_moon":varname1 = MOON_FUNC;varname2 = MOON_FUNC_2;break;\
	case "zm_tomb":varname1 = TOMB_FUNC;varname2 = TOMB_FUNC_2;break;\
	default:return;}

#define MAP_ASSIGN_FUNC_2_RETURN(varname1,varname2) MAP_ASSIGN_FUNC_2(varname1,varname2) if (!isdefined(varname1)) return;