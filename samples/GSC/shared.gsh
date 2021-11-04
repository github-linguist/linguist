#define PLAYER_1 0
#define PLAYER_2 1
#define PLAYER_3 2
#define PLAYER_4 3
	
#define ART_REVIEW GetDvarInt( "art_review", 0 )
	
//SESSION MODE FLAGS
// should stay the same as eGameModes in com_gamemodes.h
#define GAMEMODE_PUBLIC_MATCH				0
#define GAMEMODE_PRIVATE_MATCH				1
#define GAMEMODE_LOCAL_SPLITSCREEN			2
#define GAMEMODE_WAGER_MATCH				3
#define GAMEMODE_THEATER					5
#define GAMEMODE_LEAGUE_MATCH				6
#define GAMEMODE_RTS						7

//SESSION MODE ENUM
// sync with eModes
#define SESSIONMODE_ZOMBIES					0
#define SESSIONMODE_MULTIPLAYER				1
#define SESSIONMODE_CAMPAIGN				2
#define SESSIONMODE_INVALID					3	
	
// SPAWNFLAGS

#define SPAWNFLAG_MODEL_DYNAMIC_PATH		1

#define SPAWNFLAG_TRIGGER_AI_AXIS			1
#define SPAWNFLAG_TRIGGER_AI_ALLIES			2
#define SPAWNFLAG_TRIGGER_AI_NEUTRAL		4
#define SPAWNFLAG_TRIGGER_NOT_PLAYER		8
#define SPAWNFLAG_TRIGGER_VEHICLE			16
#define SPAWNFLAG_TRIGGER_SPAWN				32
#define SPAWNFLAG_TRIGGER_TOUCH_ONCE		64
#define SPAWNFLAG_TRIGGER_LOOK				256
#define SPAWNFLAG_TRIGGER_SPAWN_MANAGER		512
#define SPAWNFLAG_TRIGGER_TRIGGER_ONCE		1024
	
#define SPAWNFLAG_TRIGGER_HURT_START_OFF			1
#define SPAWNFLAG_TRIGGER_HURT_PLAYER_ONLY			2
#define SPAWNFLAG_TRIGGER_HURT_SILENT				4
#define SPAWNFLAG_TRIGGER_HURT_NO_PROTECTION		8
#define SPAWNFLAG_TRIGGER_HURT_SLOW					16
#define SPAWNFLAG_TRIGGER_HURT_ONCE					32
#define SPAWNFLAG_TRIGGER_HURT_IGNORE_LAST_STAND	64

#define SPAWNFLAG_ACTOR_SPAWNER					1
#define SPAWNFLAG_ACTOR_SCRIPTMAKEROOM			2
// UNDELETABLE									4
// ENEMYINFO									8
#define SPAWNFLAG_ACTOR_SCRIPTFORCESPAWN		16
#define SPAWNFLAG_ACTOR_SM_PRIORITY				32
#define SPAWNFLAG_ACTOR_SCRIPTINFINITESPAWN		64
#define SPAWNFLAG_ACTOR_SCRIPTDELETEONZEROCOUNT	128

#define SPAWNFLAG_VEHICLE_NODE_START_NODE	1
#define SPAWNFLAG_VEHICLE_NODE_ROTATE		65536

#define SPAWNFLAG_VEHICLE_USEABLE					1
#define SPAWNFLAG_VEHICLE_SPAWNER					2
// UNDELETABLE										4
#define SPAWNFLAG_VEHICLE_SCRIPTMAKEROOM			8
// These match the SPAWNFLAG_ACTOR flags
//#define SPAWNFLAG_VEHICLE_SCRIPTFORCESPAWN			16
//#define SPAWNFLAG_VEHICLE_SM_PRIORITY					32
//#define SPAWNFLAG_VEHICLE_SCRIPTINFINITESPAWN			64
//#define SPAWNFLAG_VEHICLE_SCRIPTDELETEONZEROCOUNT		128
	
#define SPAWNFLAG_TURRET_ENABLED			1
#define SPAWNFLAG_TURRET_GET_USERS			2

#define SPAWNFLAG_PATH_DONT_LINK			1
#define SPAWNFLAG_PATH_NOT_CHAIN			2
	
#define SPAWNFLAG_PATH_STANCE_STAND			4
#define SPAWNFLAG_PATH_STANCE_CROUCH		8
#define SPAWNFLAG_PATH_STANCE_PRONE			16
	
#define SPAWNFLAG_PATH_CAN_PARENT			256
#define SPAWNFLAG_PATH_DISABLED				512
#define SPAWNFLAG_PATH_DONT_LEFT			1024
#define SPAWNFLAG_PATH_BALCONY				1024
#define SPAWNFLAG_PATH_PROCEDURAL			1024
#define SPAWNFLAG_PATH_DONT_RIGHT			2048
#define SPAWNFLAG_PATH_BALCONY_NORAILING	2048
#define SPAWNFLAG_PATH_WALLRUN				2048

#define SPAWNFLAG_PATH_DOUBLE_WIDE			1048576
#define SPAWNFLAG_PATH_QUADROTOR			2097152
	
#define SPAWNFLAG_SCRIPTBUNDLE_INIT			1
#define SPAWNFLAG_SCRIPTBUNDLE_PLAY			2
	
#define SPAWNFLAG(__e,__f) (isdefined(__e.spawnflags)&&((__e.spawnflags & __f) == __f))
	
// CONTENTS
#define	CONTENTS_SOLID				(1 << 0)	// an eye is never valid in a solid
#define CONTENTS_FOLIAGE			(1 << 1)
#define CONTENTS_NONCOLLIDING		(1 << 2)
#define	CONTENTS_GLASS				(1 << 4)
#define	CONTENTS_WATER				(1 << 5)
#define	CONTENTS_CANSHOOTCLIP		(1 << 6)
#define	CONTENTS_MISSILECLIP		(1 << 7)
#define CONTENTS_ITEM				(1 << 8)
#define CONTENTS_VEHICLECLIP		(1 << 9)
#define CONTENTS_ITEMCLIP			(1 << 10)
#define	CONTENTS_SKY				(1 << 11)	// fixes diffuse sunlight checks right next to a skybrush
#define	CONTENTS_AI_NOSIGHT			(1 << 12)	// AI cannot see through this
#define	CONTENTS_CLIPSHOT			(1 << 13)	// bullets hit this
#define	CONTENTS_CORPSE_CLIPSHOT	(1 << 14)	// to get bullets to hit corpses, but not grenade radius damage; should never be on a brush, only in game
#define	CONTENTS_ACTOR				(1 << 15)	// using this for dogs
#define	CONTENTS_FAKE_ACTOR			(1 << 15)
#define CONTENTS_PLAYERCLIP			(1 << 16)
#define CONTENTS_MONSTERCLIP		(1 << 17)
#define	CONTENTS_PLAYERVEHICLECLIP	(1 << 18)
#define CONTENTS_USE				(1 << 21)
#define CONTENTS_UTILITYCLIP		(1 << 22)   // level specific collision type
#define CONTENTS_VEHICLE			(1 << 23)
#define CONTENTS_MANTLE				(1 << 24)	// used for mantle / ladder / ledge / pipe traversals
#define CONTENTS_PLAYER				(1 << 25)
#define CONTENTS_CORPSE				(1 << 26)
#define CONTENTS_DETAIL				(1 << 27)
#define CONTENTS_STRUCTURAL			(1 << 28)
#define CONTENTS_LOOKAT				(1 << 28)
#define CONTENTS_TRIGGER			(1 << 30)
#define CONTENTS_NODROP				(1 << 31)
	
// Physics Trace Masks
#define PHYSICS_TRACE_MASK_PHYSICS		(1 << 0)
#define PHYSICS_TRACE_MASK_VEHICLE		(1 << 1)
#define PHYSICS_TRACE_MASK_WATER		(1 << 2)
#define PHYSICS_TRACE_MASK_CLIP			(1 << 3)
#define PHYSICS_TRACE_MASK_VEHICLE_CLIP	(1 << 4)
#define PHYSICS_TRACE_MASK_PLAYER	(1 << 5)

// Navmesh materials
#define NAVMESH_MATERIAL_NONE			0
#define NAVMESH_MATERIAL_DEFAULT		(1 << 0)
#define NAVMESH_MATERIAL_WATER			(1 << 1)
#define NAVMESH_MATERIAL_WALLRUN		(1 << 3)
#define NAVMESH_MATERIAL_STAIRCASE		(1 << 4)
#define NAVMESH_MATERIAL_NOVEHICLE		(1 << 5)

// LUI defines
#define LUI_HUDELEM_TEXTSIZE_DEFAULT	25
#define LUI_HUDELEM_TEXTSIZE_SMALL		20
#define LUI_HUDELEM_TEXTSIZE_MEDIUM		36
#define LUI_HUDELEM_TEXTSIZE_LARGE		48

#define LUI_HUDELEM_ALIGNMENT_LEFT		1
#define LUI_HUDELEM_ALIGNMENT_CENTER	2
#define LUI_HUDELEM_ALIGNMENT_RIGHT		3

// Team enums
#define TEAM_FREE		0
#define TEAM_ALLIES		1
#define TEAM_AXIS		2

// IDENTIFIERS
#define INT_MAX		0x7FFFFFFF

#define X			0
#define Y			1
#define Z			2

#define PITCH		0
#define YAW			1
#define ROLL		2

#define PI 			3.14159
	
#define RGB(__r,__g,__b)(__r/255,__g/255,__b/255)

#define RED		( 1, 0, 0 )
#define GREEN	( 0, 1, 0 )
#define BLUE	( 0, 0, 1 )
#define YELLOW	( 1, 1, 0 )
#define ORANGE	( 1, .5, 0 )
#define CYAN	( 0, 1, 1 )
#define PURPLE	( 1, 0, 1 )
#define BLACK	( 0, 0, 0 )
#define WHITE	( 1, 1, 1 )
#define GREY   	( 0.75, 0.75, 0.75 )
#define GRAY1	( .1, .1, .1 )
#define GRAY2	( .2, .2, .2 )
#define GRAY3	( .3, .3, .3 )
#define GRAY4	( .4, .4, .4 )
#define GRAY5	( .5, .5, .5 )
#define GRAY6	( .6, .6, .6 )
#define GRAY7	( .7, .7, .7 )
#define GRAY8	( .8, .8, .8 )
#define GRAY9	( .9, .9, .9 )
#define SLATE	RGB( 112, 128, 144 )
#define PINK	RGB( 255, 192, 203 )
#define OLIVE	RGB( 128, 128, 0 )
#define BROWN	RGB( 139, 69, 19 )
	
// Native UI resolution is 720p
#define SCREEN_WIDTH 1280
#define SCREEN_HEIGHT 720
	
// Register Systems
#define REGISTER_SYSTEM(__sys,__func_init_preload,__reqs) \
	function autoexec __init__sytem__() { \
		system::register(__sys,__func_init_preload,undefined,__reqs); \
	}

#define REGISTER_SYSTEM_EX(__sys,__func_init_preload,__func_init_postload,__reqs) \
	function autoexec __init__sytem__() { \
		system::register(__sys,__func_init_preload,__func_init_postload,__reqs); \
	}
	
#define WAITTILL_LOAD if(!isdefined(level.flag) || !IS_TRUE(level.flag["load_main_complete"]))level waittill( "load_main_complete" )

// UTILITIES

#define TIMEOUT(__t) \
	if ( isdefined( __t ) ) { \
		__s = SpawnStruct(); \
		__s endon( "timeout" ); \
		__s util::delay_notify( __t, "timeout" ); \
	}

#define __TIME_UPDATE__ \
	__time_stamp__ = GetTime(); \
	if ( __time_stamp__ > __time_stamp_prev__ ) { \
	__time__ = ( __time_stamp__ - __time_stamp_start__ ) / 1000; \
	__time_frac__ = LerpFloat( 0, 1, Float( __time__ ) / Float( __time_length__ ) ); \
	__time_left__ = Max( __time_length__ - __time__, 0 ); \
	__time_stamp_prev__ = __time_stamp__; }

#define FOR_TIME(___time_length___,___do_last_step___) \
	__time_stamp__ = GetTime(); \
	__time_stamp_prev__ = 0; \
	__time_stamp_start__ = __time_stamp__; \
	__time_length__ = ___time_length___; \
	__do_last_step__ = ___do_last_step___; \
	while ( true ) { __TIME_UPDATE__

#define FOR_TIME_LOOP(__time_step__) \
	__TIME_UPDATE__ \
	__time_this_step__ = Min( __time_step__, __time_left__ ); \
	if ( __time_this_step__ > 0 ) { wait __time_this_step__; __TIME_UPDATE__ }\
	if ( __time_left__ > 0 ) { continue; } \
	if ( __do_last_step__ ) { __do_last_step__ = false; continue; } \
	break; \
}

#define IS_EQUAL(__a,__b) ( __a === __b )
#define IS_TRUE(__a) ( isdefined( __a ) && __a )

#define IS_EVEN(_number) ( ( _number % 2 ) == 0 )
#define IS_ODD(_number) ( ( _number % 2 ) == 1 )

#define FLAT_ORIGIN(__origin) ( __origin[0], __origin[1], 0 )
#define FLAT_ANGLES(__angles) ( 0, __angles[1], 0 )

#define VEC_SET_X(_v,_x) _v = (_x, _v[1], _v[2])
#define VEC_SET_Y(_v,_y) _v = (_v[0], _y, _v[2])
#define VEC_SET_Z(_v,_z) _v = (_v[0], _v[1], _z)

#define CLIENT_FRAME .016
#define SERVER_FRAME .05
#define WAIT_SERVER_FRAME {wait(SERVER_FRAME);}
#define WAIT_CLIENT_FRAME {wait(CLIENT_FRAME);}

#define MPH_TO_INCHES_PER_SEC	17.6

#define DEFAULT(__var,__default) if(!isdefined(__var))__var=__default
#define DEFAULT2(__var,__default1,__default2) if(!isdefined(__var))__var=(isdefined(__default1)?__default1:__default2)
#define VAL(__var,__default) (isdefined(__var)?__var:__default)
	
#define T7_CLEANUP(__msg)load::add_cleanup_msg(__msg)
	
// MATH

#define CLAMP_MIN(__var,__min) if (__var < __min) { \
		__var = __min; \
	}
	
#define CLAMP_MAX(__var,__max) if (__var > __max) { \
		__var = __max; \
	}
	
#define CLAMP(__var,__min,__max) if (__var < __min) { \
		__var = __min; \
	} \
	else if (__var > __max) { \
		__var = __max; \
	}

#define SQR(__var) ( (__var) * (__var) )

// STRINGS

#define STR(__var)(isdefined(__var)?""+__var:"")
#define STR_DEFAULT(__var,__def)(isdefined(__var)?""+__var:STR(__def))

// ARRAYS

#define RANDOM(__array) __array[GetArrayKeys(__array)[RandomInt(GetArrayKeys(__array).size)]]
#define MAKE_ARRAY(__array) if ( !isdefined( __array ) ) __array = []; else if ( !IsArray( __array ) ) __array = array( __array );
#define ARRAY_ADD(__array,__item) MAKE_ARRAY(__array) __array[__array.size]=__item;
	
#define MAX_AI 64 // match code
#define MAX_SPAWNED_PER_FRAME 1
	
//********************************************************************************	
//MOVED FROM CLIENTFLAGS.GSH
//  These are not client flags they're just constants
//  TODO - these should be moved into their respective scripts
//********************************************************************************	

// Riotshield
#define RIOTSHIELD_STATE_DEPLOYED 1
#define RIOTSHIELD_STATE_DESTROYED 2

// Trophy System
#define TROPHY_SYSTEM_INIT 0
#define TROPHY_SYSTEM_ROLLING 1
#define TROPHY_SYSTEM_STATIONARY 2
#define TROPHY_SYSTEM_STUNNED 3

// Bouncing Betty ( Trip mine, Spider mine )
#define BOUNCINGBETTY_INIT 0
#define BOUNCINGBETTY_DETONATING 1
#define BOUNCINGBETTY_DEPLOYING 2

// Proxy (Tazer Spike)
#define PROXY_PLAYER_NOT_TAZERED 0
#define PROXY_PLAYER_TAZERED 1

// QR Drone
#define QRDRONE_FX_DEFAULT		0
#define QRDRONE_FX_BLINK		1
#define QRDRONE_FX_FINAL_BLINK	2
#define QRDRONE_FX_DEATH		3


//********************************************************************************	
//MOVED FROM SP.GSH
//********************************************************************************	
#define TURRET_TARGET_AI		1
#define TURRET_TARGET_PLAYERS	2
#define TURRET_TARGET_DRONES	4
#define TURRET_TARGET_VEHICLES	8

//VEHICLE FUNCTIONS
#define IS_PLANE(__e) ( isdefined(__e.vehicleclass) && (__e.vehicleclass == "plane") )
#define IS_HELICOPTER(__e) ( isdefined(__e.vehicleclass) && (__e.vehicleclass == "helicopter" ) )
#define IS_BOAT(__e) ( isdefined(__e.vehicleclass) && (__e.vehicleclass == "boat" ) )
#define IS_TANK(__e) ( isdefined(__e.vehicleclass) && (__e.vehicleclass == "tank" ) )
#define IS_ARTILLERY(__e) ( isdefined(__e.vehicleclass) && ( __e.vehicleclass == "artillery" ) )
#define IS_MOTORCYCLE(__e) ( isdefined(__e.vehicleclass) && (__e.vehicleclass == "motorcycle" ) )
#define IS_4WHEEL(__e) ( isdefined(__e.vehicleclass) && (__e.vehicleclass == "4 wheel") )
#define VEHICLE_DELETE(__e)	__e.delete_on_death = true; \
								__e notify( "death" ); \
								if( !IsAlive( __e ) ) \
								__e Delete();
//VEHICLE TYPE FUNCTIONS - we should maybe break this out into a vehicle.gsh
#define IS_BUFFEL(__e) (__e.vehicletype == "apc_buffel" || __e.vehicletype == "apc_buffel_gun_turret" || __e.vehicletype == "apc_buffel_gun_turret_nophysics")
#define IS_ELAND(__e) (__e.vehicletype == "tank_eland")
#define IS_QUADROTOR(__e) (__e.vehicletype == "heli_quadrotor")
#define IS_MIG(__e) (__e.vehicleclass == "plane_mig17" || __e.vehicleclass == "plane_mig21")

//
#define GROUNDPOS(__e,_origin) (bullettrace(_origin,(_origin + ( 0, 0, -100000 ) ), 0, __e )[ "position" ])

#define BINK_IS_LOOPING			true
#define BINK_IN_MEMORY			true
#define BINK_START_PAUSED		true
#define BINK_SYNC_AUDIO			true
#define BINK_NORMAL_SIZE		0
#define BINK_DOUBLE_SIZE		1
#define BINK_FULLSCREEN			2
	
#define N_GAMESKILL_EASY		0
#define N_GAMESKILL_NORMAL		1
#define N_GAMESKILL_HARD		2
#define N_GAMESKILL_VETERAN		3
#define N_GAMESKILL_REALISTIC	4

// script defines for fog bank bit mask

#define FOG_BANK_1		1
#define	FOG_BANK_2		2
#define	FOG_BANK_3		4
#define FOG_BANK_4		8

//Analog stick layout stat values
#define THUMBSTICK_DEFAULT	 0
#define THUMBSTICK_SOUTHPAW	 1
#define THUMBSTICK_LEGACY	 2
#define THUMBSTICK_LEGACYSOUTHPAW	 3
	
#define BUTTON_USE		0
#define BUTTON_STANCE	1
#define BUTTON_ADS		2
#define BUTTON_ATTACK	3
#define BUTTON_UP		4 // dev only
#define BUTTON_DOWN		5 // dev only
#define BUTTON_RIGHT	6
#define BUTTON_LEFT		7
	
//ID FLAGS
//code-defined in g_local.h:
//also matches scripts\zm\_callbacks.gsc

#define IDFLAGS_NOFLAG							0	// damage
#define IDFLAGS_RADIUS							1	// damage was indirect
#define	IDFLAGS_NO_ARMOR						2	// armor does not protect from this damage
#define	IDFLAGS_NO_KNOCKBACK					4	// do not affect velocity, just view angles
#define	IDFLAGS_PENETRATION						8	// damage occurred after one or more penetrations
#define	IDFLAGS_DESTRUCTIBLE_ENTITY				16	// force the destructible system to do damage to the entity
#define	IDFLAGS_SHIELD_EXPLOSIVE_IMPACT			32	// missile impacted on the front of the victim's shield
#define	IDFLAGS_SHIELD_EXPLOSIVE_IMPACT_HUGE	64	//   ...and was from a projectile with "Big Explosion" checked on.
#define	IDFLAGS_SHIELD_EXPLOSIVE_SPLASH			128	// explosive splash, somewhat deflected by the victim's shield
#define IDFLAGS_HURT_TRIGGER_ALLOW_LASTSTAND	256 // The trigger that applied the damage will ignore laststand
#define IDFLAGS_DISABLE_RAGDOLL_SKIP			512 // Don't go to ragdoll if got damage while playing death animation
#define IDFLAGS_POWER_ARMOR						1024 // Hit player power armor
	
// script-defined:
#define	IDFLAGS_NO_TEAM_PROTECTION				1024
#define	IDFLAGS_NO_PROTECTION					2048
#define	IDFLAGS_PASSTHRU						4096

// TODO: can probably be remove once the real awareness system is up
#define STATE_UNAWARE		"unaware"
#define STATE_SUSPICIOUS	"low_alert"
#define STATE_ALERTED		"high_alert"
#define STATE_COMBAT		"combat"

//use this macro to smooth out periodic updates that don't need to occur at a tightly fixed interval.  Over time monitor loops will tend to spread out, removing large performance spikes
#define RANDOM_TIME_IN_RANGE(__time,__delta)  randomfloatrange(__time-__delta,__time+__delta)
#define WAIT_ABOUT(__time) wait RANDOM_TIME_IN_RANGE(__time,__time/3)

// IF_DEFINED MACROS	
#define DO_IF_DEFINED(__func) if ( isdefined( __func ) ) { [[ __func ]](); }
#define THREAD_IF_DEFINED(__func) if ( isdefined( __func ) ) { self thread [[ __func ]](); }
#define DELETE_IF_DEFINED(__ent) if ( isdefined( __ent ) ) { __ent Delete(); }
#define SET_IF_DEFINED(__target,__src) if ( isdefined( __src) ) { __target = __src; }

#define DELETE(__obj)			\
	__obj notify( "death" );	\
	__obj = undefined

#define SCR_BUNDLE(__name) level.scriptbundles[ SCRIPTBUNDLE ][ __name ]

#define STATS_TABLE_CP "gamedata/stats/cp/cp_statstable.csv"
#define STATS_TABLE_MP "gamedata/stats/mp/mp_statstable.csv"
#define STATS_TABLE_ZM "gamedata/stats/zm/zm_statstable.csv"
#define LOOT_CONFIG_TABLE "gamedata/loot/loot_config.csv"

//********************************************************************************	
//MOVED FROM _MENU.GSC
//********************************************************************************	
#define MENU_TEAM						"ChangeTeam"
#define MENU_START_MENU					"StartMenu_Main"
#define MENU_CLASS						"class"
#define MENU_CHANGE_CLASS				"ChooseClass_InGame"
#define MENU_EDIT_CHOOSE_CLASS			"chooseClass"
#define MENU_CONTROLS					"ingame_controls"
#define MENU_OPTIONS					"ingame_options"
#define MENU_LEAVEGAME					"popup_leavegame"
#define MENU_SPECTATE					"spectate"
#define MENU_RESTART_GAME				"restartgamepopup"
#define MENU_SCOREBOARD 				"scoreboard"
#define MENU_INIT_TEAM_ALLIES			"initteam_marines"
#define MENU_INIT_TEAM_AXIS				"initteam_opfor"
#define MENU_WAGER_SIDE_BET				"sidebet"
#define MENU_WAGER_SIDE_BET_PLAYER		"sidebet_player"
#define MENU_CHANGE_CLASS_WAGER			"changeclass_wager"
#define MENU_CHANGE_CLASS_CUSTOM		"changeclass_custom"
#define MENU_CHANGE_CLASS_BAREBONES		"changeclass_barebones"
#define MENU_FREERUN_RESTART			"freerun_endgame_popup"

//********************************************************************************	
// FILTER_SHARED 
//********************************************************************************	
#define FILTER_INDEX_OOB 									0
#define FILTER_INDEX_WATER_MIST 							0
#define FILTER_INDEX_WATER_SHEET 							1
#define FILTER_INDEX_EMP 									2
#define FILTER_INDEX_GADGET									3
#define FILTER_INDEX_VEHICLE 								3
#define FILTER_INDEX_PLAYER_SHOCK 							4
#define FILTER_INDEX_BLOOD 									2
#define FILTER_INDEX_ENVIRONMENT							5
#define FILTER_INDEX_BONUSZM 								7 // Used by campaign zombies. DO NOT CHANGE

#define FILTER_INDEX_VISION_PULSE 							FILTER_INDEX_GADGET
#define FILTER_INDEX_SPEED_BURST 							FILTER_INDEX_GADGET
#define FILTER_INDEX_OVERDRIVE	 							FILTER_INDEX_GADGET
	
#define FILTER_INDEX_DIRT 									FILTER_INDEX_ENVIRONMENT
#define FILTER_INDEX_DROWN 									FILTER_INDEX_ENVIRONMENT
	
#define SPEED_BURST_CONSTANT_INDEX_BLUR_AMOUNT 				0
#define SPEED_BURST_CONSTANT_INDEX_GLOW_PULSE 				1
#define SPEED_BURST_CONSTANT_INDEX_IRIS_ZOOM 				2
#define SPEED_BURST_CONSTANT_INDEX_IRIS_BASE_GLOW			3
#define SPEED_BURST_CONSTANT_INDEX_DESATURATION				4
#define SPEED_BURST_CONSTANT_INDEX_BLUR_ORIGIN_X			5
#define SPEED_BURST_CONSTANT_INDEX_BLUR_ORIGIN_Y			6
	
// flags for auto-deleting entities with util::auto_delete
#define DELETE_SAFE			1	// only delete if ent is behind player AND sight blocked
#define DELETE_BEHIND		2	// delete if ent is behind player
#define DELETE_BLOCKED		4	// delete if ent is sight blocked
#define DELETE_BOTH			8	// delete if ent is behind player OR sight blocked
#define DELETE_AGGRESSIVE	16	// same as DELETE_BOTH but quicker

#define COS_45	 0.7071
	
#define STREAMER_HINT_TIMEOUT 15

// "toggle_lights" client fields
#define CF_TOGGLE_LIGHTS_ON 				0
#define CF_TOGGLE_LIGHTS_OFF				1
#define CF_TOGGLE_LIGHTS_FORCE_ALLIES		2
#define CF_TOGGLE_LIGHTS_FORCE_AXIS			3
	
#define SAFEHOUSE_CAIRO 1
#define SAFEHOUSE_MOBILE 2
#define SAFEHOUSE_SINGAPORE 3
	
//********************************************************************************	
// START - SUMEET - DO NOT MODIFY - USED FOR CAMPAIGN ZOMBIES MODE
//********************************************************************************		

#define IS_BONUSZM  ( SessionModeIsCampaignZombiesGame() )
#define NOT_FOR_BONUSZM if( IS_BONUSZM ) return;
#define ONLY_FOR_BONUSZM if( !IS_BONUSZM ) return;
	
#define BONUSZM_WAIT_FOR_OBJECTIVE_COMPLETION	if( IsDefined(level.BZMWaitForObjectiveCompletionCallback) ) [[level.BZMWaitForObjectiveCompletionCallback]]();
#define BONUSZM_START_OBJECTIVE(__skiptoname,__starting)	if( IsDefined(level.BZMStartObjectiveCallback) ) [[level.BZMStartObjectiveCallback]](__skiptoname,__starting);
#define BONUSZM_WAITTILL_AI_CLEARED	if( IsDefined(level.BZMUtil_WaitForAllZombiesToDie) ) [[level.BZMUtil_WaitForAllZombiesToDie]]();
#define BONUSZM_FORCE_AI_CLEANUP if( IsDefined(level.BZM_ForceAICleanup) ) [[level.BZM_ForceAICleanup]]();;

#define BONUSZM_CYBERCOM_ON_CALLBACK(__player)	if( IsDefined( level.BZMOnCyberComOnCallback ) ) level thread [[level.BZMOnCyberComOnCallback]](__player);

#define BONUSZM_ENDON_SCENE_SEQUENCE_ENDED	level endon("BZM_SceneSeqEnded");
#define BONUSZM_SCENE_SEQUENCE_ENDED_CALLBACK(__scene) if( IsDefined( level.BZM_SceneSeqEndedCallback ) ) level thread [[level.BZM_SceneSeqEndedCallback]](__scene);

#define BONUSZM_DATA_OVERRIDE_LOCOMOTION(__walkPercent,__runPercent,__sprintPercent) if(IsDefined(level.BZM_OverrideLocomotion)) level thread [[level.BZM_OverrideLocomotion]](__walkPercent,__runPercent,__sprintPercent);
#define BONUSZM_DATA_OVERRIDE_HEALTH(__levelOneHealth,__levelTwoHealth,__levelThreeHealth) if(IsDefined(level.BZM_OverrideHealth)) level thread [[level.BZM_OverrideHealth]](__levelOneHealth,__levelTwoHealth,__levelThreeHealth);
#define BONUSZM_DATA_OVERRIDE_SUICIDE_CHANCE(__chance) if(IsDefined(level.BZM_OverrideSuicidalChance)) level thread [[level.BZM_OverrideSuicidalChance]](__chance);

#define BONUSZM_CHANGE_LOADOUT	if(IsDefined(level.BZMLoadOutChangeCallback)) level thread [[level.BZMLoadOutChangeCallback]]();

#define BONUSZM_WAIT_FOR_STREAMER_TIMEOUT(__entity,__timeout) if(IsDefined(level.BZM_WaitForStreamerOrTimeOut)) [[level.BZM_WaitForStreamerOrTimeOut]](__entity,__timeout);

#define BONUSZM_HIDE_MAGICBOXES	if(IsDefined(level.BZM_HideAllMagicBoxesCallback)) level thread [[level.BZM_HideAllMagicBoxesCallback]]();
#define BONUSZM_CLEANUP_MAGICBOX(__entity) if(IsDefined(level.BZM_CleanupMagicBoxOnDeletionCallback)) [[level.BZM_CleanupMagicBoxOnDeletionCallback]](__entity);
// SGEN
#define BONUSZM_SGEN_DLG1 	if( IsDefined(level.BZM_SGENDialogue1Callback) ) level thread [[level.BZM_SGENDialogue1Callback]]();
#define BONUSZM_SGEN_DLG1_1 	if( IsDefined(level.BZM_SGENDialogue1_1Callback) ) level thread [[level.BZM_SGENDialogue1_1Callback]]();
#define BONUSZM_SGEN_DLG1_2 	if( IsDefined(level.BZM_SGENDialogue1_2Callback) ) level thread [[level.BZM_SGENDialogue1_2Callback]]();

#define BONUSZM_SGEN_DLG1_3 	if( IsDefined(level.BZM_SGENDialogue1_3Callback) ) level thread [[level.BZM_SGENDialogue1_3Callback]]();
#define	BONUSZM_SGEN_DLG1_3_WAITTILL_DONE if( IsDefined(level.BZM_SGENDialogue1_3Callback_Waittill_Done) ) [[level.BZM_SGENDialogue1_3Callback_Waittill_Done]]();


#define BONUSZM_SGEN_DLG2 	if( IsDefined(level.BZM_SGENDialogue2Callback) ) level thread [[level.BZM_SGENDialogue2Callback]]();
#define BONUSZM_SGEN_DLG2_1 	if( IsDefined(level.BZM_SGENDialogue2_1Callback) ) level thread [[level.BZM_SGENDialogue2_1Callback]]();
#define BONUSZM_SGEN_DLG3 	if( IsDefined(level.BZM_SGENDialogue3Callback) ) level thread [[level.BZM_SGENDialogue3Callback]]();
#define BONUSZM_SGEN_DLG4 	if( IsDefined(level.BZM_SGENDialogue4Callback) ) level thread [[level.BZM_SGENDialogue4Callback]]();
#define BONUSZM_SGEN_DLG4_1 	if( IsDefined(level.BZM_SGENDialogue4_1Callback) ) level thread [[level.BZM_SGENDialogue4_1Callback]]();

#define BONUSZM_SGEN_DLG5 	if( IsDefined(level.BZM_SGENDialogue5Callback) ) level thread [[level.BZM_SGENDialogue5Callback]]();
#define BONUSZM_SGEN_DLG6 	if( IsDefined(level.BZM_SGENDialogue6Callback) ) level thread [[level.BZM_SGENDialogue6Callback]]();
#define BONUSZM_SGEN_DLG7 	if( IsDefined(level.BZM_SGENDialogue7Callback) ) level thread [[level.BZM_SGENDialogue7Callback]]();
#define BONUSZM_SGEN_DLG8 	if( IsDefined(level.BZM_SGENDialogue8Callback) ) level thread [[level.BZM_SGENDialogue8Callback]]();
#define BONUSZM_SGEN_DLG8_1 	if( IsDefined(level.BZM_SGENDialogue8_1Callback) ) level thread [[level.BZM_SGENDialogue8_1Callback]]();
#define BONUSZM_SGEN_DLG8_2 	if( IsDefined(level.BZM_SGENDialogue8_2Callback) ) level thread [[level.BZM_SGENDialogue8_2Callback]]();
#define BONUSZM_SGEN_DLG9 	if( IsDefined(level.BZM_SGENDialogue9Callback) ) level thread [[level.BZM_SGENDialogue9Callback]]();



// BIODOMES
#define BONUSZM_BIO_DLG1 	if( IsDefined(level.BZM_BIODialogue1Callback) ) level thread [[level.BZM_BIODialogue1Callback]]();
#define BONUSZM_BIO_DLG2 	if( IsDefined(level.BZM_BIODialogue2Callback) ) level thread [[level.BZM_BIODialogue2Callback]]();
#define BONUSZM_BIO_DLG2_2 	if( IsDefined(level.BZM_BIODialogue2_2Callback) ) level thread [[level.BZM_BIODialogue2_2Callback]]();
#define BONUSZM_BIO_DLG2_3 	if( IsDefined(level.BZM_BIODialogue2_3Callback) ) level thread [[level.BZM_BIODialogue2_3Callback]]();
#define BONUSZM_BIO_DLG2_4 	if( IsDefined(level.BZM_BIODialogue2_4Callback) ) level thread [[level.BZM_BIODialogue2_4Callback]]();

#define BONUSZM_BIO_DLG3 	if( IsDefined(level.BZM_BIODialogue3Callback) ) level thread [[level.BZM_BIODialogue3Callback]]();
#define BONUSZM_BIO_DLG4 	if( IsDefined(level.BZM_BIODialogue4Callback) ) level thread [[level.BZM_BIODialogue4Callback]]();
#define BONUSZM_BIO_DLG5 	if( IsDefined(level.BZM_BIODialogue5Callback) ) level thread [[level.BZM_BIODialogue5Callback]]();
#define BONUSZM_BIO_DLG5_1 	if( IsDefined(level.BZM_BIODialogue5_1Callback) ) level thread [[level.BZM_BIODialogue5_1Callback]]();

#define BONUSZM_BIO_DLG5_2 	if( IsDefined(level.BZM_BIODialogue5_2Callback) ) level thread [[level.BZM_BIODialogue5_2Callback]]();
#define BONUSZM_BIO_DLG5_3 	if( IsDefined(level.BZM_BIODialogue5_3Callback) ) level thread [[level.BZM_BIODialogue5_3Callback]]();
#define BONUSZM_BIO_DLG5_4 	if( IsDefined(level.BZM_BIODialogue5_4Callback) ) level thread [[level.BZM_BIODialogue5_4Callback]]();

#define BONUSZM_BIO_DLG6 	if( IsDefined(level.BZM_BIODialogue6Callback) ) level thread [[level.BZM_BIODialogue6Callback]]();

// PROLOGUE
#define BONUSZM_PROLOGUE_DLG1 	if( IsDefined(level.BZM_PROLOGUEDialogue1Callback) ) level thread [[level.BZM_PROLOGUEDialogue1Callback]]();
#define BONUSZM_PROLOGUE_DLG2 	if( IsDefined(level.BZM_PROLOGUEDialogue2Callback) ) level thread [[level.BZM_PROLOGUEDialogue2Callback]]();
#define BONUSZM_PROLOGUE_DLG2_1 	if( IsDefined(level.BZM_PROLOGUEDialogue2_1Callback) ) level thread [[level.BZM_PROLOGUEDialogue2_1Callback]]();

#define BONUSZM_PROLOGUE_DLG3 	if( IsDefined(level.BZM_PROLOGUEDialogue3Callback) ) level thread [[level.BZM_PROLOGUEDialogue3Callback]]();
#define BONUSZM_PROLOGUE_DLG4 	if( IsDefined(level.BZM_PROLOGUEDialogue4Callback) ) level thread [[level.BZM_PROLOGUEDialogue4Callback]]();
#define BONUSZM_PROLOGUE_DLG5 	if( IsDefined(level.BZM_PROLOGUEDialogue5Callback) ) level thread [[level.BZM_PROLOGUEDialogue5Callback]]();
#define BONUSZM_PROLOGUE_DLG5_1 	if( IsDefined(level.BZM_PROLOGUEDialogue5_1Callback) ) level thread [[level.BZM_PROLOGUEDialogue5_1Callback]]();
#define BONUSZM_PROLOGUE_DLG5_2 	if( IsDefined(level.BZM_PROLOGUEDialogue5_2Callback) ) level thread [[level.BZM_PROLOGUEDialogue5_2Callback]]();
#define BONUSZM_PROLOGUE_DLG5_3 	if( IsDefined(level.BZM_PROLOGUEDialogue5_3Callback) ) level thread [[level.BZM_PROLOGUEDialogue5_3Callback]]();

#define BONUSZM_PROLOGUE_DLG6 	if( IsDefined(level.BZM_PROLOGUEDialogue6Callback) ) level thread [[level.BZM_PROLOGUEDialogue6Callback]]();
#define BONUSZM_PROLOGUE_DLG6_1 	if( IsDefined(level.BZM_PROLOGUEDialogue6_1Callback) ) level thread [[level.BZM_PROLOGUEDialogue6_1Callback]]();
#define BONUSZM_PROLOGUE_DLG6_2 	if( IsDefined(level.BZM_PROLOGUEDialogue6_2Callback) ) level thread [[level.BZM_PROLOGUEDialogue6_2Callback]]();

#define BONUSZM_PROLOGUE_DLG7 	if( IsDefined(level.BZM_PROLOGUEDialogue7Callback) ) level thread [[level.BZM_PROLOGUEDialogue7Callback]]();
#define BONUSZM_PROLOGUE_DLG8 	if( IsDefined(level.BZM_PROLOGUEDialogue8Callback) ) level thread [[level.BZM_PROLOGUEDialogue8Callback]]();

// NEWWORLD
#define BONUSZM_NEWWORLD_DLG1 	if( IsDefined(level.BZM_NEWWORLDDialogue1Callback) ) level thread [[level.BZM_NEWWORLDDialogue1Callback]]();
#define BONUSZM_NEWWORLD_DLG2 	if( IsDefined(level.BZM_NEWWORLDDialogue2Callback) ) level thread [[level.BZM_NEWWORLDDialogue2Callback]]();
#define BONUSZM_NEWWORLD_DLG2_2 	if( IsDefined(level.BZM_NEWWORLDDialogue2_2Callback) ) level thread [[level.BZM_NEWWORLDDialogue2_2Callback]]();
#define BONUSZM_NEWWORLD_DLG2_3 	if( IsDefined(level.BZM_NEWWORLDDialogue2_3Callback) ) level thread [[level.BZM_NEWWORLDDialogue2_3Callback]]();
#define BONUSZM_NEWWORLD_DLG2_4 	if( IsDefined(level.BZM_NEWWORLDDialogue2_4Callback) ) level thread [[level.BZM_NEWWORLDDialogue2_4Callback]]();

#define BONUSZM_NEWWORLD_DLG3 	if( IsDefined(level.BZM_NEWWORLDDialogue3Callback) ) level thread [[level.BZM_NEWWORLDDialogue3Callback]]();
#define BONUSZM_NEWWORLD_DLG4 	if( IsDefined(level.BZM_NEWWORLDDialogue4Callback) ) level thread [[level.BZM_NEWWORLDDialogue4Callback]]();
#define BONUSZM_NEWWORLD_DLG5 	if( IsDefined(level.BZM_NEWWORLDDialogue5Callback) ) level thread [[level.BZM_NEWWORLDDialogue5Callback]]();
#define BONUSZM_NEWWORLD_DLG6 	if( IsDefined(level.BZM_NEWWORLDDialogue6Callback) ) level thread [[level.BZM_NEWWORLDDialogue6Callback]]();
#define BONUSZM_NEWWORLD_DLG7 	if( IsDefined(level.BZM_NEWWORLDDialogue7Callback) ) level thread [[level.BZM_NEWWORLDDialogue7Callback]]();
#define BONUSZM_NEWWORLD_DLG8 	if( IsDefined(level.BZM_NEWWORLDDialogue8Callback) ) level thread [[level.BZM_NEWWORLDDialogue8Callback]]();
#define BONUSZM_NEWWORLD_DLG9 	if( IsDefined(level.BZM_NEWWORLDDialogue9Callback) ) level thread [[level.BZM_NEWWORLDDialogue9Callback]]();
#define BONUSZM_NEWWORLD_DLG10 	if( IsDefined(level.BZM_NEWWORLDDialogue10Callback) ) level thread [[level.BZM_NEWWORLDDialogue10Callback]]();
#define BONUSZM_NEWWORLD_DLG11 	if( IsDefined(level.BZM_NEWWORLDDialogue11Callback) ) level thread [[level.BZM_NEWWORLDDialogue11Callback]]();
#define BONUSZM_NEWWORLD_DLG12 	if( IsDefined(level.BZM_NEWWORLDDialogue12Callback) ) level thread [[level.BZM_NEWWORLDDialogue12Callback]]();

// LOTUS
#define BONUSZM_LOTUS_DLG1 	if( IsDefined(level.BZM_LOTUSDialogue1Callback) ) level thread [[level.BZM_LOTUSDialogue1Callback]]();
#define BONUSZM_LOTUS_DLG2 	if( IsDefined(level.BZM_LOTUSDialogue2Callback) ) level thread [[level.BZM_LOTUSDialogue2Callback]]();
#define BONUSZM_LOTUS_DLG2_1 	if( IsDefined(level.BZM_LOTUSDialogue2_1Callback) ) level thread [[level.BZM_LOTUSDialogue2_1Callback]]();
#define BONUSZM_LOTUS_DLG2_2 	if( IsDefined(level.BZM_LOTUSDialogue2_2Callback) ) level thread [[level.BZM_LOTUSDialogue2_2Callback]]();
#define BONUSZM_LOTUS_DLG2_3 	if( IsDefined(level.BZM_LOTUSDialogue2_3Callback) ) level thread [[level.BZM_LOTUSDialogue2_3Callback]]();
#define BONUSZM_LOTUS_DLG2_4 	if( IsDefined(level.BZM_LOTUSDialogue2_4Callback) ) level thread [[level.BZM_LOTUSDialogue2_4Callback]]();

#define BONUSZM_LOTUS_DLG3 	if( IsDefined(level.BZM_LOTUSDialogue3Callback) ) level thread [[level.BZM_LOTUSDialogue3Callback]]();
#define BONUSZM_LOTUS_DLG3_2 	if( IsDefined(level.BZM_LOTUSDialogue3_2Callback) ) level thread [[level.BZM_LOTUSDialogue3_2Callback]]();

#define BONUSZM_LOTUS_DLG4 	if( IsDefined(level.BZM_LOTUSDialogue4Callback) ) level thread [[level.BZM_LOTUSDialogue4Callback]]();
#define BONUSZM_LOTUS_DLG4_1 	if( IsDefined(level.BZM_LOTUSDialogue4_1Callback) ) level thread [[level.BZM_LOTUSDialogue4_1Callback]]();
#define BONUSZM_LOTUS_DLG4_2 	if( IsDefined(level.BZM_LOTUSDialogue4_2Callback) ) level thread [[level.BZM_LOTUSDialogue4_2Callback]]();

#define BONUSZM_LOTUS_DLG6 	if( IsDefined(level.BZM_LOTUSDialogue6Callback) ) level thread [[level.BZM_LOTUSDialogue6Callback]]();
#define BONUSZM_LOTUS_DLG7 	if( IsDefined(level.BZM_LOTUSDialogue7Callback) ) level thread [[level.BZM_LOTUSDialogue7Callback]]();
#define BONUSZM_LOTUS_DLG7_1 	if( IsDefined(level.BZM_LOTUSDialogue7_1Callback) ) level thread [[level.BZM_LOTUSDialogue7_1Callback]]();

#define BONUSZM_LOTUS_DLG8 	if( IsDefined(level.BZM_LOTUSDialogue8Callback) ) level thread [[level.BZM_LOTUSDialogue8Callback]]();
#define BONUSZM_LOTUS_DLG9 	if( IsDefined(level.BZM_LOTUSDialogue9Callback) ) level thread [[level.BZM_LOTUSDialogue9Callback]]();
#define BONUSZM_LOTUS_DLG10 	if( IsDefined(level.BZM_LOTUSDialogue10Callback) ) level thread [[level.BZM_LOTUSDialogue10Callback]]();

#define BONUSZM_LOTUS_DLG11 if( IsDefined(level.BZM_LOTUSDialogue11Callback) ) level thread [[level.BZM_LOTUSDialogue11Callback]]();
#define BONUSZM_LOTUS_DLG12 if( IsDefined(level.BZM_LOTUSDialogue12Callback) ) level thread [[level.BZM_LOTUSDialogue12Callback]]();
#define BONUSZM_LOTUS_DLG13 if( IsDefined(level.BZM_LOTUSDialogue13Callback) ) level thread [[level.BZM_LOTUSDialogue13Callback]]();
#define BONUSZM_LOTUS_DLG14 if( IsDefined(level.BZM_LOTUSDialogue14Callback) ) level thread [[level.BZM_LOTUSDialogue14Callback]]();

// RAMSES
#define BONUSZM_RAMSES_DLG1 	if( IsDefined(level.BZM_RAMSESDialogue1Callback) ) level thread [[level.BZM_RAMSESDialogue1Callback]]();
#define BONUSZM_RAMSES_DLG2 	if( IsDefined(level.BZM_RAMSESDialogue2Callback) ) level thread [[level.BZM_RAMSESDialogue2Callback]]();
#define BONUSZM_RAMSES_DLG3 	if( IsDefined(level.BZM_RAMSESDialogue3Callback) ) level thread [[level.BZM_RAMSESDialogue3Callback]]();
#define BONUSZM_RAMSES_DLG3_1 	if( IsDefined(level.BZM_RAMSESDialogue3_1Callback) ) level thread [[level.BZM_RAMSESDialogue3_1Callback]]();
#define BONUSZM_RAMSES_DLG3_2 	if( IsDefined(level.BZM_RAMSESDialogue3_2Callback) ) level thread [[level.BZM_RAMSESDialogue3_2Callback]]();

#define BONUSZM_RAMSES_DLG5 	if( IsDefined(level.BZM_RAMSESDialogue5Callback) ) level thread [[level.BZM_RAMSESDialogue5Callback]]();
#define BONUSZM_RAMSES_DLG5_1 	if( IsDefined(level.BZM_RAMSESDialogue5_1Callback) ) level thread [[level.BZM_RAMSESDialogue5_1Callback]]();

#define BONUSZM_RAMSES_DLG6 	if( IsDefined(level.BZM_RAMSESDialogue6Callback) ) level thread [[level.BZM_RAMSESDialogue6Callback]]();
#define BONUSZM_RAMSES_DLG7 	if( IsDefined(level.BZM_RAMSESDialogue7Callback) ) level thread [[level.BZM_RAMSESDialogue7Callback]]();
#define BONUSZM_RAMSES_DLG7_1 	if( IsDefined(level.BZM_RAMSESDialogue7_1Callback) ) level thread [[level.BZM_RAMSESDialogue7_1Callback]]();

#define BONUSZM_RAMSES_DLG8 	if( IsDefined(level.BZM_RAMSESDialogue8Callback) ) level thread [[level.BZM_RAMSESDialogue8Callback]]();

// INFECTION
#define BONUSZM_INFECTION_DLG1 	if( IsDefined(level.BZM_INFECTIONDialogue1Callback) ) level thread [[level.BZM_INFECTIONDialogue1Callback]]();
#define BONUSZM_INFECTION_DLG2 	if( IsDefined(level.BZM_INFECTIONDialogue2Callback) ) level thread [[level.BZM_INFECTIONDialogue2Callback]]();
#define BONUSZM_INFECTION_DLG3 	if( IsDefined(level.BZM_INFECTIONDialogue3Callback) ) level thread [[level.BZM_INFECTIONDialogue3Callback]]();
#define BONUSZM_INFECTION_DLG3_1 	if( IsDefined(level.BZM_INFECTIONDialogue3_1Callback) ) level thread [[level.BZM_INFECTIONDialogue3_1Callback]]();

#define BONUSZM_INFECTION_DLG4 	if( IsDefined(level.BZM_INFECTIONDialogue4Callback) ) level thread [[level.BZM_INFECTIONDialogue4Callback]]();
#define BONUSZM_INFECTION_DLG5 	if( IsDefined(level.BZM_INFECTIONDialogue5Callback) ) level thread [[level.BZM_INFECTIONDialogue5Callback]]();
#define BONUSZM_INFECTION_DLG6 	if( IsDefined(level.BZM_INFECTIONDialogue6Callback) ) level thread [[level.BZM_INFECTIONDialogue6Callback]]();
#define BONUSZM_INFECTION_DLG7 	if( IsDefined(level.BZM_INFECTIONDialogue7Callback) ) level thread [[level.BZM_INFECTIONDialogue7Callback]]();
#define BONUSZM_INFECTION_DLG8 	if( IsDefined(level.BZM_INFECTIONDialogue8Callback) ) level thread [[level.BZM_INFECTIONDialogue8Callback]]();
#define BONUSZM_INFECTION_DLG8_1 	if( IsDefined(level.BZM_INFECTIONDialogue8_1Callback) ) level thread [[level.BZM_INFECTIONDialogue8_1Callback]]();

#define BONUSZM_INFECTION_DLG9 	if( IsDefined(level.BZM_INFECTIONDialogue9Callback) ) level thread [[level.BZM_INFECTIONDialogue9Callback]]();
#define BONUSZM_INFECTION_DLG10 if( IsDefined(level.BZM_INFECTIONDialogue10Callback) ) level thread [[level.BZM_INFECTIONDialogue10Callback]]();
#define BONUSZM_INFECTION_DLG11 if( IsDefined(level.BZM_INFECTIONDialogue11Callback) ) level thread [[level.BZM_INFECTIONDialogue11Callback]]();
#define BONUSZM_INFECTION_DLG12 if( IsDefined(level.BZM_INFECTIONDialogue12Callback) ) level thread [[level.BZM_INFECTIONDialogue12Callback]]();

#define BONUSZM_INFECTION_DLG13 if( IsDefined(level.BZM_INFECTIONDialogue13Callback) ) level thread [[level.BZM_INFECTIONDialogue13Callback]]();
#define BONUSZM_INFECTION_DLG14 if( IsDefined(level.BZM_INFECTIONDialogue14Callback) ) level thread [[level.BZM_INFECTIONDialogue14Callback]]();
#define BONUSZM_INFECTION_DLG15 if( IsDefined(level.BZM_INFECTIONDialogue15Callback) ) level thread [[level.BZM_INFECTIONDialogue15Callback]]();
#define BONUSZM_INFECTION_DLG16 if( IsDefined(level.BZM_INFECTIONDialogue16Callback) ) level thread [[level.BZM_INFECTIONDialogue16Callback]]();
#define BONUSZM_INFECTION_DLG17 if( IsDefined(level.BZM_INFECTIONDialogue17Callback) ) level thread [[level.BZM_INFECTIONDialogue17Callback]]();
#define BONUSZM_INFECTION_DLG18 if( IsDefined(level.BZM_INFECTIONDialogue18Callback) ) level thread [[level.BZM_INFECTIONDialogue18Callback]]();
#define BONUSZM_INFECTION_DLG19 if( IsDefined(level.BZM_INFECTIONDialogue19Callback) ) level thread [[level.BZM_INFECTIONDialogue19Callback]]();
#define BONUSZM_INFECTION_DLG20 if( IsDefined(level.BZM_INFECTIONDialogue20Callback) ) level thread [[level.BZM_INFECTIONDialogue20Callback]]();
#define BONUSZM_INFECTION_DLG21 if( IsDefined(level.BZM_INFECTIONDialogue21Callback) ) level thread [[level.BZM_INFECTIONDialogue21Callback]]();
#define BONUSZM_INFECTION_DLG22 if( IsDefined(level.BZM_INFECTIONDialogue22Callback) ) level thread [[level.BZM_INFECTIONDialogue22Callback]]();
#define BONUSZM_INFECTION_DLG23 if( IsDefined(level.BZM_INFECTIONDialogue23Callback) ) level thread [[level.BZM_INFECTIONDialogue23Callback]]();

// BLACKSTATION
#define BONUSZM_BLACKSTATION_DLG1 	if( IsDefined(level.BZM_BLACKSTATIONDialogue1Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue1Callback]]();
#define BONUSZM_BLACKSTATION_DLG2 	if( IsDefined(level.BZM_BLACKSTATIONDialogue2Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue2Callback]]();
#define BONUSZM_BLACKSTATION_DLG3 	if( IsDefined(level.BZM_BLACKSTATIONDialogue3Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue3Callback]]();
#define BONUSZM_BLACKSTATION_DLG3_1 	if( IsDefined(level.BZM_BLACKSTATIONDialogue3_1Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue3_1Callback]]();
#define BONUSZM_BLACKSTATION_DLG3_2 	if( IsDefined(level.BZM_BLACKSTATIONDialogue3_2Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue3_2Callback]]();
#define BONUSZM_BLACKSTATION_DLG3_3 	if( IsDefined(level.BZM_BLACKSTATIONDialogue3_3Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue3_3Callback]]();
#define BONUSZM_BLACKSTATION_DLG3_4 	if( IsDefined(level.BZM_BLACKSTATIONDialogue3_4Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue3_4Callback]]();
#define BONUSZM_BLACKSTATION_DLG3_5 	if( IsDefined(level.BZM_BLACKSTATIONDialogue3_5Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue3_5Callback]]();

#define BONUSZM_BLACKSTATION_DLG4 	if( IsDefined(level.BZM_BLACKSTATIONDialogue4Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue4Callback]]();
#define BONUSZM_BLACKSTATION_DLG4_1 	if( IsDefined(level.BZM_BLACKSTATIONDialogue4_1Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue4_1Callback]]();
#define BONUSZM_BLACKSTATION_DLG4_2 	if( IsDefined(level.BZM_BLACKSTATIONDialogue4_2Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue4_2Callback]]();
#define BONUSZM_BLACKSTATION_DLG4_3 	if( IsDefined(level.BZM_BLACKSTATIONDialogue4_3Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue4_3Callback]]();

#define BONUSZM_BLACKSTATION_DLG5 	if( IsDefined(level.BZM_BLACKSTATIONDialogue5Callback) ) level thread [[level.BZM_BLACKSTATIONDialogue5Callback]]();

// AQUIFER
#define BONUSZM_AQUIFER_DLG1 	if( IsDefined(level.BZM_AQUIFERDialogue1Callback) ) level thread [[level.BZM_AQUIFERDialogue1Callback]]();
#define BONUSZM_AQUIFER_DLG1_1 	if( IsDefined(level.BZM_AQUIFERDialogue1_1Callback) ) level thread [[level.BZM_AQUIFERDialogue1_1Callback]]();
#define BONUSZM_AQUIFER_DLG1_2 	if( IsDefined(level.BZM_AQUIFERDialogue1_2Callback) ) level thread [[level.BZM_AQUIFERDialogue1_2Callback]]();
#define BONUSZM_AQUIFER_DLG1_3 	if( IsDefined(level.BZM_AQUIFERDialogue1_3Callback) ) level thread [[level.BZM_AQUIFERDialogue1_3Callback]]();
#define BONUSZM_AQUIFER_DLG1_4 	if( IsDefined(level.BZM_AQUIFERDialogue1_4Callback) ) level thread [[level.BZM_AQUIFERDialogue1_4Callback]]();
#define BONUSZM_AQUIFER_DLG1_4_1 	if( IsDefined(level.BZM_AQUIFERDialogue1_4_1Callback) ) level thread [[level.BZM_AQUIFERDialogue1_4_1Callback]]();

#define BONUSZM_AQUIFER_DLG1_5 	if( IsDefined(level.BZM_AQUIFERDialogue1_5Callback) ) level thread [[level.BZM_AQUIFERDialogue1_5Callback]]();
#define BONUSZM_AQUIFER_DLG1_6 	if( IsDefined(level.BZM_AQUIFERDialogue1_6Callback) ) level thread [[level.BZM_AQUIFERDialogue1_6Callback]]();
#define BONUSZM_AQUIFER_DLG1_7 	if( IsDefined(level.BZM_AQUIFERDialogue1_7Callback) ) level thread [[level.BZM_AQUIFERDialogue1_7Callback]]();


#define BONUSZM_AQUIFER_DLG2 	if( IsDefined(level.BZM_AQUIFERDialogue2Callback) ) level thread [[level.BZM_AQUIFERDialogue2Callback]]();
#define BONUSZM_AQUIFER_DLG2_1 	if( IsDefined(level.BZM_AQUIFERDialogue2_1Callback) ) level thread [[level.BZM_AQUIFERDialogue2_1Callback]]();

#define BONUSZM_AQUIFER_DLG3 	if( IsDefined(level.BZM_AQUIFERDialogue3Callback) ) level thread [[level.BZM_AQUIFERDialogue3Callback]]();
#define BONUSZM_AQUIFER_DLG3_1 	if( IsDefined(level.BZM_AQUIFERDialogue3_1Callback) ) level thread [[level.BZM_AQUIFERDialogue3_1Callback]]();

#define BONUSZM_AQUIFER_DLG4 	if( IsDefined(level.BZM_AQUIFERDialogue4Callback) ) level thread [[level.BZM_AQUIFERDialogue4Callback]]();
#define BONUSZM_AQUIFER_DLG4_1 	if( IsDefined(level.BZM_AQUIFERDialogue4_1Callback) ) level thread [[level.BZM_AQUIFERDialogue4_1Callback]]();

#define BONUSZM_AQUIFER_DLG5 	if( IsDefined(level.BZM_AQUIFERDialogue5Callback) ) level thread [[level.BZM_AQUIFERDialogue5Callback]]();
#define BONUSZM_AQUIFER_DLG6 	if( IsDefined(level.BZM_AQUIFERDialogue6Callback) ) level thread [[level.BZM_AQUIFERDialogue6Callback]]();
#define BONUSZM_AQUIFER_DLG7 	if( IsDefined(level.BZM_AQUIFERDialogue7Callback) ) level thread [[level.BZM_AQUIFERDialogue7Callback]]();
#define BONUSZM_AQUIFER_DLG7_1 	if( IsDefined(level.BZM_AQUIFERDialogue7_1Callback) ) level thread [[level.BZM_AQUIFERDialogue7_1Callback]]();


// VENGEANCE
#define BONUSZM_VENGEANCE_DLG1 	if( IsDefined(level.BZM_VENGEANCEDialogue1Callback) ) level thread [[level.BZM_VENGEANCEDialogue1Callback]]();

#define BONUSZM_VENGEANCE_DLG2 	if( IsDefined(level.BZM_VENGEANCEDialogue2Callback) ) level thread [[level.BZM_VENGEANCEDialogue2Callback]]();
#define BONUSZM_VENGEANCE_DLG2_1 	if( IsDefined(level.BZM_VENGEANCEDialogue2_1Callback) ) level thread [[level.BZM_VENGEANCEDialogue2_1Callback]]();

#define BONUSZM_VENGEANCE_DLG3 	if( IsDefined(level.BZM_VENGEANCEDialogue3Callback) ) level thread [[level.BZM_VENGEANCEDialogue3Callback]]();
#define BONUSZM_VENGEANCE_DLG3_1 	if( IsDefined(level.BZM_VENGEANCEDialogue3_1Callback) ) level thread [[level.BZM_VENGEANCEDialogue3_1Callback]]();

#define BONUSZM_VENGEANCE_DLG4 	if( IsDefined(level.BZM_VENGEANCEDialogue4Callback) ) level thread [[level.BZM_VENGEANCEDialogue4Callback]]();
#define BONUSZM_VENGEANCE_DLG5 	if( IsDefined(level.BZM_VENGEANCEDialogue5Callback) ) level thread [[level.BZM_VENGEANCEDialogue5Callback]]();

#define BONUSZM_VENGEANCE_DLG6 	if( IsDefined(level.BZM_VENGEANCEDialogue6Callback) ) level thread [[level.BZM_VENGEANCEDialogue6Callback]]();
#define BONUSZM_VENGEANCE_DLG6_1 	if( IsDefined(level.BZM_VENGEANCEDialogue6_1Callback) ) level thread [[level.BZM_VENGEANCEDialogue6_1Callback]]();
#define BONUSZM_VENGEANCE_DLG6_2 	if( IsDefined(level.BZM_VENGEANCEDialogue6_2Callback) ) level thread [[level.BZM_VENGEANCEDialogue6_2Callback]]();

#define BONUSZM_VENGEANCE_DLG7 	if( IsDefined(level.BZM_VENGEANCEDialogue7Callback) ) level thread [[level.BZM_VENGEANCEDialogue7Callback]]();
#define BONUSZM_VENGEANCE_DLG7_1 	if( IsDefined(level.BZM_VENGEANCEDialogue7_1Callback) ) level thread [[level.BZM_VENGEANCEDialogue7_1Callback]]();

#define BONUSZM_VENGEANCE_DLG8 	if( IsDefined(level.BZM_VENGEANCEDialogue8Callback) ) level thread [[level.BZM_VENGEANCEDialogue8Callback]]();
#define BONUSZM_VENGEANCE_DLG9 	if( IsDefined(level.BZM_VENGEANCEDialogue9Callback) ) level thread [[level.BZM_VENGEANCEDialogue9Callback]]();


// ZURICH
#define BONUSZM_ZURICH_DLG1 	if( IsDefined(level.BZM_ZURICHDialogue1Callback) ) level thread [[level.BZM_ZURICHDialogue1Callback]]();
#define BONUSZM_ZURICH_DLG1_1 	if( IsDefined(level.BZM_ZURICHDialogue1_1Callback) ) level thread [[level.BZM_ZURICHDialogue1_1Callback]]();
#define BONUSZM_ZURICH_DLG1_2 	if( IsDefined(level.BZM_ZURICHDialogue1_2Callback) ) level thread [[level.BZM_ZURICHDialogue1_2Callback]]();
#define BONUSZM_ZURICH_DLG1_3 	if( IsDefined(level.BZM_ZURICHDialogue1_3Callback) ) level thread [[level.BZM_ZURICHDialogue1_3Callback]]();
#define BONUSZM_ZURICH_DLG1_4 	if( IsDefined(level.BZM_ZURICHDialogue1_4Callback) ) level thread [[level.BZM_ZURICHDialogue1_4Callback]]();

#define BONUSZM_ZURICH_DLG2 	if( IsDefined(level.BZM_ZURICHDialogue2Callback) ) level thread [[level.BZM_ZURICHDialogue2Callback]]();
#define BONUSZM_ZURICH_DLG3 	if( IsDefined(level.BZM_ZURICHDialogue3Callback) ) level thread [[level.BZM_ZURICHDialogue3Callback]]();
#define BONUSZM_ZURICH_DLG4 	if( IsDefined(level.BZM_ZURICHDialogue4Callback) ) level thread [[level.BZM_ZURICHDialogue4Callback]]();
#define BONUSZM_ZURICH_DLG4_1 	if( IsDefined(level.BZM_ZURICHDialogue4_1Callback) ) level thread [[level.BZM_ZURICHDialogue4_1Callback]]();

#define BONUSZM_ZURICH_DLG5 	if( IsDefined(level.BZM_ZURICHDialogue5Callback) ) level thread [[level.BZM_ZURICHDialogue5Callback]]();
#define BONUSZM_ZURICH_DLG6 	if( IsDefined(level.BZM_ZURICHDialogue6Callback) ) level thread [[level.BZM_ZURICHDialogue6Callback]]();
#define BONUSZM_ZURICH_DLG7 	if( IsDefined(level.BZM_ZURICHDialogue7Callback) ) level thread [[level.BZM_ZURICHDialogue7Callback]]();
#define BONUSZM_ZURICH_DLG8 	if( IsDefined(level.BZM_ZURICHDialogue8Callback) ) level thread [[level.BZM_ZURICHDialogue8Callback]]();
#define BONUSZM_ZURICH_DLG9 	if( IsDefined(level.BZM_ZURICHDialogue9Callback) ) level thread [[level.BZM_ZURICHDialogue9Callback]]();
#define BONUSZM_ZURICH_DLG10 if( IsDefined(level.BZM_ZURICHDialogue10Callback) ) level thread [[level.BZM_ZURICHDialogue10Callback]]();
#define BONUSZM_ZURICH_DLG11 if( IsDefined(level.BZM_ZURICHDialogue11Callback) ) level thread [[level.BZM_ZURICHDialogue11Callback]]();
#define BONUSZM_ZURICH_DLG12 if( IsDefined(level.BZM_ZURICHDialogue12Callback) ) level thread [[level.BZM_ZURICHDialogue12Callback]]();
#define BONUSZM_ZURICH_DLG13 if( IsDefined(level.BZM_ZURICHDialogue13Callback) ) level thread [[level.BZM_ZURICHDialogue13Callback]]();
#define BONUSZM_ZURICH_DLG14 if( IsDefined(level.BZM_ZURICHDialogue14Callback) ) level thread [[level.BZM_ZURICHDialogue14Callback]]();
#define BONUSZM_ZURICH_DLG15 if( IsDefined(level.BZM_ZURICHDialogue15Callback) ) level thread [[level.BZM_ZURICHDialogue15Callback]]();
#define BONUSZM_ZURICH_DLG16 if( IsDefined(level.BZM_ZURICHDialogue16Callback) ) level thread [[level.BZM_ZURICHDialogue16Callback]]();
#define BONUSZM_ZURICH_DLG17 if( IsDefined(level.BZM_ZURICHDialogue17Callback) ) level thread [[level.BZM_ZURICHDialogue17Callback]]();
#define BONUSZM_ZURICH_DLG18 if( IsDefined(level.BZM_ZURICHDialogue18Callback) ) level thread [[level.BZM_ZURICHDialogue18Callback]]();
#define BONUSZM_ZURICH_DLG19 if( IsDefined(level.BZM_ZURICHDialogue19Callback) ) level thread [[level.BZM_ZURICHDialogue19Callback]]();
#define BONUSZM_ZURICH_DLG20 if( IsDefined(level.BZM_ZURICHDialogue20Callback) ) level thread [[level.BZM_ZURICHDialogue20Callback]]();
#define BONUSZM_ZURICH_DLG21 if( IsDefined(level.BZM_ZURICHDialogue21Callback) ) level thread [[level.BZM_ZURICHDialogue21Callback]]();
#define BONUSZM_ZURICH_DLG22 if( IsDefined(level.BZM_ZURICHDialogue22Callback) ) level thread [[level.BZM_ZURICHDialogue22Callback]]();
#define BONUSZM_ZURICH_DLG23 if( IsDefined(level.BZM_ZURICHDialogue23Callback) ) level thread [[level.BZM_ZURICHDialogue23Callback]]();
#define BONUSZM_ZURICH_DLG24 if( IsDefined(level.BZM_ZURICHDialogue24Callback) ) level thread [[level.BZM_ZURICHDialogue24Callback]]();

// This one is purposefully not threaded
#define BONUSZM_ZURICH_DLG25 if( IsDefined(level.BZM_ZURICHDialogue25Callback) ) [[level.BZM_ZURICHDialogue25Callback]]();


//********************************************************************************	
// END - SUMEET - DO NOT MODIFY - USED FOR CAMPAIGN ZOMBIES MODE
//********************************************************************************		

#define SET_CLEARANCE_CEILING_WRAPPER(__value) if(IS_BONUSZM && __value<34) SetClearanceCeiling(34); else SetClearanceCeiling(__value);

// detail levels for silhouettes
#define CUTTING_DETAIL_LEVEL_OBB			0
#define CUTTING_DETAIL_LEVEL_CONVEX_HULL	1
#define CUTTING_DETAIL_LEVEL_EXPENSIVE_FULL	2


// HACKER TOOL	
#define HACKER_TOOL_INACTIVE 			0
#define HACKER_TOOL_ACTIVE 				1
#define HACKER_TOOL_HACKING 			2
#define HACKER_TOOL_FIREWALL 			3

#define HACKER_TOOL_STATUS_SCANNING		0
#define HACKER_TOOL_STATUS_BREACHING	1
#define HACKER_TOOL_STATUS_HACKING		2
#define HACKER_TOOL_STATUS_EMPED		3

// Matches r_stream.h
#define STREAM_NO_MIPS 0
#define STREAM_MIP0 1
#define STREAM_MIP1 2
#define STREAM_MIP2 3
#define STREAM_MIP3 4
#define STREAM_MIP4 5

#define STREAM_MIP_LOWEST 1
#define STREAM_MIP_HIGHEST 4
#define STREAM_MIP_EXCLUDE_HIGHEST -1

#define STREAM_NO_LODS 0
#define STREAM_LOD0 1
#define STREAM_LOD1 2
#define STREAM_LOD2 3
#define STREAM_LOD3 4
#define STREAM_LOD4 5
#define STREAM_LOD5 6
#define STREAM_LOD6 7
#define STREAM_LOD7 8

#define STREAM_LOD_LOWEST 1
#define STREAM_LOD_HIGHEST 8
#define STREAM_LOD_EXCLUDE_HIGHEST -1

// INTERACTS
#define INTERACT_HOLD_TIME 0.35
#define INTERACT_STANDARD_HEIGHT 24
	
// JUKE end reasons
#define JUKE_END_REASON_ENVIORMENTAL 	0
#define JUKE_END_REASON_EXCEEDED_TIME	1
#define JUKE_END_REASON_COLLISION		2
#define JUKE_END_REASON_SCRIPTED		3
#define JUKE_END_REASON_REQUESTED		4
#define JUKE_END_REASON_NONE			5

//cp stats table defines	
#define STATS_COST_COLUMN 17
#define STATS_REFERENCE_COLUMN 4
#define STATS_GROUP_COLUMN 2
#define STATS_LOADOUT_SLOT 13
#define STATS_ALLOCATION_COLUMN 12
	
#define CYBERCOM_STATS_START_INDEX 100
#define CYBERCOM_STATS_END_INDEX 141

#define WEAPONS_STATS_START_INDEX 1
#define WEAPONS_STATS_END_INDEX 76
	
#define WEAPONS_STATS_GUNS_START_INDEX 	1
#define WEAPONS_STATS_GUNS_END_INDEX	60

#define CYBERCOM_ABILITIES_PER_WHEEL 14

// CUSTOMIZATION PAINTJOB INVALID ID
// Must match CustomizationPaintjobInvalidID in Lua_CoD_EnumDefinitions.h
#define CUSTOMIZATION_INVALID_PAINTJOB_INDEX 15
#define CUSTOMIZATION_INVALID_PAINTJOB_SLOT 15

	
//Difficulty reporting slots, needs to be kept up to date with the matching enum, difficultySlot_t code side.
#define START_DIFFICULTY_REPORT_INDEX 0
#define END_DIFFICULTY_REPORT_INDEX 1
#define MIN_DIFFICULTY_REPORT_INDEX 2
#define MAX_DIFFICULTY_REPORT_INDEX 3
	
//Match Record Map Events
#define MAPEVENT_UNKNOWN 0
#define MAPEVENT_PLAYER_ENTERS_LAST_STAND 1
	
// ETYPE matches with entityType_t in bg_enttype.h
	
#define ET_GENERAL				0
#define ET_PLAYER				1
#define ET_PLAYER_CORPSE		2
#define ET_ITEM					3
#define ET_MISSILE				4
#define ET_PLAYER_INVISIBLE		5
#define ET_SCRIPTMOVER			6
#define ET_SOUND_BLEND			7
#define ET_FX					8
#define ET_LOOP_FX				9
#define ET_PRIMARY_LIGHT		10
#define ET_LENSFLARE			11
#define ET_REFLECTION_PROBE		12
#define ET_HELICOPTER			13
#define ET_PLANE				14	
#define ET_VEHICLE				15
#define ET_VEHICLE_SPAWNER		16
#define ET_VEHICLE_CORPSE		17
#define ET_ACTOR				18
#define ET_ACTOR_SPAWNER		19
#define ET_ACTOR_CORPSE			20
#define ET_STREAMER_HINT		21
#define ET_ZBARRIER				22
#define ET_TRIGGER				23
