// Notes about scripts
//=====================
//
// Anim variables
// -------------- 
// Anim variables keep track of what the character is doing with respect to his 
// animations.  They know if he's standing, crouching, kneeling, walking, running, etc, 
// so that he can play appropriate transitions to get to the animation he wants.
// anim_movement - "stop", "walk", "run"
// anim_pose - "stand", "crouch", some others for pain poses.
// I'm putting functions to do the basic animations to change these variables in 
// zombie_SetPoseMovement.gsc, 
//
// Error Reporting
// ---------------
// To report a script error condition (similar to assert(0)), I assign a non-existent variable to 
// the variable homemade_error  I use the name of the non-existent variable to try to explain the 
// error.  For example:
// 		homemade_error = Unexpected_anim_pose_value + self.a.pose;
// I also have a kind of assert, called as follows:
//		[[anim.assert(condition, message_string);
// If condition evaluates to 0, the assert fires, prints message_string and stops the server. Since 
// I don't have stack traces of any kind, the message string needs to say from where the assert was 
// called.

// #include maps\mp\animscripts\zm_utility;
// #include maps\_utility;
// #include animscripts\Combat_utility;
// #include common_scripts\Utility;
// 

#include common_scripts\utility;
#include maps\mp\animscripts\shared;
#include maps\mp\animscripts\utility;
#include maps\mp\animscripts\zm_utility;



main()
{
	self.a = SpawnStruct();
	
	self.team = level.zombie_team;

		
	firstInit();

	// Set initial states for poses
	self.a.pose = "stand";
	self.a.movement = "stop";
	self.a.state = "stop";
	self.a.special = "none";

	self.a.combatEndTime = GetTime();
	self.a.script = "init";
	self.a.alertness = "casual"; // casual, alert, aiming
	self.a.lastEnemyTime = GetTime();
	self.a.forced_cover = "none";
	self.a.desired_script = "none";
	self.a.current_script = "none";
	self.a.lookangle = 0;
	self.a.painTime = 0;
	self.a.nextGrenadeTryTime = 0;
	
	// setup the speed variables
	self.walk			= false;
	self.sprint			= false;
	
	// WW (11/16/2010): Setting variable for run blend speed. This allows the black hole bomb to snap guys 180 with less pop.
	self.a.runBlendTime = 0.2;
	
	// MikeD (9/28/2007): Flame pain time... Tracks when the AI gets hit with flame.
	self.a.flamepainTime = 0;
	
	self.a.postScriptFunc = undefined;
	self.a.stance = "stand";
	
	self._animActive = 0;
	
	self thread deathNotify();

	// use the GDT settings to start with
	self.baseAccuracy = self.accuracy;

	// set default accuracy mod
	if( !IsDefined(self.script_accuracy) )
	{
		self.script_accuracy = 1;
	}
	
	self.a.missTime = 0;
	
	self.a.yawTransition = "none";
	self.a.nodeath = false;
	self.a.missTime = 0;
	self.a.missTimeDebounce = 0;
	self.a.disablePain = false;

	self.accuracyStationaryMod = 1;
	self.chatInitialized = false;
	self.sightPosTime = 0;
	self.sightPosLeft = true;
	self.preCombatRunEnabled = true;
	self.is_zombie = true;

	self.a.crouchpain = false; // for dying pain guys
	self.a.nextStandingHitDying = false;

	// Makes AI able to throw grenades at other AI.
	if (!IsDefined (self.script_forcegrenade))
	{
		self.script_forcegrenade = 0;
	}

/#
	self.a.lastDebugPrint = "";
#/

	// state tracking
	self.lastEnemySightTime = 0; // last time we saw our current enemy
	self.combatTime = 0; // how long we've been in/out of combat
	
	// Random range makes the grenades less accurate and do less damage, but also makes it difficult to throw back.
// 	if ( self.team == "allies" )
// 	{
// 		self.randomGrenadeRange = 0;
// 	}
// 	else
// 	{
// 		self.randomGrenadeRange = 128;
// 	}

    self.coverIdleSelectTime = -696969;

	self.old = SpawnStruct();
	
	self.reacquire_state = 0;

	self.a.allow_shooting					= false;
}

DoNothing()
{
}

empty(one, two, three, whatever)
{
}

clearEnemy()
{
	self notify ("stop waiting for enemy to die");
	self endon ("stop waiting for enemy to die");
	self.sightEnemy waittill ("death");
	self.sightpos = undefined;
	self.sightTime = 0;
	self.sightEnemy = undefined;
}

// Cleans up scripts on death
deathNotify()
{
	self waittill( "death", other );
	self notify( anim.scriptChange );
}

firstInit()
{
	// Initialization that should happen once per level
	if ( IsDefined (anim.NotFirstTime) ) // Use this to trigger the first init
	{
		return;
	}
	
	anim.NotFirstTime = true;
	
	anim.useFacialAnims = false; // remove me when facial anims are fixed

	if ( !IsDefined( anim.dog_health ) )
	{
		anim.dog_health = 1;
	}
		
	if ( !IsDefined( anim.dog_presstime ) )
	{
		anim.dog_presstime = 350;
	}
		
	if ( !IsDefined( anim.dog_hits_before_kill ) )
	{
		anim.dog_hits_before_kill = 1;
	}

	level.nextGrenadeDrop = RandomInt(3);
	level.lastPlayerSighted = 100;
	anim.defaultException = maps\mp\animscripts\zm_init::empty;
	
	SetDvar( "scr_expDeathMayMoveCheck", "on" );
	
	// Global constants
	anim.lastSideStepAnim = 0;
	anim.meleeRange = 64;
	anim.meleeRangeSq = anim.meleeRange * anim.meleeRange;
	anim.standRangeSq = 512*512;
	anim.chargeRangeSq = 200*200;
	anim.chargeLongRangeSq = 512*512;
	anim.aiVsAiMeleeRangeSq = 400*400;

	anim.combatMemoryTimeConst = 10000;
	anim.combatMemoryTimeRand = 6000;
	anim.scriptChange = "script_change";

	// MikeD (10/23/2007 10:38:58): Gib Support
	anim.lastGibTime 	= 0;
	anim.gibDelay 		= 3 * 1000; // 3 seconds
	anim.minGibs		= 2;
	anim.maxGibs		= 4;
	anim.totalGibs		= RandomIntRange( anim.minGibs, anim.maxGibs );
	
	anim.corner_straight_yaw_limit = 36;

	if (!IsDefined(anim.optionalStepEffectFunction))
	{
		anim.optionalStepEffects = [];
		anim.optionalStepEffectFunction = ::empty;
	}

	// string based array for notetracks
	anim.notetracks = [];
	maps\mp\animscripts\zm_shared::registerNoteTracks();
	
	if ( !IsDefined( level.flag ) )
	{
		level.flag = [];
		level.flags_lock = [];
	}

	level.painAI = undefined;
	
	
	anim.maymoveCheckEnabled = true; // corner_axis doesnt do the check if this is false, for credits

	anim.badPlaces = []; // queue for animscript badplaces
	anim.badPlaceInt = 0; // assigns unique names to animscript badplaces since we cant save a badplace as an entity

	anim.coverCrouchLeanPitch = -55;
	
	anim.lastCarExplosionTime = -100000;
}

onPlayerConnect()
{
	player = self;
	
	// make sure the init has been called
	firstInit();

	player.invul = false;
}

