/*
 * shavit's Timer - .inc file
 * by: shavit
 *
 * This file is part of shavit's Timer.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
*/

#if defined _shavit_included
	#endinput
#endif
#define _shavit_included

#define SHAVIT_VERSION "3.0.0"
#define STYLE_LIMIT 256
#define MAX_ZONES 64
#define MAX_STAGES 51 // 😐 kind of arbitrary but also some space between this and MAX_ZONES

// HUD
#define HUD_NONE				0
#define HUD_MASTER				(1 << 0) // master setting
#define HUD_CENTER				(1 << 1) // show hud as hint text
#define HUD_ZONEHUD				(1 << 2) // show start/end zone hud
#define HUD_OBSERVE				(1 << 3) // show the HUD of the player you spectate
#define HUD_SPECTATORS			(1 << 4) // show list of spectators
#define HUD_KEYOVERLAY			(1 << 5) // show a key overlay
#define HUD_HIDEWEAPON			(1 << 6) // hide the player's weapon
#define HUD_TOPLEFT				(1 << 7) // show top left white HUD with WR/PB times
#define HUD_SYNC				(1 << 8) // shows sync at right side of the screen (css only)
#define HUD_TIMELEFT			(1 << 9) // shows time left at right tside of the screen (css only)
#define HUD_2DVEL				(1 << 10) // shows 2d velocity
#define HUD_NOSOUNDS			(1 << 11) // disables sounds on personal best, world record etc
#define HUD_NOPRACALERT			(1 << 12) // hides practice mode chat alert

// status
enum TimerStatus
{
	Timer_Stopped,
	Timer_Running,
	Timer_Paused
};

enum //ReplayStatus
{
	Replay_Start,
	Replay_Running,
	Replay_End,
	Replay_Idle
};

enum //ReplayBotType
{
	Replay_Central,
	Replay_Looping, // these are the ones that loop styles, tracks, and (eventually) stages...
	Replay_Dynamic, // these are bots that spawn on !replay when the central bot is taken
	Replay_Prop,    // A prop entity that is being used as a replay...
};

enum
{
	CPR_ByConVar = (1 << 0),
	CPR_NoTimer = (1 << 1),
	CPR_InStartZone = (1 << 2),
	CPR_NotOnGround = (1 << 3),
	CPR_Moving = (1 << 4),
	CPR_Duck = (1 << 5), // quack
	CPR_InEndZone = (1 << 6),
};

enum
{
	Migration_RemoveWorkshopMaptiers,
	Migration_RemoveWorkshopMapzones,
	Migration_RemoveWorkshopPlayertimes,
	Migration_LastLoginIndex,
	Migration_RemoveCountry,
	Migration_ConvertIPAddresses,
	Migration_ConvertSteamIDsUsers,
	Migration_ConvertSteamIDsPlayertimes,
	Migration_ConvertSteamIDsChat,
	Migration_PlayertimesDateToInt,
	Migration_AddZonesFlagsAndData,
	Migration_AddPlayertimesCompletions,
	Migration_AddCustomChatAccess,
	Migration_AddPlayertimesExactTimeInt,
	Migration_FixOldCompletionCounts, // old completions accidentally started at 2
	MIGRATIONS_END
};

enum
{
	Zone_Start,
	Zone_End,
	Zone_Respawn,
	Zone_Stop,
	Zone_Slay,
	Zone_Freestyle,
	Zone_CustomSpeedLimit,
	Zone_Teleport,
	Zone_CustomSpawn,
	Zone_Easybhop,
	Zone_Slide,
	Zone_Airaccelerate,
	Zone_Stage,
	ZONETYPES_SIZE
};

enum
{
	Track_Main,
	Track_Bonus,
	Track_Bonus_Last = 8,
	TRACKS_SIZE
};

// for Shavit_GetStyleStrings
enum
{
	sStyleName,
	sShortName,
	sHTMLColor,
	sChangeCommand,
	sClanTag,
	sSpecialString,
	sStylePermission
};

// for Shavit_GetChatStrings
enum
{
	sMessagePrefix,
	sMessageText,
	sMessageWarning,
	sMessageVariable,
	sMessageVariable2,
	sMessageStyle
};

enum struct stylestrings_t
{
	char sStyleName[64];
	char sShortName[32];
	char sHTMLColor[32];
	char sChangeCommand[128];
	char sClanTag[32];
	char sSpecialString[128];
	char sStylePermission[64];
}

enum struct chatstrings_t
{
	char sPrefix[64];
	char sText[16];
	char sWarning[16];
	char sVariable[16];
	char sVariable2[16];
	char sStyle[16];
}

enum struct timer_snapshot_t
{
	bool bTimerEnabled;
	float fCurrentTime;
	bool bClientPaused;
	int iJumps;
	int bsStyle;
	int iStrafes;
	int iTotalMeasures;
	int iGoodGains;
	float fServerTime;
	int iSHSWCombination;
	int iTimerTrack;
	int iMeasuredJumps;
	int iPerfectJumps;
	float fTimeOffset[2];
	float fDistanceOffset[2];
	float fAvgVelocity;
	float fMaxVelocity;
	float fTimescale;
}

enum struct cp_cache_t
{
	float fPosition[3];
	float fAngles[3];
	float fVelocity[3];
	MoveType iMoveType;
	float fGravity;
	float fSpeed;
	float fStamina;
	bool bDucked;
	bool bDucking;
	float fDucktime; // m_flDuckAmount in csgo
	float fDuckSpeed; // m_flDuckSpeed in csgo; doesn't exist in css
	int iFlags;
	timer_snapshot_t aSnapshot;
	char sTargetname[64];
	char sClassname[64];
	ArrayList aFrames;
	int iPreFrames;
	bool bSegmented;
	bool bPractice;
	int iGroundEntity;
	int iSteamID;
	ArrayList aEvents;
	ArrayList aOutputWaits;
	float vecLadderNormal[3];
}

enum struct frame_t
{
	float pos[3];
	float ang[2];
	int buttons;
	// iReplayVersion >= 0x02
	int flags;
	MoveType mt;
	// Everything below is generally NOT loaded into memory for playback
	// iReplayVersion >= 0x06
	int mousexy; // `mousex | (mousey << 16)` // unpack with UnpackSignedShorts
	int vel; // basically `forwardmove | (sidemove << 16)` // unpack with UnpackSignedShorts
}

enum struct frame_cache_t
{
	int iFrameCount;
	float fTime;
	bool bNewFormat;
	int iReplayVersion;
	char sReplayName[MAX_NAME_LENGTH];
	int iPreFrames;
	ArrayList aFrames;
	// iReplayVersion >= 0x05
	int iPostFrames;
	float fTickrate;
}

#if defined USES_CHAT_COLORS
// hardcoded colors
char gS_GlobalColorNames[][] =
{
	"{default}",
	"{team}",
	"{green}"
};

char gS_GlobalColors[][] =
{
	"\x01",
	"\x03",
	"\x04"
};

char gS_CSGOColorNames[][] =
{
	"{blue}",
	"{bluegrey}",
	"{darkblue}",
	"{darkred}",
	"{gold}",
	"{grey}",
	"{grey2}",
	"{lightgreen}",
	"{lightred}",
	"{lime}",
	"{orchid}",
	"{yellow}",
	"{palered}"
};

char gS_CSGOColors[][] =
{
	"\x0B",
	"\x0A",
	"\x0C",
	"\x02",
	"\x10",
	"\x08",
	"\x0D",
	"\x05",
	"\x0F",
	"\x06",
	"\x0E",
	"\x09",
	"\x07"
};
#endif

// connects synchronously to the bhoptimer database
// calls errors if needed
stock Database GetTimerDatabaseHandle()
{
	Database db = null;
	char sError[255];

	if(SQL_CheckConfig("shavit"))
	{
		if((db = SQL_Connect("shavit", true, sError, 255)) == null)
		{
			SetFailState("Timer startup failed. Reason: %s", sError);
		}
	}
	else
	{
		db = SQLite_UseDatabase("shavit", sError, 255);
	}

	return db;
}

// figures out if the database is a mysql database
stock bool IsMySQLDatabase(Database db)
{
	char sDriver[8];
	db.Driver.GetIdentifier(sDriver, 8);

	return StrEqual(sDriver, "mysql", false);
}

// retrieves the table prefix defined in configs/shavit-prefix.txt
stock void GetTimerSQLPrefix(char[] buffer, int maxlen)
{
	char sFile[PLATFORM_MAX_PATH];
	BuildPath(Path_SM, sFile, PLATFORM_MAX_PATH, "configs/shavit-prefix.txt");

	File fFile = OpenFile(sFile, "r");

	if(fFile == null)
	{
		SetFailState("Cannot open \"configs/shavit-prefix.txt\". Make sure this file exists and that the server has read permissions to it.");
	}

	char sLine[PLATFORM_MAX_PATH * 2];

	if(fFile.ReadLine(sLine, PLATFORM_MAX_PATH * 2))
	{
		TrimString(sLine);
		strcopy(buffer, maxlen, sLine);
	}

	delete fFile;
}

stock bool IsValidClient(int client, bool bAlive = false)
{
	return (client >= 1 && client <= MaxClients && IsClientConnected(client) && IsClientInGame(client) && !IsClientSourceTV(client) && (!bAlive || IsPlayerAlive(client)));
}

stock bool IsSource2013(EngineVersion ev)
{
	return (ev == Engine_CSS || ev == Engine_TF2);
}

stock void IPAddressToString(int ip, char[] buffer, int maxlen)
{
	FormatEx(buffer, maxlen, "%d.%d.%d.%d", ((ip >> 24) & 0xFF), ((ip >> 16) & 0xFF), ((ip >> 8) & 0xFF), (ip & 0xFF));
}

stock int IPStringToAddress(const char[] ip)
{
	char sExplodedAddress[4][4];
	ExplodeString(ip, ".", sExplodedAddress, 4, 4, false);

	int iIPAddress =
			(StringToInt(sExplodedAddress[0]) << 24) |
			(StringToInt(sExplodedAddress[1]) << 16) |
			(StringToInt(sExplodedAddress[2]) << 8) |
			StringToInt(sExplodedAddress[3]);

	return iIPAddress;
}

// Retrieves authid from STEAM_X:Y:Z and [U:1:123]
stock int SteamIDToAuth(const char[] sInput)
{
	char sSteamID[32];
	strcopy(sSteamID, sizeof(sSteamID), sInput);
	ReplaceString(sSteamID, 32, "\"", "");

	if (StrContains(sSteamID, "STEAM_") != -1)
	{
		ReplaceString(sSteamID, 32, "STEAM_", "");

		char parts[3][11];
		ExplodeString(sSteamID, ":", parts, 3, 11);

		// Let X, Y and Z constants be defined by the SteamID: STEAM_X:Y:Z.
		// Using the formula W=Z*2+Y, a SteamID can be converted:
		return StringToInt(parts[2]) * 2 + StringToInt(parts[1]);
	}
	else if (StrContains(sSteamID, "U:1:") != -1)
	{
		ReplaceString(sSteamID, 32, "[", "");
		ReplaceString(sSteamID, 32, "U:1:", "");
		ReplaceString(sSteamID, 32, "]", "");

		return StringToInt(sSteamID);
	}

	return StringToInt(sSteamID);
}

// Can be used to unpack frame_t.mousexy and frame_t.vel
stock void UnpackSignedShorts(int x, int out[2])
{
	out[0] =  ((x        & 0xFFFF) ^ 0x8000) - 0x8000;
	out[1] = (((x >> 16) & 0xFFFF) ^ 0x8000) - 0x8000;
}

// time formatting!
stock void FormatSeconds(float time, char[] newtime, int newtimesize, bool precise = true, bool nodecimal = false)
{
	float fTempTime = time;

	if(fTempTime < 0.0)
	{
		fTempTime = -fTempTime;
	}

	int iRounded = RoundToFloor(fTempTime);
	int iSeconds = (iRounded % 60);
	float fSeconds = iSeconds + fTempTime - iRounded;

	char sSeconds[8];

	if (nodecimal)
	{
		FormatEx(sSeconds, 8, "%d", iSeconds);
	}
	else
	{
		FormatEx(sSeconds, 8, precise? "%.3f":"%.1f", fSeconds);
	}

	if(fTempTime < 60.0)
	{
		strcopy(newtime, newtimesize, sSeconds);
		FormatEx(newtime, newtimesize, "%s%s", (time < 0.0) ? "-":"", sSeconds);
	}
	else
	{
		int iMinutes = (iRounded / 60);

		if(fTempTime < 3600.0)
		{
			FormatEx(newtime, newtimesize, "%s%d:%s%s", (time < 0.0)? "-":"", iMinutes, (fSeconds < 10)? "0":"", sSeconds);
		}
		else
		{
			int iHours = (iMinutes / 60);
			iMinutes %= 60;

			FormatEx(newtime, newtimesize, "%s%d:%s%d:%s%s", (time < 0.0)? "-":"", iHours, (iMinutes < 10)? "0":"", iMinutes, (fSeconds < 10)? "0":"", sSeconds);
		}
	}
}

stock bool GuessBestMapName(ArrayList maps, const char[] input, char[] output, int size)
{
	if(maps.FindString(input) != -1)
	{
		strcopy(output, size, input);

		return true;
	}

	char sCache[128];

	for(int i = 0; i < maps.Length; i++)
	{
		maps.GetString(i, sCache, 128);

		if(StrContains(sCache, input) != -1)
		{
			strcopy(output, size, sCache);

			return true;
		}
	}

	return false;
}

stock void GetTrackName(int client, int track, char[] output, int size)
{
	if (track == Track_Main)
	{
		FormatEx(output, size, "%T", "Track_Main", client);
	}
	else if (track >= Track_Bonus)
	{
		FormatEx(output, size, "%T", "Track_Bonus", client, track);
	}
	else //if (track < Track_Main)
	{
		FormatEx(output, size, "%T", "Track_Unknown", client);
	}
}

stock int GetSpectatorTarget(int client, int fallback = -1)
{
	int target = fallback;

	if(IsClientObserver(client))
	{
		int iObserverMode = GetEntProp(client, Prop_Send, "m_iObserverMode");

		if(iObserverMode >= 3 && iObserverMode <= 5)
		{
			int iTarget = GetEntPropEnt(client, Prop_Send, "m_hObserverTarget");

			if (IsValidEntity(iTarget))
			{
				target = iTarget;
			}
		}
	}

	return target;
}

stock float GetAngleDiff(float current, float previous)
{
	float diff = current - previous;
	return diff - 360.0 * RoundToFloor((diff + 180.0) / 360.0);
}

forward Action Shavit_OnUserCmdPre(int client, int &buttons, int &impulse, float vel[3], float angles[3], TimerStatus status, int track, int style, int mouse[2]);

forward void Shavit_OnTimeIncrement(int client, timer_snapshot_t snapshot, float &time);

forward void Shavit_OnTimeIncrementPost(int client, float time);

forward Action Shavit_OnStartPre(int client, int track);

/**
 * Called when a player's timer starts.
 * (WARNING: Will be called every tick when the player stands at the start zone!)
 *
 * @param client                    Client index.
 * @param track                     Timer track.
 * @return                          Unused.
 */
forward Action Shavit_OnStart(int client, int track);

/**
 * Called when a player uses the restart command.
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
forward void Shavit_OnRestart(int client, int track);

/**
 * Called when a player uses the !end command.
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
forward void Shavit_OnEnd(int client, int track);

/**
 * Called before a player's timer is stopped. (stop =/= finish a map)
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @return							False to prevent the timer from stopping.
 */
forward bool Shavit_OnStopPre(int client, int track);

/**
 * Called when a player's timer stops. (stop =/= finish a map)
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
forward void Shavit_OnStop(int client, int track);

/**
 * Called before a player finishes a map.
 *
 * @param client					Client index.
 * @param snapshot					A snapshot of the player's timer.
 * @return							Plugin_Continue to do nothing, Plugin_Changed to change the variables or anything else to stop the timer from finishing.
 */
forward Action Shavit_OnFinishPre(int client, timer_snapshot_t snapshot);

/**
 * Called when a player finishes a map. (touches the end zone)
 *
 * @param client					Client index.
 * @param style						Style the record was done on.
 * @param time						Record time.
 * @param jumps						Jumps amount.
 * @param strafes					Amount of strafes.
 * @param sync						Sync percentage (0.0 to 100.0) or -1.0 when not measured.
 * @param track						Timer track.
 * @param oldtime					The player's best time on the map before this finish.
 * @param perfs						Perfect jump percentage (0.0 to 100.0) or 100.0 when not measured.
 * @param avgvel					Player's average velocity throughout the run.
 * @param maxvel					Player's highest reached velocity.
 * @param timestamp					System time of when player finished.
 * @noreturn
 */
forward void Shavit_OnFinish(int client, int style, float time, int jumps, int strafes, float sync, int track, float oldtime, float perfs, float avgvel, float maxvel, int timestamp);

/**
 * Like Shavit_OnFinish, but after the insertion query was called.
 * Called from shavit-wr
 *
 * @param client					Client index.
 * @param style						Style the record was done on.
 * @param time						Record time.
 * @param jumps						Jumps amount.
 * @param strafes					Amount of strafes.
 * @param sync						Sync percentage (0.0 to 100.0) or -1.0 when not measured.
 * @param rank						Rank on map.
 * @param overwrite					1 - brand new record. 2 - update.
 * @param track						Timer track.
 * @param oldtime					The player's best time on the map before this finish.
 * @param perfs						Perfect jump percentage (0.0 to 100.0) or 100.0 when not measured.
 * @param avgvel					Player's average velocity throughout the run.
 * @param maxvel					Player's highest reached velocity.
 * @param timestamp					System time of when player finished.
 * @noreturn
 */
forward void Shavit_OnFinish_Post(int client, int style, float time, int jumps, int strafes, float sync, int rank, int overwrite, int track, float oldtime, float perfs, float avgvel, float maxvel, int timestamp);

/**
 * Called when there's a new WR on the map.
 *
 * @param client					Client index.
 * @param style						Style the record was done on.
 * @param time						Record time.
 * @param jumps						Jumps amount.
 * @param strafes					Amount of strafes.
 * @param sync						Sync percentage (0.0 to 100.0) or -1.0 when not measured.
 * @param track						Timer track.
 * @param oldwr						Time of the old WR. 0.0 if there's none.
 * @param oldtime					The player's best time on the map before this finish.
 * @param perfs						Perfect jump percentage (0.0 to 100.0) or 100.0 when not measured.
 * @param avgvel					Player's average velocity throughout the run.
 * @param maxvel					Player's highest reached velocity.
 * @param timestamp					System time of when player finished.
 * @noreturn
 */
forward void Shavit_OnWorldRecord(int client, int style, float time, int jumps, int strafes, float sync, int track, float oldwr, float oldtime, float perfs, float avgvel, float maxvel, int timestamp);

/**
 * Called when an admin deletes a WR.
 *
 * @param style						Style the record was done on.
 * @param id						Record ID. -1 if mass deletion.
 * @param track						Timer track.
 * @param accountid					The account ID of the wr holder
 * @param mapname					The map name.
 * @noreturn
 */
forward void Shavit_OnWRDeleted(int style, int id, int track, int accountid, const char[] mapname);

/**
 * Called when a player's timer paused.
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
forward void Shavit_OnPause(int client, int track);

/**
 * Called when a player's timer resumed.
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
forward void Shavit_OnResume(int client, int track);

/**
 * Called when a player changes their bhopstyle.
 * Note: Doesn't guarantee that the player is in-game or connected.
 *
 * @param client					Client index.
 * @param oldstyle					Old bhop style.
 * @param newstyle					New bhop style.
 * @param track						Timer track.
 * @param manual					Was the change manual, or assigned automatically?
 * @noreturn
 */
forward void Shavit_OnStyleChanged(int client, int oldstyle, int newstyle, int track, bool manual);

/**
 * Called when a player changes their bhop track.
 *
 * @param client					Client index.
 * @param oldtrack					Old bhop track.
 * @param newtrack					New bhop track.
 * @noreturn
 */
forward void Shavit_OnTrackChanged(int client, int oldtrack, int newtrack);

/**
 * Called when the styles configuration finishes loading and it's ready to load everything into the cache.
 *
 * @param styles					Amount of styles loaded.
 * @noreturn
 */
forward void Shavit_OnStyleConfigLoaded(int styles);

/**
 * Called when there's a successful connection to the database and it is ready to be used.
 * Called through shavit-core after migrations have been applied, and after the attempt to create the default `users` table.
 *
 * @noreturn
 */
forward void Shavit_OnDatabaseLoaded();

/**
 * Called when the chat messages configuration finishes loading and it's ready to load everything into the cache.
 *
 * @noreturn
 */
forward void Shavit_OnChatConfigLoaded();

/**
 * Called when a player teleports with checkpoints.
 *
 * @param client					Client index.
 * @param index						Checkpoint that was teleported to.
 * @return							Plugin_Continue to allow teleporting, anything else to prevent.
 */
forward Action Shavit_OnTeleport(int client, int index);

/**
 * Called when a saves a checkpoint.
 *
 * @param client					Client index.
 * @param index						Checkpoint that was saved to.
 * @param overflow					Does this checkpoint shift the rest.
 * @return							Plugin_Continue to allow saving, anything else to prevent.
 */
forward Action Shavit_OnSave(int client, int index, bool overflow);

/**
 * Called when a player deletes a checkpoint.
 *
 * @param client					Client index.
 * @param index						Checkpoint that will be deleted.
 * @return							Plugin_Continue to continue deletion, anything else to prevent.
 */
forward Action Shavit_OnDelete(int client, int index);

/**
 * Called when a player enters a zone.
 *
 * @param client					Client index.
 * @param type						Zone type.
 * @param track						Zone track.
 * @param id						Zone ID.
 * @param entity					Zone trigger entity index.
 * @param data						Zone data if any.
 * @noreturn
 */
forward void Shavit_OnEnterZone(int client, int type, int track, int id, int entity, int data);

/**
 * Called when a player leaves a zone.
 *
 * @param client					Client index.
 * @param type						Zone type.
 * @param track						Zone track.
 * @param id						Zone ID.
 * @param entity					Zone trigger entity index.
 * @param data						Zone data if any.
 * @noreturn
 */
forward void Shavit_OnLeaveZone(int client, int type, int track, int id, int entity, int data);

/**
 * Called when a player leaves a zone.
 *
 * @param client					Client index.
 * @param stageNumber				Stage number.
 * @param message					The stage time message that will be printed.
 * @param maxlen					The buffer size of message.
 * @return							Plugin_Handled to block the timer from printing msg to the client. Plugin_Continue to let the timer print msg.
 */
forward Action Shavit_OnStageMessage(int client, int stageNumber, char[] message, int maxlen);

/**
 * Called when a player gets the worst record in the server for the style.
 * Note: Will be only called for ranked styles.
 *
 * @param client					Client index.
 * @param style						Style the record was done on.
 * @param time						Record time.
 * @param jumps						Jumps amount.
 * @param strafes					Amount of strafes.
 * @param sync						Sync percentage (0.0 to 100.0) or -1.0 when not measured.
 * @param track						Timer track.
 * @param oldtime					The player's best time on the map before this finish.
 * @param perfs						Perfect jump percentage (0.0 to 100.0) or 100.0 when not measured.
 * @param avgvel					Player's average velocity throughout the run.
 * @param maxvel					Player's highest reached velocity.
 * @param timestamp					System time of when player finished.
 * @noreturn
 */
forward void Shavit_OnWorstRecord(int client, int style, float time, int jumps, int strafes, float sync, int track, float oldtime, float perfs, float avgvel, float maxvel, int timestamp);

/**
 * Gets called when a map's tier is assigned.
 * Only called once per map, if the rankings plugin is enabled.
 * The exception is if the admin changes the current map's tier.
 *
 * @param map						Map display name.
 * @param tier						Map's tier.
 * @noreturn
 */
forward void Shavit_OnTierAssigned(const char[] map, int tier);

/**
 * Gets called when the server acknowledges the client's ranking status.
 * It is called after OnClientPostAdminCheck and at forced rank recalculations.
 *
 * @param client					Client index.
 * @param rank						Client's rank. (0 if unranked or unassigned)
 * @param points					Client's points. (0.0 if unranked or unassigned)
 * @param first						True if the forward is called after the initial connection, false if it is caused by recalculation.
 * @noreturn
 */
forward void Shavit_OnRankAssigned(int client, int rank, float points, bool first);

/**
 * Called when replay playback starts.
 *
 * @param ent						Entity index for the replay.
 * @param type						The type of replay. Replay_Prop means `ent` is not a fakeclient, but instead a prop.
 * @noreturn
 */
forward void Shavit_OnReplayStart(int ent, int type);

/**
 * Called when replay playback ends.
 *
 * @param client					Entity index for the replay.
 * @param type						The type of replay. Replay_Prop means `ent` is not a fakeclient, but instead a prop.
 * @noreturn
 */
forward void Shavit_OnReplayEnd(int ent, int type);

/**
 * Called when all replays files have been loaded.
 *
 * @noreturn
 */
forward void Shavit_OnReplaysLoaded();

/**
 * Called when a player finishes a time. Allows you to save a replay even if the run is not a WR.
 *
 * @param client					Client index.
 * @param style						Style the record was done on.
 * @param time						Record time.
 * @param jumps						Jumps amount.
 * @param strafes					Amount of strafes.
 * @param sync						Sync percentage (0.0 to 100.0) or -1.0 when not measured.
 * @param track						Timer track.
 * @param oldtime					The player's best time on the map before this finish.
 * @param perfs						Perfect jump percentage (0.0 to 100.0) or 100.0 when not measured.
 * @param avgvel					Player's average velocity throughout the run.
 * @param maxvel					Player's highest reached velocity.
 * @param timestamp					System time of when player finished.
 * @param isbestreplay				If the time is the new replay.
 * @param istoolong					If the time is too long to save a replay if the time is a WR. Note: replays WON'T be full length if this is true.
 * @return							Return Plugin_Changed (or higher) to cause a copy of the replay to be saved. Return Plugin_Continue otherwise.
 */
forward Action Shavit_ShouldSaveReplayCopy(int client, int style, float time, int jumps, int strafes, float sync, int track, float oldtime, float perfs, float avgvel, float maxvel, int timestamp, bool isbestreplay, bool istoolong);

/**
 * Called when either a WR replay or a copy of a replay has been saved.
 *
 * @param client					Client index.
 * @param style						Style the record was done on.
 * @param time						Record time.
 * @param jumps						Jumps amount.
 * @param strafes					Amount of strafes.
 * @param sync						Sync percentage (0.0 to 100.0) or -1.0 when not measured.
 * @param track						Timer track.
 * @param oldtime					The player's best time on the map before this finish.
 * @param perfs						Perfect jump percentage (0.0 to 100.0) or 100.0 when not measured.
 * @param avgvel					Player's average velocity throughout the run.
 * @param maxvel					Player's highest reached velocity.
 * @param timestamp					System time of when player finished.
 * @param isbestreplay				If the time is the new replay.
 * @param istoolong					If the time is too long to save a replay if the time is a WR. Note: replays WON'T be full length if this is true.
 * @param iscopy					If the path points to a copy of the replay.
 * @param replaypath				Path to the saved replay.
 * @noreturn
 */
forward void Shavit_OnReplaySaved(int client, int style, float time, int jumps, int strafes, float sync, int track, float oldtime, float perfs, float avgvel, float maxvel, int timestamp, bool isbestreplay, bool istoolong, bool iscopy, const char[] replaypath);

/**
 * Called when top left HUD updates.
 *
 * @param client					Client index that recieves the hud.
 * @param target					Target entity that is either the client or what the client is spectating.
 * @param topleft					Reference to the HUD buffer.
 * @param topleftlength				Max length of the topleft buffer.
 * @return							Plugin_Handled or Plugin_Stop to block the HUD message from appearing. Anything else to pass along new values.
 */
forward Action Shavit_OnTopLeftHUD(int client, int target, char[] topleft, int topleftlength);

/**
 * Called before clan tag variables are processed.
 *
 * @param client					Client index.
 * @param clantag					Reference to the clan tag buffer.
 * @param clantaglength				Max length of the customtag buffer.
 * @return							Plugin_Handled or Plugin_Stop to block the clan tag from changing. Anything else to pass along new values.
 */
forward Action Shavit_OnClanTagChangePre(int client, char[] clantag, int clantaglength);

/**
 * Called after clan tags are changed.
 *
 * @param client					Client index.
 * @param customtag					Reference to the custom clan tag buffer.
 * @param customtaglength			Max length of the customtag buffer.
 * @noreturn
 */
forward void Shavit_OnClanTagChangePost(int client, char[] customtag, int customtaglength);

/**
 * Called when a time offset is calculated
 *
 * @param client					Client index.
 * @param zonetype					Zone type (Zone_Start or Zone_End).
 * @param offset					Time offset from the given zone.
 * @param distance					Distance used in time offset.
 * @noreturn
 */
forward void Shavit_OnTimeOffsetCalculated(int client, int zonetype, float offset, float distance);

/**
 * Called before the timer finish message is printed to the users.
 *
 * @param client					Client index.
 * @param everyone					Is the message printed to everyone, or just the client?
 * @param snapshot					A snapshot of the client's timer when printing the message.
 * @param overwrite					Modify the database? 0 - no. 1 - brand new record. 2 - new personal best.
 * @param rank						Rank on map.
 * @param message					The finish message.
 * @param maxlen					Buffer size of message.
 * @param message2					A second line of info that is printed on finish.
 * @param maxlen2					Buffer size of message2.
 *
 * @return							Plugin_Handled or Plugin_Stop to stop the message. Anything else to use new values.
 */
forward Action Shavit_OnFinishMessage(int client, bool &everyone, timer_snapshot_t snapshot, int overwrite, int rank, char[] message, int maxlen, char[] message2, int maxlen2);

/**
 * Called when a clients dynamic timescale has been changed.
 *
 * @param client					Client index.
 * @param oldtimescale				The old timescale value
 * @param newtimescale				The new timescale value
 * @noreturn
 */
forward void Shavit_OnTimescaleChanged(int client, float oldtimescale, float newtimescale);

/**
 * Called after the checkpoint menu has been made and before it's sent to the client.
 *
 * @param client					Client index.
 * @param segmented					If the menu was a segmented menu
 * @return							Plugin_Handled or Plugin_Stop to stop the menu.
 */
forward Action Shavit_OnCheckPointMenuMade(int client, bool segmented);

/**
 * Called before a selection is processed in the main checkpoint menu.
 *
 * @param client					Client index.
 * @param param2					Second parameter in the callback, usually the item selected.
 * @param info						reference copy of the info string used in the callback
 * @param maxlength					length of the info buffer
 * @param currentCheckpoint			Clients current checkpoint
 * @param maxCPs					Max checkpoints the client can use
 * @return							Plugin_Continue to continue the callback.
 */
forward Action Shavit_OnCheckpointMenuSelect(int client, int param2, char[] info, int maxlength, int currentCheckpoint, int maxCPs);

/**
 * Called before a sound is played by shavit-sounds.
 *
 * @param client					Index of the client that triggered the sound event.
 * @param sound						Reference to the sound that will be played.
 * @param maxlength					Length of the sound buffer, always PLATFORM_MAX_PATH.
 * @param clients					Reference to the array of clients to receive the sound, maxsize of MaxClients.
 * @param count						Reference to the number of clients to receive the sound.
 * @return							Plugin_Handled or Plugin_Stop to block the sound from being played. Anything else to continue the operation.
 */
forward Action Shavit_OnPlaySound(int client, char[] sound, int maxlength, int[] clients, int &count);

/**
 * Called before the server & timer handle the ProcessMovement method.
 *
 * @param client					Client Index.
 * @noreturn
 */
forward void Shavit_OnProcessMovement(int client);

/**
 * Called After the server handles the ProcessMovement method, but before the timer handles the method.
 *
 * @param client					Client Index.
 * @noreturn
 */
forward void Shavit_OnProcessMovementPost(int client);

/**
 * Returns bhoptimer's database handle.
 * Call within Shavit_OnDatabaseLoaded. Safety is not guaranteed anywhere else!
 *
 * @return							Database handle.
 */
native Database Shavit_GetDatabase();

/**
 * Starts the timer for a player.
 * Will not teleport the player to anywhere, it's handled inside the mapzones plugin.
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
native void Shavit_StartTimer(int client, int track);

/**
 * Sets the player's current location as their spawn location for the specified track.
 *
 * @param client					Client index.
 * @param track 					Timer track.
 * @param anglesonly 				Whether to save angles only.
 * @noreturn
 */
native void Shavit_SetStart(int client, int track, bool anglesonly);

/**
 * Deletes the player's current set start position for the specified track.
 *
 * @param client					Client index.
 * @param track 					Timer track.
 * @noreturn
 */
native void Shavit_DeleteSetStart(int client, int track);

/**
 * Restarts the timer for a player.
 * Will work as if the player just used sm_r.
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
native void Shavit_RestartTimer(int client, int track);

/**
 * Stops the timer for a player.
 * Will not teleport the player to anywhere, it's handled inside the mapzones plugin.
 *
 * @param client					Client index.
 * @param bypass					Bypass call to Shavit_OnStopPre?
 * @return							True if the operation went through.
 */
native bool Shavit_StopTimer(int client, bool bypass = true);

/**
 * Deletes all map records for the specified map.
 * Plugin will refresh if map is currently on.
 *
 * @param map						Map name.
 * @noreturn
 */
native void Shavit_WR_DeleteMap(const char[] map);

/**
 * Deletes all map zones for the specified map.
 * Plugin will refresh if map is currently on.
 *
 * @param map						Map name.
 * @noreturn
 */
native void Shavit_Zones_DeleteMap(const char[] map);

/**
 * Deletes all replays for the specified map.
 * Plugin will refresh if map is currently on.
 *
 * @param map						Map name.
 * @noreturn
 */
native void Shavit_Replay_DeleteMap(const char[] map);

/**
 * Deletes tier setting for the specified map.
 * Points recalculation will run right after this is finished.
 *
 * @param map						Map name.
 * @noreturn
 */
native void Shavit_Rankings_DeleteMap(const char[] map);

/**
 * Changes a player's bhop style.
 *
 * @param client					Client index.
 * @param style						Style.
 * @param force						Ignore style permissions. This being true will bypass the `inaccessible` style setting as well.
 * @param manual					Is it a manual style change? (Was it caused by user interaction?)
 * @param noforward					Bypasses the call to `Shavit_OnStyleChanged`.
 * @return							False if failed due to lack of access, true otherwise.
 */
native bool Shavit_ChangeClientStyle(int client, int style, bool force = false, bool manual = false, bool noforward = false);

/**
 * Finishes the map for a player, with their current timer stats.
 * Will not teleport the player to anywhere, it's handled inside the mapzones plugin.
 *
 * @param client					Client index.
 * @param track						Timer track.
 * @noreturn
 */
native void Shavit_FinishMap(int client, int track);

native float Shavit_GetClientTime(int client);

native int Shavit_GetClientTrack(int client);

native int Shavit_GetClientJumps(int client);

native int Shavit_GetBhopStyle(int client);

native TimerStatus Shavit_GetTimerStatus(int client);

native int Shavit_GetStageZone(int stage, int track=Track_Main);

native int Shavit_GetHighestStage(int track);

native int Shavit_GetClientLastStage(int client);

native float Shavit_GetStageWR(int track, int style, int stage);

//native float Shavit_GetStagePB(int client, int track, int style, int stage);

//native float Shavit_GetStageWRCP(int track, int style, int stage);
//native float Shavit_GetStagePBCP(int client, int track, int style, int stage);

native int Shavit_GetStrafeCount(int client);

native float Shavit_GetPerfectJumps(int client);

native float Shavit_GetSync(int client);

native float Shavit_GetWorldRecord(int style, int track);

native void Shavit_ReloadLeaderboards();

native void Shavit_GetWRRecordID(int style, int &recordid, int track);

native void Shavit_GetWRName(int style, char[] wrname, int wrmaxlength, int track);

native float Shavit_GetClientPB(int client, int style, int track);

native void Shavit_SetClientPB(int client, int style, int track, float time);

native int Shavit_GetClientCompletions(int client, int style, int track);

native int Shavit_GetRecordAmount(int style, int track);

native int Shavit_GetRankForTime(int style, float time, int track);

native float Shavit_GetTimeForRank(int style, int rank, int track);

native bool Shavit_ZoneExists(int type, int track);

native bool Shavit_InsideZone(int client, int type, int track);

native int Shavit_GetZoneData(int zoneid);

native int Shavit_GetZoneFlags(int zoneid);

native bool Shavit_InsideZoneGetID(int client, int type, int track, int &zoneid);

native bool Shavit_IsClientCreatingZone(int client);

native void Shavit_PauseTimer(int client);

native void Shavit_ResumeTimer(int client, bool teleport = false);

native float Shavit_GetTimeOffset(int client, int zonetype);

native float Shavit_GetDistanceOffset(int client, int zonetype);

native bool Shavit_DeleteReplay(const char[] map, int style, int track, int accountid = 0);

native float Shavit_GetReplayBotFirstFrameTime(int entity);

native int Shavit_GetReplayBotIndex(int style, int track);

native int Shavit_GetReplayBotStyle(int entity);

native int Shavit_GetReplayBotTrack(int entity);

native int Shavit_GetReplayBotType();

native int Shavit_GetReplayBotCurrentFrame(int entity);

native int Shavit_GetReplayStarter(int ent);

native int Shavit_GetReplayButtons(int ent, float& anglediff);

native int Shavit_GetReplayFrameCount(int style, int track);

native int Shavit_GetReplayPreFrames(int style, int track);

native int Shavit_GetReplayPostFrames(int style, int track);

native int Shavit_GetReplayCacheFrameCount(int bot);

native int Shavit_GetReplayCachePreFrames(int bot);

native int Shavit_GetReplayCachePostFrames(int bot);

native ArrayList Shavit_GetReplayFrames(int style, int track, bool cheapCloneHandle=false);

native int Shavit_GetClientFrameCount(int client);

native float Shavit_GetReplayLength(int style, int track);

native float Shavit_GetReplayCacheLength(int bot);

native float Shavit_GetReplayTime(int entity);

native void Shavit_GetReplayName(int style, int track, char[] buffer, int length);

native void Shavit_HijackAngles(int client, float pitch, float yaw);

native bool Shavit_IsReplayDataLoaded(int style, int track);

native float Shavit_GetPoints(int client);

native int Shavit_GetRank(int client);

native int Shavit_GetRankedPlayers();

native int Shavit_ForceHUDUpdate(int client, bool spectators);

native void Shavit_OpenStatsMenu(int client, int steamid);

native int Shavit_GetWRCount(int client, int track = -1, int style = -1, bool usecvars = true);

native int Shavit_GetWRHolders(int track = -1, int style = -1, bool usecvars = true);

native int Shavit_GetWRHolderRank(int client, int track = -1, int style = -1, bool usecvars = true);

native bool Shavit_GetStyleSetting(int style, const char[] key, char[] value, int maxlength);

native int Shavit_GetStyleSettingInt(int style, const char[] key);

native bool Shavit_GetStyleSettingBool(int style, const char[] key);

native float Shavit_GetStyleSettingFloat(int style, const char[] key);

native bool Shavit_HasStyleSetting(int style, const char[] key);

native bool Shavit_SetStyleSettingFloat(int style, const char[] key, float value, bool replace = true);

native bool Shavit_SetStyleSettingBool(int style, const char[] key, bool value, bool replace = true);

native bool Shavit_SetStyleSettingInt(int style, const char[] key, int value, bool replace = true);

native int Shavit_GetStyleStrings(int style, int stringtype, char[] StyleStrings, int size);

native int Shavit_GetStyleStringsStruct(int style, any[] strings, int size = sizeof(stylestrings_t));

native int Shavit_GetStyleCount();

native void Shavit_GetOrderedStyles(int[] arr, int size);

native int Shavit_GetChatStrings(int stringtype, char[] ChatStrings, int size);

native int Shavit_GetChatStringsStruct(any[] strings, int size = sizeof(chatstrings_t));

native int Shavit_GetHUDSettings(int client);

native void Shavit_SetPracticeMode(int client, bool practice, bool alert);

native bool Shavit_IsPracticeMode(int client);

native void Shavit_SaveSnapshot(int client, any[] snapshot, int size = sizeof(timer_snapshot_t));

native void Shavit_LoadSnapshot(int client, any[] snapshot, int size = sizeof(timer_snapshot_t));

native void Shavit_SetReplayData(int client, ArrayList data, bool cheapCloneHandle=false);

native ArrayList Shavit_GetReplayData(int client, bool cheapCloneHandle=false);

native bool Shavit_IsReplayEntity(int ent);

native int Shavit_StartReplay(int style, int track, float delay, int client, int bot, bool ignorelimit = false);

native int Shavit_StartReplayFromFrameCache(int style, int track, float delay, int client, int bot, bool ignorelimit = false, any[] cache, int size = sizeof(frame_cache_t));

native int Shavit_StartReplayFromFile(int style, int track, float delay, int client, int bot, bool ignorelimit = false, const char[] path);

native bool Shavit_ReloadReplay(int style, int track, bool restart, char[] path = "");

native int Shavit_ReloadReplays(bool restart);

native float Shavit_GetClosestReplayTime(int client);

native int Shavit_GetClosestReplayStyle(int client);

native void Shavit_SetClosestReplayStyle(int client, int style);

native float Shavit_GetClosestReplayVelocityDifference(int client, bool threeD);

native void Shavit_StopChatSound();

native void Shavit_MarkKZMap();

native bool Shavit_IsKZMap();

native int Shavit_GetMapTier(const char[] map);

native StringMap Shavit_GetMapTiers();

native bool Shavit_HasStyleAccess(int client, int style);

native bool Shavit_IsPaused(int client);

native int Shavit_CanPause(int client);

native int Shavit_PrintToChat(int client, const char[] format, any ...);

native void Shavit_PrintToChatAll(const char[] format, any ...);

native void Shavit_LogMessage(const char[] format, any ...);

native float Shavit_GetAvgVelocity(int client);

native float Shavit_GetMaxVelocity(int client);

native void Shavit_SetAvgVelocity(int client, float vel);

native void Shavit_SetMaxVelocity(int client, float vel);

 native int Shavit_GetTotalCheckpoints(int client);

native bool Shavit_GetCheckpoint(int client, int index, any[] cpcache, int size = sizeof(cp_cache_t));

native void Shavit_SetCheckpoint(int client, int index, any[] cpcache, int size = sizeof(cp_cache_t));

native void Shavit_TeleportToCheckpoint(int client, int index, bool suppress = false);

native void Shavit_ClearCheckpoints(int client);

native void Shavit_OpenCheckpointMenu(int client);

native void Shavit_SetClientTimescale(int client, float scale);

native float Shavit_GetClientTimescale(int client);

native int Shavit_GetReplayStatus(int ent);

native int Shavit_SaveCheckpoint(int client);

native int Shavit_GetCurrentCheckpoint(int client);

native void Shavit_SetCurrentCheckpoint(int client, int index);

native int Shavit_GetTimesTeleported(int client);

native int Shavit_GetPlayerPreFrames(int client);

native void Shavit_SetPlayerPreFrames(int client, int PreFrame);

native void Shavit_GetPlainChatrank(int client, char[] buf, int buflen, bool includename=false);

native void Shavit_DeleteWR(int style, int track, const char[] map, int accountid, int recordid, bool delete_sql, bool update_cache);

native int Shavit_GetLoopingBotByName(const char[] name);

public SharedPlugin __pl_shavit =
{
	name = "shavit",
	// SM bug? commented until it's fixed
	// file = "shavit-core.smx",
#if defined REQUIRE_PLUGIN
	required = 1
#else
	required = 0
#endif
};

#if !defined REQUIRE_PLUGIN
public void __pl_shavit_SetNTVOptional()
{
	MarkNativeAsOptional("Shavit_CanPause");
	MarkNativeAsOptional("Shavit_ChangeClientStyle");
	MarkNativeAsOptional("Shavit_DeleteReplay");
	MarkNativeAsOptional("Shavit_DeleteWR");
	MarkNativeAsOptional("Shavit_FinishMap");
	MarkNativeAsOptional("Shavit_ForceHUDUpdate");
	MarkNativeAsOptional("Shavit_FormatChat");
	MarkNativeAsOptional("Shavit_GetBhopStyle");
	MarkNativeAsOptional("Shavit_GetChatStrings");
	MarkNativeAsOptional("Shavit_GetChatStringsStruct");
	MarkNativeAsOptional("Shavit_GetClientCompletions");
	MarkNativeAsOptional("Shavit_GetClientJumps");
	MarkNativeAsOptional("Shavit_GetClientPB");
	MarkNativeAsOptional("Shavit_GetClientTime");
	MarkNativeAsOptional("Shavit_GetClientTrack");
	MarkNativeAsOptional("Shavit_GetDatabase");
	MarkNativeAsOptional("Shavit_GetHUDSettings");
	MarkNativeAsOptional("Shavit_GetMapTier");
	MarkNativeAsOptional("Shavit_GetMapTiers");
	MarkNativeAsOptional("Shavit_GetOrderedStyles");
	MarkNativeAsOptional("Shavit_GetPerfectJumps");
	MarkNativeAsOptional("Shavit_GetPoints");
	MarkNativeAsOptional("Shavit_GetRank");
	MarkNativeAsOptional("Shavit_GetRankedPlayers");
	MarkNativeAsOptional("Shavit_GetRankForTime");
	MarkNativeAsOptional("Shavit_GetRecordAmount");
	MarkNativeAsOptional("Shavit_GetReplayBotCurrentFrame");
	MarkNativeAsOptional("Shavit_GetClientFrameCount");
	MarkNativeAsOptional("Shavit_GetReplayBotFirstFrameTime");
	MarkNativeAsOptional("Shavit_GetReplayBotIndex");
	MarkNativeAsOptional("Shavit_GetReplayBotStyle");
	MarkNativeAsOptional("Shavit_GetReplayBotTrack");
	MarkNativeAsOptional("Shavit_GetReplayBotType");
	MarkNativeAsOptional("Shavit_GetReplayStarter");
	MarkNativeAsOptional("Shavit_GetReplayData");
	MarkNativeAsOptional("Shavit_GetReplayFrameCount");
	MarkNativeAsOptional("Shavit_GetReplayFrames");
	MarkNativeAsOptional("Shavit_GetReplayLength");
	MarkNativeAsOptional("Shavit_GetReplayName");
	MarkNativeAsOptional("Shavit_GetReplayStatus");
	MarkNativeAsOptional("Shavit_GetReplayTime");
	MarkNativeAsOptional("Shavit_GetStageZone");
	MarkNativeAsOptional("Shavit_GetStageCount");
	MarkNativeAsOptional("Shavit_GetStrafeCount");
	MarkNativeAsOptional("Shavit_GetStyleCount");
	MarkNativeAsOptional("Shavit_GetStyleSetting");
	MarkNativeAsOptional("Shavit_GetStyleSettingInt");
	MarkNativeAsOptional("Shavit_GetStyleSettingFloat");
	MarkNativeAsOptional("Shavit_HasStyleSetting");
	MarkNativeAsOptional("Shavit_GetStyleStrings");
	MarkNativeAsOptional("Shavit_GetStyleStringsStruct");
	MarkNativeAsOptional("Shavit_GetSync");
	MarkNativeAsOptional("Shavit_GetTimeOffset");
	MarkNativeAsOptional("Shavit_GetDistanceOffset");
	MarkNativeAsOptional("Shavit_GetTimeForRank");
	MarkNativeAsOptional("Shavit_GetTimerStatus");
	MarkNativeAsOptional("Shavit_GetWorldRecord");
	MarkNativeAsOptional("Shavit_GetWRCount");
	MarkNativeAsOptional("Shavit_GetWRHolders");
	MarkNativeAsOptional("Shavit_GetWRHolderRank");
	MarkNativeAsOptional("Shavit_GetWRName");
	MarkNativeAsOptional("Shavit_GetWRRecordID");
	MarkNativeAsOptional("Shavit_GetZoneData");
	MarkNativeAsOptional("Shavit_GetZoneFlags");
	MarkNativeAsOptional("Shavit_HasStyleAccess");
	MarkNativeAsOptional("Shavit_HijackAngles");
	MarkNativeAsOptional("Shavit_InsideZone");
	MarkNativeAsOptional("Shavit_InsideZoneGetID");
	MarkNativeAsOptional("Shavit_IsClientCreatingZone");
	MarkNativeAsOptional("Shavit_IsKZMap");
	MarkNativeAsOptional("Shavit_IsPaused");
	MarkNativeAsOptional("Shavit_IsPracticeMode");
	MarkNativeAsOptional("Shavit_IsReplayDataLoaded");
	MarkNativeAsOptional("Shavit_LoadSnapshot");
	MarkNativeAsOptional("Shavit_MarkKZMap");
	MarkNativeAsOptional("Shavit_OpenStatsMenu");
	MarkNativeAsOptional("Shavit_PauseTimer");
	MarkNativeAsOptional("Shavit_PrintToChat");
	MarkNativeAsOptional("Shavit_PrintToChatAll");
	MarkNativeAsOptional("Shavit_Rankings_DeleteMap");
	MarkNativeAsOptional("Shavit_ReloadLeaderboards");
	MarkNativeAsOptional("Shavit_ReloadReplay");
	MarkNativeAsOptional("Shavit_ReloadReplays");
	MarkNativeAsOptional("Shavit_Replay_DeleteMap");
	MarkNativeAsOptional("Shavit_RestartTimer");
	MarkNativeAsOptional("Shavit_ResumeTimer");
	MarkNativeAsOptional("Shavit_SaveSnapshot");
	MarkNativeAsOptional("Shavit_SetClientPB");
	MarkNativeAsOptional("Shavit_SetPracticeMode");
	MarkNativeAsOptional("Shavit_SetReplayData");
	MarkNativeAsOptional("Shavit_StartReplay");
	MarkNativeAsOptional("Shavit_StartTimer");
	MarkNativeAsOptional("Shavit_SetStart");
	MarkNativeAsOptional("Shavit_DeleteSetStart");
	MarkNativeAsOptional("Shavit_StopChatSound");
	MarkNativeAsOptional("Shavit_StopTimer");
	MarkNativeAsOptional("Shavit_WR_DeleteMap");
	MarkNativeAsOptional("Shavit_ZoneExists");
	MarkNativeAsOptional("Shavit_Zones_DeleteMap");
	MarkNativeAsOptional("Shavit_GetTotalCheckpoints");
	MarkNativeAsOptional("Shavit_GetCheckpoint");
	MarkNativeAsOptional("Shavit_SetCheckpoint");
	MarkNativeAsOptional("Shavit_TeleportToCheckpoint");
	MarkNativeAsOptional("Shavit_ClearCheckpoints");
	MarkNativeAsOptional("Shavit_OpenCheckpointMenu");
	MarkNativeAsOptional("Shavit_GetClientTimescale");
	MarkNativeAsOptional("Shavit_SetClientTimescale");
	MarkNativeAsOptional("Shavit_SaveCheckpoint");
	MarkNativeAsOptional("Shavit_GetCurrentCheckpoint");
	MarkNativeAsOptional("Shavit_SetCurrentCheckpoint");
	MarkNativeAsOptional("Shavit_GetTimesTeleported");
	MarkNativeAsOptional("Shavit_GetPlayerPreFrames");
	MarkNativeAsOptional("Shavit_SetPlayerPreFrames");
	MarkNativeAsOptional("Shavit_GetClosestReplayTime");
	MarkNativeAsOptional("Shavit_SetStyleSetting");
	MarkNativeAsOptional("Shavit_SetStyleSettingFloat");
	MarkNativeAsOptional("Shavit_SetStyleSettingBool");
	MarkNativeAsOptional("Shavit_SetStyleSettingInt");
	MarkNativeAsOptional("Shavit_GetPlainChatrank");
	MarkNativeAsOptional("Shavit_GetClosestReplayVelocityDifference");
	MarkNativeAsOptional("Shavit_IsReplayEntity");
	MarkNativeAsOptional("Shavit_GetReplayButtons");
	MarkNativeAsOptional("Shavit_GetClosestReplayStyle");
	MarkNativeAsOptional("Shavit_SetClosestReplayStyle");
	MarkNativeAsOptional("Shavit_GetReplayCacheFrameCount");
	MarkNativeAsOptional("Shavit_GetReplayCacheLength");
	MarkNativeAsOptional("Shavit_StartReplayFromFrameCache");
	MarkNativeAsOptional("Shavit_StartReplayFromFile");
	MarkNativeAsOptional("Shavit_GetClientLastStage");
	MarkNativeAsOptional("Shavit_GetStageWR");
	MarkNativeAsOptional("Shavit_GetStagePB");
	MarkNativeAsOptional("Shavit_GetReplayPreFrames");
	MarkNativeAsOptional("Shavit_GetReplayPostFrames");
	MarkNativeAsOptional("Shavit_GetReplayCachePreFrames");
	MarkNativeAsOptional("Shavit_GetReplayCachePostFrames");
	MarkNativeAsOptional("Shavit_GetLoopingBotByName");
}
#endif
