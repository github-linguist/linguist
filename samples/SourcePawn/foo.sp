/**
 * vim: set ts=4 :
 * =============================================================================
 * SourceMod Mapchooser Plugin
 * Creates a map vote at appropriate times, setting sm_nextmap to the winning
 * vote
 *
 * SourceMod (C)2004-2007 AlliedModders LLC.  All rights reserved.
 * =============================================================================
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
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */
 
//#define DEBUG

#if defined DEBUG
	#define assert(%1) if (!(%1)) ThrowError("Debug Assertion Failed");
	#define assert_msg(%1,%2) if (!(%1)) ThrowError(%2);
#else
	#define assert(%1)
	#define assert_msg(%1,%2)
#endif

#pragma semicolon 1
#include <sourcemod>
#include <mapchooser>
#include <nextmap>

public Plugin:myinfo =
{
	name = "MapChooser",
	author = "AlliedModders LLC",
	description = "Automated Map Voting",
	version = SOURCEMOD_VERSION,
	url = "http://www.sourcemod.net/"
};

/* Valve ConVars */
new Handle:g_Cvar_Winlimit = INVALID_HANDLE;
new Handle:g_Cvar_Maxrounds = INVALID_HANDLE;
new Handle:g_Cvar_Fraglimit = INVALID_HANDLE;
new Handle:g_Cvar_Bonusroundtime = INVALID_HANDLE;

/* Plugin ConVars */
new Handle:g_Cvar_StartTime = INVALID_HANDLE;
new Handle:g_Cvar_StartRounds = INVALID_HANDLE;
new Handle:g_Cvar_StartFrags = INVALID_HANDLE;
new Handle:g_Cvar_ExtendTimeStep = INVALID_HANDLE;
new Handle:g_Cvar_ExtendRoundStep = INVALID_HANDLE;
new Handle:g_Cvar_ExtendFragStep = INVALID_HANDLE;
new Handle:g_Cvar_ExcludeMaps = INVALID_HANDLE;
new Handle:g_Cvar_IncludeMaps = INVALID_HANDLE;
new Handle:g_Cvar_NoVoteMode = INVALID_HANDLE;
new Handle:g_Cvar_Extend = INVALID_HANDLE;
new Handle:g_Cvar_DontChange = INVALID_HANDLE;
new Handle:g_Cvar_EndOfMapVote = INVALID_HANDLE;
new Handle:g_Cvar_VoteDuration = INVALID_HANDLE;
new Handle:g_Cvar_RunOff = INVALID_HANDLE;
new Handle:g_Cvar_RunOffPercent = INVALID_HANDLE;

new Handle:g_VoteTimer = INVALID_HANDLE;
new Handle:g_RetryTimer = INVALID_HANDLE;

/* Data Handles */
new Handle:g_MapList = INVALID_HANDLE;
new Handle:g_NominateList = INVALID_HANDLE;
new Handle:g_NominateOwners = INVALID_HANDLE;
new Handle:g_OldMapList = INVALID_HANDLE;
new Handle:g_NextMapList = INVALID_HANDLE;
new Handle:g_VoteMenu = INVALID_HANDLE;

new g_Extends;
new g_TotalRounds;
new bool:g_HasVoteStarted;
new bool:g_WaitingForVote;
new bool:g_MapVoteCompleted;
new bool:g_ChangeMapAtRoundEnd;
new bool:g_ChangeMapInProgress;
new g_mapFileSerial = -1;

new g_NominateCount = 0;
new MapChange:g_ChangeTime;

new Handle:g_NominationsResetForward = INVALID_HANDLE;
new Handle:g_MapVoteStartedForward = INVALID_HANDLE;

/* Upper bound of how many team there could be */
#define MAXTEAMS 10
new g_winCount[MAXTEAMS];

#define VOTE_EXTEND "##extend##"
#define VOTE_DONTCHANGE "##dontchange##"

public OnPluginStart()
{
	LoadTranslations("mapchooser.phrases");
	LoadTranslations("common.phrases");
	
	new arraySize = ByteCountToCells(PLATFORM_MAX_PATH);
	g_MapList = CreateArray(arraySize);
	g_NominateList = CreateArray(arraySize);
	g_NominateOwners = CreateArray(1);
	g_OldMapList = CreateArray(arraySize);
	g_NextMapList = CreateArray(arraySize);
	
	g_Cvar_EndOfMapVote = CreateConVar("sm_mapvote_endvote", "1", "Specifies if MapChooser should run an end of map vote", _, true, 0.0, true, 1.0);

	g_Cvar_StartTime = CreateConVar("sm_mapvote_start", "3.0", "Specifies when to start the vote based on time remaining.", _, true, 1.0);
	g_Cvar_StartRounds = CreateConVar("sm_mapvote_startround", "2.0", "Specifies when to start the vote based on rounds remaining. Use 0 on TF2 to start vote during bonus round time", _, true, 0.0);
	g_Cvar_StartFrags = CreateConVar("sm_mapvote_startfrags", "5.0", "Specifies when to start the vote base on frags remaining.", _, true, 1.0);
	g_Cvar_ExtendTimeStep = CreateConVar("sm_extendmap_timestep", "15", "Specifies how much many more minutes each extension makes", _, true, 5.0);
	g_Cvar_ExtendRoundStep = CreateConVar("sm_extendmap_roundstep", "5", "Specifies how many more rounds each extension makes", _, true, 1.0);
	g_Cvar_ExtendFragStep = CreateConVar("sm_extendmap_fragstep", "10", "Specifies how many more frags are allowed when map is extended.", _, true, 5.0);	
	g_Cvar_ExcludeMaps = CreateConVar("sm_mapvote_exclude", "5", "Specifies how many past maps to exclude from the vote.", _, true, 0.0);
	g_Cvar_IncludeMaps = CreateConVar("sm_mapvote_include", "5", "Specifies how many maps to include in the vote.", _, true, 2.0, true, 6.0);
	g_Cvar_NoVoteMode = CreateConVar("sm_mapvote_novote", "1", "Specifies whether or not MapChooser should pick a map if no votes are received.", _, true, 0.0, true, 1.0);
	g_Cvar_Extend = CreateConVar("sm_mapvote_extend", "0", "Number of extensions allowed each map.", _, true, 0.0);
	g_Cvar_DontChange = CreateConVar("sm_mapvote_dontchange", "1", "Specifies if a 'Don't Change' option should be added to early votes", _, true, 0.0);
	g_Cvar_VoteDuration = CreateConVar("sm_mapvote_voteduration", "20", "Specifies how long the mapvote should be available for.", _, true, 5.0);
	g_Cvar_RunOff = CreateConVar("sm_mapvote_runoff", "0", "Hold run of votes if winning choice is less than a certain margin", _, true, 0.0, true, 1.0);
	g_Cvar_RunOffPercent = CreateConVar("sm_mapvote_runoffpercent", "50", "If winning choice has less than this percent of votes, hold a runoff", _, true, 0.0, true, 100.0);
	
	RegAdminCmd("sm_mapvote", Command_Mapvote, ADMFLAG_CHANGEMAP, "sm_mapvote - Forces MapChooser to attempt to run a map vote now.");
	RegAdminCmd("sm_setnextmap", Command_SetNextmap, ADMFLAG_CHANGEMAP, "sm_setnextmap <map>");

	g_Cvar_Winlimit = FindConVar("mp_winlimit");
	g_Cvar_Maxrounds = FindConVar("mp_maxrounds");
	g_Cvar_Fraglimit = FindConVar("mp_fraglimit");
	g_Cvar_Bonusroundtime = FindConVar("mp_bonusroundtime");
	
	if (g_Cvar_Winlimit != INVALID_HANDLE || g_Cvar_Maxrounds != INVALID_HANDLE)
	{
		decl String:folder[64];
		GetGameFolderName(folder, sizeof(folder));

		if (strcmp(folder, "tf") == 0)
		{
			HookEvent("teamplay_win_panel", Event_TeamPlayWinPanel);
			HookEvent("teamplay_restart_round", Event_TFRestartRound);
			HookEvent("arena_win_panel", Event_TeamPlayWinPanel);
		}
		else if (strcmp(folder, "nucleardawn") == 0)
		{
			HookEvent("round_win", Event_RoundEnd);
		}
		else
		{
			HookEvent("round_end", Event_RoundEnd);
		}
	}
	
	if (g_Cvar_Fraglimit != INVALID_HANDLE)
	{
		HookEvent("player_death", Event_PlayerDeath);		
	}
	
	AutoExecConfig(true, "mapchooser");
	
	//Change the mp_bonusroundtime max so that we have time to display the vote
	//If you display a vote during bonus time good defaults are 17 vote duration and 19 mp_bonustime
	if (g_Cvar_Bonusroundtime != INVALID_HANDLE)
	{
		SetConVarBounds(g_Cvar_Bonusroundtime, ConVarBound_Upper, true, 30.0);		
	}
	
	g_NominationsResetForward = CreateGlobalForward("OnNominationRemoved", ET_Ignore, Param_String, Param_Cell);
	g_MapVoteStartedForward = CreateGlobalForward("OnMapVoteStarted", ET_Ignore);
}

public APLRes:AskPluginLoad2(Handle:myself, bool:late, String:error[], err_max)
{
	RegPluginLibrary("mapchooser");	
	
	CreateNative("NominateMap", Native_NominateMap);
	CreateNative("RemoveNominationByMap", Native_RemoveNominationByMap);
	CreateNative("RemoveNominationByOwner", Native_RemoveNominationByOwner);
	CreateNative("InitiateMapChooserVote", Native_InitiateVote);
	CreateNative("CanMapChooserStartVote", Native_CanVoteStart);
	CreateNative("HasEndOfMapVoteFinished", Native_CheckVoteDone);
	CreateNative("GetExcludeMapList", Native_GetExcludeMapList);
	CreateNative("GetNominatedMapList", Native_GetNominatedMapList);
	CreateNative("EndOfMapVoteEnabled", Native_EndOfMapVoteEnabled);

	return APLRes_Success;
}

public OnConfigsExecuted()
{
	if (ReadMapList(g_MapList,
					 g_mapFileSerial, 
					 "mapchooser",
					 MAPLIST_FLAG_CLEARARRAY|MAPLIST_FLAG_MAPSFOLDER)
		!= INVALID_HANDLE)
		
	{
		if (g_mapFileSerial == -1)
		{
			LogError("Unable to create a valid map list.");
		}
	}
	
	CreateNextVote();
	SetupTimeleftTimer();
	
	g_TotalRounds = 0;
	
	g_Extends = 0;
	
	g_MapVoteCompleted = false;
	
	g_NominateCount = 0;
	ClearArray(g_NominateList);
	ClearArray(g_NominateOwners);
	
	for (new i=0; i<MAXTEAMS; i++)
	{
		g_winCount[i] = 0;	
	}
	

	/* Check if mapchooser will attempt to start mapvote during bonus round time - TF2 Only */
	if ((g_Cvar_Bonusroundtime != INVALID_HANDLE) && !GetConVarInt(g_Cvar_StartRounds))
	{
		if (GetConVarFloat(g_Cvar_Bonusroundtime) <= GetConVarFloat(g_Cvar_VoteDuration))
		{
			LogError("Warning - Bonus Round Time shorter than Vote Time. Votes during bonus round may not have time to complete");
		}
	}
}

public OnMapEnd()
{
	g_HasVoteStarted = false;
	g_WaitingForVote = false;
	g_ChangeMapAtRoundEnd = false;
	g_ChangeMapInProgress = false;
	
	g_VoteTimer = INVALID_HANDLE;
	g_RetryTimer = INVALID_HANDLE;
	
	decl String:map[PLATFORM_MAX_PATH];
	GetCurrentMap(map, sizeof(map));
	PushArrayString(g_OldMapList, map);
				
	if (GetArraySize(g_OldMapList) > GetConVarInt(g_Cvar_ExcludeMaps))
	{
		RemoveFromArray(g_OldMapList, 0);
	}	
}

public OnClientDisconnect(client)
{
	new index = FindValueInArray(g_NominateOwners, client);
	
	if (index == -1)
	{
		return;
	}
	
	new String:oldmap[PLATFORM_MAX_PATH];
	GetArrayString(g_NominateList, index, oldmap, sizeof(oldmap));
	Call_StartForward(g_NominationsResetForward);
	Call_PushString(oldmap);
	Call_PushCell(GetArrayCell(g_NominateOwners, index));
	Call_Finish();
	
	RemoveFromArray(g_NominateOwners, index);
	RemoveFromArray(g_NominateList, index);
	g_NominateCount--;
}

public Action:Command_SetNextmap(client, args)
{
	if (args < 1)
	{
		ReplyToCommand(client, "[SM] Usage: sm_setnextmap <map>");
		return Plugin_Handled;
	}

	decl String:map[PLATFORM_MAX_PATH];
	GetCmdArg(1, map, sizeof(map));

	if (!IsMapValid(map))
	{
		ReplyToCommand(client, "[SM] %t", "Map was not found", map);
		return Plugin_Handled;
	}

	ShowActivity(client, "%t", "Changed Next Map", map);
	LogAction(client, -1, "\"%L\" changed nextmap to \"%s\"", client, map);

	SetNextMap(map);
	g_MapVoteCompleted = true;

	return Plugin_Handled;
}

public OnMapTimeLeftChanged()
{
	if (GetArraySize(g_MapList))
	{
		SetupTimeleftTimer();
	}
}

SetupTimeleftTimer()
{
	new time;
	if (GetMapTimeLeft(time) && time > 0)
	{
		new startTime = GetConVarInt(g_Cvar_StartTime) * 60;
		if (time - startTime < 0 && GetConVarBool(g_Cvar_EndOfMapVote) && !g_MapVoteCompleted && !g_HasVoteStarted)
		{
			InitiateVote(MapChange_MapEnd, INVALID_HANDLE);		
		}
		else
		{
			if (g_VoteTimer != INVALID_HANDLE)
			{
				KillTimer(g_VoteTimer);
				g_VoteTimer = INVALID_HANDLE;
			}	
			
			//g_VoteTimer = CreateTimer(float(time - startTime), Timer_StartMapVote, _, TIMER_FLAG_NO_MAPCHANGE);
			new Handle:data;
			g_VoteTimer = CreateDataTimer(float(time - startTime), Timer_StartMapVote, data, TIMER_FLAG_NO_MAPCHANGE);
			WritePackCell(data, _:MapChange_MapEnd);
			WritePackCell(data, _:INVALID_HANDLE);
			ResetPack(data);
		}		
	}
}

public Action:Timer_StartMapVote(Handle:timer, Handle:data)
{
	if (timer == g_RetryTimer)
	{
		g_WaitingForVote = false;
		g_RetryTimer = INVALID_HANDLE;
	}
	else
	{
		g_VoteTimer = INVALID_HANDLE;
	}
	
	if (!GetArraySize(g_MapList) || !GetConVarBool(g_Cvar_EndOfMapVote) || g_MapVoteCompleted || g_HasVoteStarted)
	{
		return Plugin_Stop;
	}
	
	new MapChange:mapChange = MapChange:ReadPackCell(data);
	new Handle:hndl = Handle:ReadPackCell(data);

	InitiateVote(mapChange, hndl);

	return Plugin_Stop;
}

public Event_TFRestartRound(Handle:event, const String:name[], bool:dontBroadcast)
{
	/* Game got restarted - reset our round count tracking */
	g_TotalRounds = 0;	
}

public Event_TeamPlayWinPanel(Handle:event, const String:name[], bool:dontBroadcast)
{
	if (g_ChangeMapAtRoundEnd)
	{
		g_ChangeMapAtRoundEnd = false;
		CreateTimer(2.0, Timer_ChangeMap, INVALID_HANDLE, TIMER_FLAG_NO_MAPCHANGE);
		g_ChangeMapInProgress = true;
	}
	
	new bluescore = GetEventInt(event, "blue_score");
	new redscore = GetEventInt(event, "red_score");
		
	if(GetEventInt(event, "round_complete") == 1 || StrEqual(name, "arena_win_panel"))
	{
		g_TotalRounds++;
		
		if (!GetArraySize(g_MapList) || g_HasVoteStarted || g_MapVoteCompleted || !GetConVarBool(g_Cvar_EndOfMapVote))
		{
			return;
		}
		
		CheckMaxRounds(g_TotalRounds);
		
		switch(GetEventInt(event, "winning_team"))
		{
			case 3:
			{
				CheckWinLimit(bluescore);
			}
			case 2:
			{
				CheckWinLimit(redscore);				
			}			
			//We need to do nothing on winning_team == 0 this indicates stalemate.
			default:
			{
				return;
			}			
		}
	}
}
/* You ask, why don't you just use team_score event? And I answer... Because CSS doesn't. */
public Event_RoundEnd(Handle:event, const String:name[], bool:dontBroadcast)
{
	if (g_ChangeMapAtRoundEnd)
	{
		g_ChangeMapAtRoundEnd = false;
		CreateTimer(2.0, Timer_ChangeMap, INVALID_HANDLE, TIMER_FLAG_NO_MAPCHANGE);
		g_ChangeMapInProgress = true;
	}
	
	new winner;
	if (strcmp(name, "round_win") == 0)
	{
		// Nuclear Dawn
		winner = GetEventInt(event, "team");
	}
	else
	{
		winner = GetEventInt(event, "winner");
	}
	
	if (winner == 0 || winner == 1 || !GetConVarBool(g_Cvar_EndOfMapVote))
	{
		return;
	}
	
	if (winner >= MAXTEAMS)
	{
		SetFailState("Mod exceed maximum team count - Please file a bug report.");	
	}

	g_TotalRounds++;
	
	g_winCount[winner]++;
	
	if (!GetArraySize(g_MapList) || g_HasVoteStarted || g_MapVoteCompleted)
	{
		return;
	}
	
	CheckWinLimit(g_winCount[winner]);
	CheckMaxRounds(g_TotalRounds);
}

public CheckWinLimit(winner_score)
{	
	if (g_Cvar_Winlimit != INVALID_HANDLE)
	{
		new winlimit = GetConVarInt(g_Cvar_Winlimit);
		if (winlimit)
		{			
			if (winner_score >= (winlimit - GetConVarInt(g_Cvar_StartRounds)))
			{
				InitiateVote(MapChange_MapEnd, INVALID_HANDLE);
			}
		}
	}
}

public CheckMaxRounds(roundcount)
{		
	if (g_Cvar_Maxrounds != INVALID_HANDLE)
	{
		new maxrounds = GetConVarInt(g_Cvar_Maxrounds);
		if (maxrounds)
		{
			if (roundcount >= (maxrounds - GetConVarInt(g_Cvar_StartRounds)))
			{
				InitiateVote(MapChange_MapEnd, INVALID_HANDLE);
			}			
		}
	}
}

public Event_PlayerDeath(Handle:event, const String:name[], bool:dontBroadcast)
{
	if (!GetArraySize(g_MapList) || g_Cvar_Fraglimit == INVALID_HANDLE || g_HasVoteStarted)
	{
		return;
	}
	
	if (!GetConVarInt(g_Cvar_Fraglimit) || !GetConVarBool(g_Cvar_EndOfMapVote))
	{
		return;
	}

	if (g_MapVoteCompleted)
	{
		return;
	}

	new fragger = GetClientOfUserId(GetEventInt(event, "attacker"));

	if (!fragger)
	{
		return;
	}

	if (GetClientFrags(fragger) >= (GetConVarInt(g_Cvar_Fraglimit) - GetConVarInt(g_Cvar_StartFrags)))
	{
		InitiateVote(MapChange_MapEnd, INVALID_HANDLE);
	}
}

public Action:Command_Mapvote(client, args)
{
	InitiateVote(MapChange_MapEnd, INVALID_HANDLE);

	return Plugin_Handled;	
}

/**
 * Starts a new map vote
 *
 * @param when			When the resulting map change should occur.
 * @param inputlist		Optional list of maps to use for the vote, otherwise an internal list of nominations + random maps will be used.
 * @param noSpecials	Block special vote options like extend/nochange (upgrade this to bitflags instead?)
 */
InitiateVote(MapChange:when, Handle:inputlist=INVALID_HANDLE)
{
	g_WaitingForVote = true;
	
	if (IsVoteInProgress())
	{
		// Can't start a vote, try again in 5 seconds.
		//g_RetryTimer = CreateTimer(5.0, Timer_StartMapVote, _, TIMER_FLAG_NO_MAPCHANGE);
		
		new Handle:data;
		g_RetryTimer = CreateDataTimer(5.0, Timer_StartMapVote, data, TIMER_FLAG_NO_MAPCHANGE);
		WritePackCell(data, _:when);
		WritePackCell(data, _:inputlist);
		ResetPack(data);
		return;
	}
	
	/* If the main map vote has completed (and chosen result) and its currently changing (not a delayed change) we block further attempts */
	if (g_MapVoteCompleted && g_ChangeMapInProgress)
	{
		return;
	}
	
	g_ChangeTime = when;
	
	g_WaitingForVote = false;
		
	g_HasVoteStarted = true;
	g_VoteMenu = CreateMenu(Handler_MapVoteMenu, MenuAction:MENU_ACTIONS_ALL);
	SetMenuTitle(g_VoteMenu, "Vote Nextmap");
	SetVoteResultCallback(g_VoteMenu, Handler_MapVoteFinished);

	/* Call OnMapVoteStarted() Forward */
	Call_StartForward(g_MapVoteStartedForward);
	Call_Finish();
	
	/**
	 * TODO: Make a proper decision on when to clear the nominations list.
	 * Currently it clears when used, and stays if an external list is provided.
	 * Is this the right thing to do? External lists will probably come from places
	 * like sm_mapvote from the adminmenu in the future.
	 */
	 
	decl String:map[PLATFORM_MAX_PATH];
	
	/* No input given - User our internal nominations and maplist */
	if (inputlist == INVALID_HANDLE)
	{
		new nominateCount = GetArraySize(g_NominateList);
		new voteSize = GetConVarInt(g_Cvar_IncludeMaps);
		
		/* Smaller of the two - It should be impossible for nominations to exceed the size though (cvar changed mid-map?) */
		new nominationsToAdd = nominateCount >= voteSize ? voteSize : nominateCount;
		
		
		for (new i=0; i<nominationsToAdd; i++)
		{
			GetArrayString(g_NominateList, i, map, sizeof(map));
			AddMenuItem(g_VoteMenu, map, map);
			RemoveStringFromArray(g_NextMapList, map);
			
			/* Notify Nominations that this map is now free */
			Call_StartForward(g_NominationsResetForward);
			Call_PushString(map);
			Call_PushCell(GetArrayCell(g_NominateOwners, i));
			Call_Finish();
		}
		
		/* Clear out the rest of the nominations array */
		for (new i=nominationsToAdd; i<nominateCount; i++)
		{
			GetArrayString(g_NominateList, i, map, sizeof(map));
			/* These maps shouldn't be excluded from the vote as they weren't really nominated at all */
			
			/* Notify Nominations that this map is now free */
			Call_StartForward(g_NominationsResetForward);
			Call_PushString(map);
			Call_PushCell(GetArrayCell(g_NominateOwners, i));
			Call_Finish();			
		}
		
		/* There should currently be 'nominationsToAdd' unique maps in the vote */
		
		new i = nominationsToAdd;
		new count = 0;
		new availableMaps = GetArraySize(g_NextMapList);
		
		while (i < voteSize)
		{
			GetArrayString(g_NextMapList, count, map, sizeof(map));
			count++;
			
			/* Insert the map and increment our count */
			AddMenuItem(g_VoteMenu, map, map);
			i++;
			
			if (count >= availableMaps)
			{
				//Run out of maps, this will have to do.
				break;	
			}
		}
		
		/* Wipe out our nominations list - Nominations have already been informed of this */
		ClearArray(g_NominateOwners);
		ClearArray(g_NominateList);
	}
	else //We were given a list of maps to start the vote with
	{
		new size = GetArraySize(inputlist);
		
		for (new i=0; i<size; i++)
		{
			GetArrayString(inputlist, i, map, sizeof(map));
			
			if (IsMapValid(map))
			{
				AddMenuItem(g_VoteMenu, map, map);
			}	
		}
	}
	
	/* Do we add any special items? */
	if ((when == MapChange_Instant || when == MapChange_RoundEnd) && GetConVarBool(g_Cvar_DontChange))
	{
		AddMenuItem(g_VoteMenu, VOTE_DONTCHANGE, "Don't Change");
	}
	else if (GetConVarBool(g_Cvar_Extend) && g_Extends < GetConVarInt(g_Cvar_Extend))
	{
		AddMenuItem(g_VoteMenu, VOTE_EXTEND, "Extend Map");
	}
	
	new voteDuration = GetConVarInt(g_Cvar_VoteDuration);

	SetMenuExitButton(g_VoteMenu, false);
	VoteMenuToAll(g_VoteMenu, voteDuration);

	LogAction(-1, -1, "Voting for next map has started.");
	PrintToChatAll("[SM] %t", "Nextmap Voting Started");
}

public Handler_VoteFinishedGeneric(Handle:menu,
						   num_votes, 
						   num_clients,
						   const client_info[][2], 
						   num_items,
						   const item_info[][2])
{
	decl String:map[PLATFORM_MAX_PATH];
	GetMenuItem(menu, item_info[0][VOTEINFO_ITEM_INDEX], map, sizeof(map));

	if (strcmp(map, VOTE_EXTEND, false) == 0)
	{
		g_Extends++;
		
		new time;
		if (GetMapTimeLimit(time))
		{
			if (time > 0)
			{
				ExtendMapTimeLimit(GetConVarInt(g_Cvar_ExtendTimeStep)*60);						
			}
		}
		
		if (g_Cvar_Winlimit != INVALID_HANDLE)
		{
			new winlimit = GetConVarInt(g_Cvar_Winlimit);
			if (winlimit)
			{
				SetConVarInt(g_Cvar_Winlimit, winlimit + GetConVarInt(g_Cvar_ExtendRoundStep));
			}					
		}
		
		if (g_Cvar_Maxrounds != INVALID_HANDLE)
		{
			new maxrounds = GetConVarInt(g_Cvar_Maxrounds);
			if (maxrounds)
			{
				SetConVarInt(g_Cvar_Maxrounds, maxrounds + GetConVarInt(g_Cvar_ExtendRoundStep));
			}
		}
		
		if (g_Cvar_Fraglimit != INVALID_HANDLE)
		{
			new fraglimit = GetConVarInt(g_Cvar_Fraglimit);
			if (fraglimit)
			{
				SetConVarInt(g_Cvar_Fraglimit, fraglimit + GetConVarInt(g_Cvar_ExtendFragStep));						
			}
		}

		PrintToChatAll("[SM] %t", "Current Map Extended", RoundToFloor(float(item_info[0][VOTEINFO_ITEM_VOTES])/float(num_votes)*100), num_votes);
		LogAction(-1, -1, "Voting for next map has finished. The current map has been extended.");
		
		// We extended, so we'll have to vote again.
		g_HasVoteStarted = false;
		CreateNextVote();
		SetupTimeleftTimer();
		
	}
	else if (strcmp(map, VOTE_DONTCHANGE, false) == 0)
	{
		PrintToChatAll("[SM] %t", "Current Map Stays", RoundToFloor(float(item_info[0][VOTEINFO_ITEM_VOTES])/float(num_votes)*100), num_votes);
		LogAction(-1, -1, "Voting for next map has finished. 'No Change' was the winner");
		
		g_HasVoteStarted = false;
		CreateNextVote();
		SetupTimeleftTimer();
	}
	else
	{
		if (g_ChangeTime == MapChange_MapEnd)
		{
			SetNextMap(map);
		}
		else if (g_ChangeTime == MapChange_Instant)
		{
			new Handle:data;
			CreateDataTimer(2.0, Timer_ChangeMap, data);
			WritePackString(data, map);
			g_ChangeMapInProgress = false;
		}
		else // MapChange_RoundEnd
		{
			SetNextMap(map);
			g_ChangeMapAtRoundEnd = true;
		}
		
		g_HasVoteStarted = false;
		g_MapVoteCompleted = true;
		
		PrintToChatAll("[SM] %t", "Nextmap Voting Finished", map, RoundToFloor(float(item_info[0][VOTEINFO_ITEM_VOTES])/float(num_votes)*100), num_votes);
		LogAction(-1, -1, "Voting for next map has finished. Nextmap: %s.", map);
	}	
}

public Handler_MapVoteFinished(Handle:menu,
						   num_votes, 
						   num_clients,
						   const client_info[][2], 
						   num_items,
						   const item_info[][2])
{
	if (GetConVarBool(g_Cvar_RunOff) && num_items > 1)
	{
		new Float:winningvotes = float(item_info[0][VOTEINFO_ITEM_VOTES]);
		new Float:required = num_votes * (GetConVarFloat(g_Cvar_RunOffPercent) / 100.0);
		
		if (winningvotes <= required)
		{
			/* Insufficient Winning margin - Lets do a runoff */
			g_VoteMenu = CreateMenu(Handler_MapVoteMenu, MenuAction:MENU_ACTIONS_ALL);
			SetMenuTitle(g_VoteMenu, "Runoff Vote Nextmap");
			SetVoteResultCallback(g_VoteMenu, Handler_VoteFinishedGeneric);

			decl String:map[PLATFORM_MAX_PATH];
			decl String:info1[PLATFORM_MAX_PATH];
			decl String:info2[PLATFORM_MAX_PATH];
			
			GetMenuItem(menu, item_info[0][VOTEINFO_ITEM_INDEX], map, sizeof(map), _, info1, sizeof(info1));
			AddMenuItem(g_VoteMenu, map, info1);
			GetMenuItem(menu, item_info[1][VOTEINFO_ITEM_INDEX], map, sizeof(map), _, info2, sizeof(info2));
			AddMenuItem(g_VoteMenu, map, info2);
			
			new voteDuration = GetConVarInt(g_Cvar_VoteDuration);
			SetMenuExitButton(g_VoteMenu, false);
			VoteMenuToAll(g_VoteMenu, voteDuration);
			
			/* Notify */
			new Float:map1percent = float(item_info[0][VOTEINFO_ITEM_VOTES])/ float(num_votes) * 100;
			new Float:map2percent = float(item_info[1][VOTEINFO_ITEM_VOTES])/ float(num_votes) * 100;
			
			
			PrintToChatAll("[SM] %t", "Starting Runoff", GetConVarFloat(g_Cvar_RunOffPercent), info1, map1percent, info2, map2percent);
			LogMessage("Voting for next map was indecisive, beginning runoff vote");
					
			return;
		}
	}
	
	Handler_VoteFinishedGeneric(menu, num_votes, num_clients, client_info, num_items, item_info);
}

public Handler_MapVoteMenu(Handle:menu, MenuAction:action, param1, param2)
{
	switch (action)
	{
		case MenuAction_End:
		{
			g_VoteMenu = INVALID_HANDLE;
			CloseHandle(menu);
		}
		
		case MenuAction_Display:
		{
	 		decl String:buffer[255];
			Format(buffer, sizeof(buffer), "%T", "Vote Nextmap", param1);

			new Handle:panel = Handle:param2;
			SetPanelTitle(panel, buffer);
		}		
		
		case MenuAction_DisplayItem:
		{
			if (GetMenuItemCount(menu) - 1 == param2)
			{
				decl String:map[PLATFORM_MAX_PATH], String:buffer[255];
				GetMenuItem(menu, param2, map, sizeof(map));
				if (strcmp(map, VOTE_EXTEND, false) == 0)
				{
					Format(buffer, sizeof(buffer), "%T", "Extend Map", param1);
					return RedrawMenuItem(buffer);
				}
				else if (strcmp(map, VOTE_DONTCHANGE, false) == 0)
				{
					Format(buffer, sizeof(buffer), "%T", "Dont Change", param1);
					return RedrawMenuItem(buffer);					
				}
			}
		}		
	
		case MenuAction_VoteCancel:
		{
			// If we receive 0 votes, pick at random.
			if (param1 == VoteCancel_NoVotes && GetConVarBool(g_Cvar_NoVoteMode))
			{
				new count = GetMenuItemCount(menu);
				new item = GetRandomInt(0, count - 1);
				decl String:map[PLATFORM_MAX_PATH];
				GetMenuItem(menu, item, map, sizeof(map));
				
				while (strcmp(map, VOTE_EXTEND, false) == 0)
				{
					item = GetRandomInt(0, count - 1);
					GetMenuItem(menu, item, map, sizeof(map));
				}
				
				SetNextMap(map);			
			}
			else
			{
				// We were actually cancelled. I guess we do nothing.
			}
			
			g_HasVoteStarted = false;
			g_MapVoteCompleted = true;
		}
	}
	
	return 0;
}

public Action:Timer_ChangeMap(Handle:hTimer, Handle:dp)
{
	g_ChangeMapInProgress = false;
	
	new String:map[PLATFORM_MAX_PATH];
	
	if (dp == INVALID_HANDLE)
	{
		if (!GetNextMap(map, sizeof(map)))
		{
			//No passed map and no set nextmap. fail!
			return Plugin_Stop;	
		}
	}
	else
	{
		ResetPack(dp);
		ReadPackString(dp, map, sizeof(map));		
	}
	
	ForceChangeLevel(map, "Map Vote");
	
	return Plugin_Stop;
}

bool:RemoveStringFromArray(Handle:array, String:str[])
{
	new index = FindStringInArray(array, str);
	if (index != -1)
	{
		RemoveFromArray(array, index);
		return true;
	}
	
	return false;
}

CreateNextVote()
{
	assert(g_NextMapList)
	ClearArray(g_NextMapList);
	
	decl String:map[PLATFORM_MAX_PATH];
	new Handle:tempMaps  = CloneArray(g_MapList);
	
	GetCurrentMap(map, sizeof(map));
	RemoveStringFromArray(tempMaps, map);
	
	if (GetConVarInt(g_Cvar_ExcludeMaps) && GetArraySize(tempMaps) > GetConVarInt(g_Cvar_ExcludeMaps))
	{
		for (new i = 0; i < GetArraySize(g_OldMapList); i++)
		{
			GetArrayString(g_OldMapList, i, map, sizeof(map));
			RemoveStringFromArray(tempMaps, map);
		}	
	}

	new limit = (GetConVarInt(g_Cvar_IncludeMaps) < GetArraySize(tempMaps) ? GetConVarInt(g_Cvar_IncludeMaps) : GetArraySize(tempMaps));
	for (new i = 0; i < limit; i++)
	{
		new b = GetRandomInt(0, GetArraySize(tempMaps) - 1);
		GetArrayString(tempMaps, b, map, sizeof(map));		
		PushArrayString(g_NextMapList, map);
		RemoveFromArray(tempMaps, b);
	}
	
	CloseHandle(tempMaps);
}

bool:CanVoteStart()
{
	if (g_WaitingForVote || g_HasVoteStarted)
	{
		return false;	
	}
	
	return true;
}

NominateResult:InternalNominateMap(String:map[], bool:force, owner)
{
	if (!IsMapValid(map))
	{
		return Nominate_InvalidMap;
	}
	
	new index;

	/* Look to replace an existing nomination by this client - Nominations made with owner = 0 aren't replaced */
	if (owner && ((index = FindValueInArray(g_NominateOwners, owner)) != -1))
	{
		new String:oldmap[PLATFORM_MAX_PATH];
		GetArrayString(g_NominateList, index, oldmap, sizeof(oldmap));
		Call_StartForward(g_NominationsResetForward);
		Call_PushString(oldmap);
		Call_PushCell(owner);
		Call_Finish();
		
		SetArrayString(g_NominateList, index, map);
		return Nominate_Replaced;
	}
	
	/* Too many nominated maps. */
	if (g_NominateCount >= GetConVarInt(g_Cvar_IncludeMaps) && !force)
	{
		return Nominate_VoteFull;
	}
	
	/* Map already in the vote */
	if (FindStringInArray(g_NominateList, map) != -1)
	{
		return Nominate_AlreadyInVote;	
	}
	
	
	PushArrayString(g_NominateList, map);
	PushArrayCell(g_NominateOwners, owner);
	g_NominateCount++;
	
	while (GetArraySize(g_NominateList) > GetConVarInt(g_Cvar_IncludeMaps))
	{
		new String:oldmap[PLATFORM_MAX_PATH];
		GetArrayString(g_NominateList, 0, oldmap, sizeof(oldmap));
		Call_StartForward(g_NominationsResetForward);
		Call_PushString(oldmap);
		Call_PushCell(GetArrayCell(g_NominateOwners, 0));
		Call_Finish();
		
		RemoveFromArray(g_NominateList, 0);
		RemoveFromArray(g_NominateOwners, 0);
	}
	
	return Nominate_Added;
}

/* Add natives to allow nominate and initiate vote to be call */

/* native  bool:NominateMap(const String:map[], bool:force, &NominateError:error); */
public Native_NominateMap(Handle:plugin, numParams)
{
	new len;
	GetNativeStringLength(1, len);
	
	if (len <= 0)
	{
	  return false;
	}
	
	new String:map[len+1];
	GetNativeString(1, map, len+1);
	
	return _:InternalNominateMap(map, GetNativeCell(2), GetNativeCell(3));
}

bool:InternalRemoveNominationByMap(String:map[])
{	
	for (new i = 0; i < GetArraySize(g_NominateList); i++)
	{
		new String:oldmap[PLATFORM_MAX_PATH];
		GetArrayString(g_NominateList, i, oldmap, sizeof(oldmap));

		if(strcmp(map, oldmap, false) == 0)
		{
			Call_StartForward(g_NominationsResetForward);
			Call_PushString(oldmap);
			Call_PushCell(GetArrayCell(g_NominateOwners, i));
			Call_Finish();

			RemoveFromArray(g_NominateList, i);
			RemoveFromArray(g_NominateOwners, i);
			g_NominateCount--;

			return true;
		}
	}
	
	return false;
}

/* native  bool:RemoveNominationByMap(const String:map[]); */
public Native_RemoveNominationByMap(Handle:plugin, numParams)
{
	new len;
	GetNativeStringLength(1, len);
	
	if (len <= 0)
	{
	  return false;
	}
	
	new String:map[len+1];
	GetNativeString(1, map, len+1);
	
	return _:InternalRemoveNominationByMap(map);
}

bool:InternalRemoveNominationByOwner(owner)
{	
	new index;

	if (owner && ((index = FindValueInArray(g_NominateOwners, owner)) != -1))
	{
		new String:oldmap[PLATFORM_MAX_PATH];
		GetArrayString(g_NominateList, index, oldmap, sizeof(oldmap));

		Call_StartForward(g_NominationsResetForward);
		Call_PushString(oldmap);
		Call_PushCell(owner);
		Call_Finish();

		RemoveFromArray(g_NominateList, index);
		RemoveFromArray(g_NominateOwners, index);
		g_NominateCount--;

		return true;
	}
	
	return false;
}

/* native  bool:RemoveNominationByOwner(owner); */
public Native_RemoveNominationByOwner(Handle:plugin, numParams)
{	
	return _:InternalRemoveNominationByOwner(GetNativeCell(1));
}

/* native InitiateMapChooserVote(); */
public Native_InitiateVote(Handle:plugin, numParams)
{
	new MapChange:when = MapChange:GetNativeCell(1);
	new Handle:inputarray = Handle:GetNativeCell(2);
	
	LogAction(-1, -1, "Starting map vote because outside request");
	InitiateVote(when, inputarray);
}

public Native_CanVoteStart(Handle:plugin, numParams)
{
	return CanVoteStart();	
}

public Native_CheckVoteDone(Handle:plugin, numParams)
{
	return g_MapVoteCompleted;
}

public Native_EndOfMapVoteEnabled(Handle:plugin, numParams)
{
	return GetConVarBool(g_Cvar_EndOfMapVote);
}

public Native_GetExcludeMapList(Handle:plugin, numParams)
{
	new Handle:array = Handle:GetNativeCell(1);
	
	if (array == INVALID_HANDLE)
	{
		return;	
	}
	new size = GetArraySize(g_OldMapList);
	decl String:map[PLATFORM_MAX_PATH];
	
	for (new i=0; i<size; i++)
	{
		GetArrayString(g_OldMapList, i, map, sizeof(map));
		PushArrayString(array, map);	
	}
	
	return;
}

public Native_GetNominatedMapList(Handle:plugin, numParams)
{
	new Handle:maparray = Handle:GetNativeCell(1);
	new Handle:ownerarray = Handle:GetNativeCell(2);
	
	if (maparray == INVALID_HANDLE)
		return;

	decl String:map[PLATFORM_MAX_PATH];

	for (new i = 0; i < GetArraySize(g_NominateList); i++)
	{
		GetArrayString(g_NominateList, i, map, sizeof(map));
		PushArrayString(maparray, map);

		// If the optional parameter for an owner list was passed, then we need to fill that out as well
		if(ownerarray != INVALID_HANDLE)
		{
			new index = GetArrayCell(g_NominateOwners, i);
			PushArrayCell(ownerarray, index);
		}
	}

	return;
}
