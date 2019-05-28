// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TimeLeft Plugin
//

#include <amxmodx>

const TASK_TIMEREMAIN_SHORT = 8648458	// 0.8s repeat task
const TASK_TIMEREMAIN_LARGE = 34543		// 1.0s repeat task

// time display flags
const TD_BOTTOM_WHITE_TEXT = 1			// a - display white text on bottom
const TD_USE_VOICE = 2					// b - use voice
const TD_NO_REMAINING_VOICE = 4			// c - don't add "remaining" (only in voice)
const TD_NO_HOURS_MINS_SECS_VOICE = 8		// d - don't add "hours/minutes/seconds" (only in voice)
const TD_SHOW_SPEAK_VALUES_BELOW = 16	// e - show/speak if current time is less than this set in parameter

new g_TimeSet[32][2]
new g_LastTime
new g_CountDown
new g_Switch

// pcvars
new g_amx_time_voice, g_amx_timeleft
new g_mp_timelimit

public plugin_init()
{
	register_plugin("TimeLeft", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("timeleft.txt")
	g_amx_time_voice = register_cvar("amx_time_voice", "1")
	register_srvcmd("amx_time_display", "setDisplaying")
	g_amx_timeleft = register_cvar("amx_timeleft", "00:00", FCVAR_SERVER|FCVAR_EXTDLL|FCVAR_UNLOGGED|FCVAR_SPONLY)
	register_clcmd("say timeleft", "sayTimeLeft", 0, "- displays timeleft")
	register_clcmd("say thetime", "sayTheTime", 0, "- displays current time")
	
	set_task(0.8, "timeRemain", TASK_TIMEREMAIN_SHORT, "", 0, "b")

	g_mp_timelimit = get_cvar_pointer("mp_timelimit")
}

public sayTheTime(id)
{
	if (get_pcvar_num(g_amx_time_voice))
	{
		new mhours[6], mmins[6], whours[32], wmins[32], wpm[6]
		
		get_time("%H", mhours, charsmax(mhours))
		get_time("%M", mmins, charsmax(mmins))
		
		new mins = str_to_num(mmins)
		new hrs = str_to_num(mhours)
		
		if (mins)
			num_to_word(mins, wmins, charsmax(wmins))
		else
			wmins[0] = EOS
		
		if (hrs < 12)
			wpm = "am "
		else
		{
			if (hrs > 12) hrs -= 12
			wpm = "pm "
		}

		if (hrs) 
			num_to_word(hrs, whours, charsmax(whours))
		else
			whours = "twelve "
		
		client_cmd(id, "spk ^"fvox/time_is_now %s_period %s%s^"", whours, wmins, wpm)
	}
	
	new ctime[64]
	
	get_time("%m/%d/%Y - %H:%M:%S", ctime, charsmax(ctime))
	client_print(0, print_chat, "%L:   %s", LANG_PLAYER, "THE_TIME", ctime)
	
	return PLUGIN_CONTINUE
}

public sayTimeLeft(id)
{
	if (get_pcvar_float(g_mp_timelimit))
	{
		new a = get_timeleft()
		
		if (get_pcvar_num(g_amx_time_voice))
		{
			new svoice[128]
			setTimeVoice(svoice, charsmax(svoice), 0, a)
			client_cmd(id, "%s", svoice)
		}
		client_print(0, print_chat, "%L:  %d:%02d", LANG_PLAYER, "TIME_LEFT", (a / 60), (a % 60))
	}
	else
		client_print(0, print_chat, "%L", LANG_PLAYER, "NO_T_LIMIT")
	
	return PLUGIN_CONTINUE
}

setTimeText(text[], len, tmlf, id)
{
	new secs = tmlf % 60
	new mins = tmlf / 60
	
	if (secs == 0)
		formatex(text, len, "%d %L", mins, id, (mins > 1) ? "MINUTES" : "MINUTE")
	else if (mins == 0)
		formatex(text, len, "%d %L", secs, id, (secs > 1) ? "SECONDS" : "SECOND")
	else
		formatex(text, len, "%d %L %d %L", mins, id, (mins > 1) ? "MINUTES" : "MINUTE", secs, id, (secs > 1) ? "SECONDS" : "SECOND")
}

setTimeVoice(text[], len, flags, tmlf)
{
	new temp[7][32]
	new secs = tmlf % 60
	new mins = tmlf / 60
	
	// for (new a = 0;a < 7;++a) // we just created it, already null
		// temp[a][0] = 0

	if (secs > 0)
	{
		num_to_word(secs, temp[4], charsmax(temp[]))
		
		if ( ~flags & TD_NO_HOURS_MINS_SECS_VOICE ) 
			temp[5] = "seconds "	/* there is no "second" in default hl */
	}
	
	if (mins > 59)
	{
		new hours = mins / 60
		
		num_to_word(hours, temp[0], charsmax(temp[]))
		
		if ( ~flags & TD_NO_HOURS_MINS_SECS_VOICE )
			temp[1] = "hours "
		
		mins = mins % 60
	}
	
	if (mins > 0)
	{
		num_to_word(mins, temp[2], charsmax(temp[]))
		
		if ( ~flags & TD_NO_HOURS_MINS_SECS_VOICE )
			temp[3] = "minutes "
	}
	
	if ( ~flags & TD_NO_REMAINING_VOICE )
		temp[6] = "remaining "
	
	return formatex(text, len, "spk ^"vox/%s%s%s%s%s%s%s^"", temp[0], temp[1], temp[2], temp[3], temp[4], temp[5], temp[6])
}

findDispFormat(_time)
{
	// it is important to check i<sizeof BEFORE g_TimeSet[i][0] to prevent out of bound error
	for (new i = 0; i < sizeof(g_TimeSet) && g_TimeSet[i][0]; ++i)
	{
		if (g_TimeSet[i][1] & TD_SHOW_SPEAK_VALUES_BELOW)
		{
			if (g_TimeSet[i][0] > _time)
			{
				if (!g_Switch)
				{
					g_CountDown = g_Switch = _time
					remove_task(TASK_TIMEREMAIN_SHORT)
					set_task(1.0, "timeRemain", TASK_TIMEREMAIN_LARGE, "", 0, "b")
				}
				
				return i
			}
		}
		else if (g_TimeSet[i][0] == _time)
		{
			return i
		}
	}
	
	return -1
}

public setDisplaying()
{
	new arg[32], flags[32], num[32]
	new argc = read_argc() - 1
	new i = 0

	while (i < argc && i < sizeof(g_TimeSet))
	{
		read_argv(i + 1, arg, charsmax(arg))
		parse(arg, flags, charsmax(flags), num, charsmax(num))
		
		g_TimeSet[i][0] = str_to_num(num)
		g_TimeSet[i][1] = read_flags(flags)
		
		i++
	}

	if( i < sizeof(g_TimeSet) )
		g_TimeSet[i][0] = 0 // has to be zeroed in case command is sent twice
	
	return PLUGIN_HANDLED
}

public timeRemain(param[])
{
	new gmtm = get_timeleft()
	new tmlf = g_Switch ? --g_CountDown : gmtm
	new stimel[12]
	
	formatex(stimel, charsmax(stimel), "%02d:%02d", gmtm / 60, gmtm % 60)
	set_pcvar_string(g_amx_timeleft, stimel)
	
	if (g_Switch && gmtm > g_Switch)
	{
		remove_task(TASK_TIMEREMAIN_LARGE)
		g_Switch = 0
		set_task(0.8, "timeRemain", TASK_TIMEREMAIN_SHORT, "", 0, "b")
		
		return
	}

	if (tmlf > 0 && g_LastTime != tmlf)
	{
		g_LastTime = tmlf
		new tm_set = findDispFormat(tmlf)
		
		if (tm_set != -1)
		{
			new flags = g_TimeSet[tm_set][1]
			new arg[128]
			
			if (flags & TD_BOTTOM_WHITE_TEXT)
			{
				new players[MAX_PLAYERS], pnum, plr
				
				get_players(players, pnum, "c")

				if (flags & TD_SHOW_SPEAK_VALUES_BELOW) // yes this is correct flag, just because message should be shorter if it is shown every seconds
					set_hudmessage(255, 255, 255, -1.0, 0.85, 0, 0.0, 1.1, 0.1, 0.5, -1)
				else
					set_hudmessage(255, 255, 255, -1.0, 0.85, 0, 0.0, 3.0, 0.0, 0.5, -1)
					
				for (new i = 0; i < pnum; i++)
				{
					plr = players[i]
					setTimeText(arg, charsmax(arg), tmlf, plr)
					show_hudmessage(plr, "%s", arg)
				}
			}

			if (flags & TD_USE_VOICE)
			{
				setTimeVoice(arg, charsmax(arg), flags, tmlf)
				client_cmd(0, "%s", arg)
			}
		}
	}
}
