/*
	
	About: mapicon streamer
	Author:	ziggi
	
*/

#if defined _mapicon_stream_included
	#endinput
#endif

#define _mapicon_stream_included
#pragma library mapicon_stream


enum info_MapIcon {
	icon_type,
	Float:Coord_X,
	Float:Coord_Y,
	Float:Coord_Z
};

static gIcons[MAX_ICONS + 1][info_MapIcon];
static bool:gIconsShowed[MAX_PLAYERS][MAX_ICONS + 1];
static gSlotIcons[MAX_PLAYERS][MAX_SHOWED_ICONS + 1];
static gIconsShowedCount[MAX_PLAYERS];


stock icon_Create(markertype, Float:X, Float:Y, Float:Z)
{
	new icon_id = icon_GetFreeSlot();
	if (icon_id == 0) {
		log_Error(_(MAPICON_IS_MAX), MAX_ICONS);
		return 0;
	}
	gIcons[icon_id][icon_type] = markertype;
	gIcons[icon_id][Coord_X] = X;
	gIcons[icon_id][Coord_Y] = Y;
	gIcons[icon_id][Coord_Z] = Z;
	return icon_id;
}

stock icon_Delete(icon_id)
{
	gIcons[icon_id][icon_type] = 0;
	gIcons[icon_id][Coord_X] = 0.0;
	gIcons[icon_id][Coord_Y] = 0.0;
	gIcons[icon_id][Coord_Z] = 0.0;
	foreach (new i : Player) {
		icon_Hide(i, icon_id);
	}
}

stock icon_UpdatePlayerStream(playerid, icon_id)
{
	icon_HouseFix(playerid, icon_id);
	icon_BusinessFix(playerid, icon_id);
	new slot = icon_GetSlot(playerid, icon_id);
	RemovePlayerMapIcon(playerid, slot);
	SetPlayerMapIcon(playerid, slot, gIcons[icon_id][Coord_X], gIcons[icon_id][Coord_Y], gIcons[icon_id][Coord_Z], gIcons[icon_id][icon_type], 0, MAPICON_LOCAL);
}

stock icon_UpdateStream(icon_id)
{
	foreach (new i : Player) {
		icon_UpdatePlayerStream(i, icon_id);
	}
}

stock icon_Timer(playerid)
{
	new inArea;
	for (new icon_id = 1; icon_id <= MAX_ICONS; icon_id++) {
		inArea = IsPlayerInRangeOfPoint(playerid, MAP_ICON_SHOW_DIST, gIcons[icon_id][Coord_X], gIcons[icon_id][Coord_Y], gIcons[icon_id][Coord_Z]);
		// если игрок не в зоне отображения иконки, но иконка ототбражается
		if (!inArea && icon_IsValid(icon_id) && icon_IsShowed(playerid, icon_id)) {
			icon_Hide(playerid, icon_id);
			continue;
		}
		
		// если иконка показана или иконки нет
		if (icon_IsShowed(playerid, icon_id) || !icon_IsValid(icon_id) || icon_GetShowedCount(playerid) >= MAX_SHOWED_ICONS) {
			continue;
		}
		
		// если игрок в зоне отображения иконки и нулевом интерьере
		if (inArea && GetPlayerInterior(playerid) == 0)  {
			icon_Show(playerid, icon_id);
		}
	}
}

stock icon_IsShowed(playerid, icon_id)
{
	return gIconsShowed[playerid][icon_id];
}

stock icon_Hide(playerid, icon_id)
{
	new slot = icon_GetSlot(playerid, icon_id);
	RemovePlayerMapIcon(playerid, slot);
	gSlotIcons[playerid][slot] = 0;
	gIconsShowed[playerid][icon_id] = false;
	gIconsShowedCount[playerid]--;
}

stock icon_Show(playerid, icon_id)
{
	new slot = icon_GetFreePlayerSlot(playerid);
	SetPlayerMapIcon(playerid, slot, gIcons[icon_id][Coord_X], gIcons[icon_id][Coord_Y], gIcons[icon_id][Coord_Z], gIcons[icon_id][icon_type], 0, MAPICON_LOCAL);
	gSlotIcons[playerid][slot] = icon_id;
	gIconsShowed[playerid][icon_id] = true;
	gIconsShowedCount[playerid]++;
}

stock icon_GetShowedCount(playerid)
{
	return gIconsShowedCount[playerid];
}

stock icon_GetPos(icon_id, &Float:icon_x, &Float:icon_y, &Float:icon_z)
{
	if (!icon_IsValid(icon_id)) {
		return 0;
	}
	icon_x = gIcons[icon_id][Coord_X];
	icon_y = gIcons[icon_id][Coord_Y];
	icon_z = gIcons[icon_id][Coord_Z];
	return 1;
}

stock icon_GetIdByPos(Float:X, Float:Y, Float:Z)
{
	for (new icon_id = 1; icon_id <= MAX_ICONS; icon_id++) {
		if (X == gIcons[icon_id][Coord_X] && Y == gIcons[icon_id][Coord_Y] && Z == gIcons[icon_id][Coord_Z]) {
			return icon_id;
		}
	}
	return 0;
}

stock icon_IsValid(icon_id)
{
	if (gIcons[icon_id][icon_type] == 0 && gIcons[icon_id][Coord_X] == 0.0 && gIcons[icon_id][Coord_Y] == 0.0 && gIcons[icon_id][Coord_Z] == 0.0) {
		return 0;
	}
	return 1;
}

stock icon_GetFreeSlot()
{
	for (new icon_id = 1; icon_id <= MAX_ICONS; icon_id++) {
		if (!icon_IsValid(icon_id)) {
			return icon_id;
		}
	}
	return 0;
}

stock icon_GetFreePlayerSlot(playerid)
{
	for (new slot = 1; slot <= MAX_SHOWED_ICONS; slot++) {
		if (gSlotIcons[playerid][slot] == 0) {
			return slot;
		}
	}
	return 0;
}

stock icon_GetSlot(playerid, icon_id)
{
	for (new slot = 1; slot <= MAX_SHOWED_ICONS; slot++) {
		if (gSlotIcons[playerid][slot] == icon_id) {
			return slot;
		}
	}
	return 0;
}
