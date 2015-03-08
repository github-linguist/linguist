/*  SA-MP Functions
 *
 *  (c) Copyright 2005-2014, SA-MP Team
 *
 */

#if defined _samp_included
	#endinput
#endif
#define _samp_included
#pragma library samp

#pragma tabsize 4
// Ignores warning 217 for properly indented PAWNO code
// It's tab size is 4 and often uses 4 spaces instead, PAWNCC's is 8

#include <core>
#include <float>
#include <string>
#include <file>
#include <time>
#include <datagram>
#include <a_players>
#include <a_vehicles>
#include <a_objects>
#include <a_sampdb>

// Limits and internal constants
#define MAX_PLAYER_NAME							(24)
#define MAX_PLAYERS								(500)
#define MAX_VEHICLES							(2000)
#define INVALID_PLAYER_ID						(0xFFFF)
#define INVALID_VEHICLE_ID						(0xFFFF)
#define NO_TEAM									(255)
#define MAX_OBJECTS								(1000)
#define INVALID_OBJECT_ID						(0xFFFF)
#define MAX_GANG_ZONES							(1024)
#define MAX_TEXT_DRAWS							(2048)
#define MAX_PLAYER_TEXT_DRAWS					(256)
#define MAX_MENUS								(128)
#define MAX_3DTEXT_GLOBAL						(1024)
#define MAX_3DTEXT_PLAYER						(1024)
#define MAX_PICKUPS								(4096)
#define INVALID_MENU							(0xFF)
#define INVALID_TEXT_DRAW						(0xFFFF)
#define INVALID_GANG_ZONE						(-1)
#define INVALID_3DTEXT_ID						(0xFFFF)

// --------------------------------------------------
// Natives
// --------------------------------------------------

// Util
native print(const string[]);
native printf(const format[], {Float,_}:...);
native format(output[], len, const format[], {Float,_}:...);
native SendClientMessage(playerid, color, const message[]);
native SendClientMessageToAll(color, const message[]);
native SendPlayerMessageToPlayer(playerid, senderid, const message[]);
native SendPlayerMessageToAll(senderid, const message[]);
native SendDeathMessage(killer, killee, weapon);
native SendDeathMessageToPlayer(playerid, killer, killee, weapon);
native GameTextForAll(const string[],time,style);
native GameTextForPlayer(playerid,const string[],time,style);
native SetTimer(funcname[], interval, repeating);
native SetTimerEx(funcname[], interval, repeating, const format[], {Float,_}:...);
native KillTimer(timerid);
native GetTickCount();
native GetMaxPlayers();
native CallRemoteFunction(const function[], const format[], {Float,_}:...);
native CallLocalFunction(const function[], const format[], {Float,_}:...);
native Float:VectorSize(Float:x, Float:y, Float:z);
native Float:asin(Float:value);
native Float:acos(Float:value);
native Float:atan(Float:value);
native Float:atan2(Float:x, Float:y);

// Game
native SetGameModeText(const string[]);
native SetTeamCount(count);
native AddPlayerClass(modelid, Float:spawn_x, Float:spawn_y, Float:spawn_z, Float:z_angle, weapon1, weapon1_ammo, weapon2, weapon2_ammo, weapon3, weapon3_ammo);
native AddPlayerClassEx(teamid, modelid, Float:spawn_x, Float:spawn_y, Float:spawn_z, Float:z_angle, weapon1, weapon1_ammo, weapon2, weapon2_ammo, weapon3, weapon3_ammo);
native AddStaticVehicle(modelid, Float:spawn_x, Float:spawn_y, Float:spawn_z, Float:z_angle, color1, color2);
native AddStaticVehicleEx(modelid, Float:spawn_x, Float:spawn_y, Float:spawn_z, Float:z_angle, color1, color2, respawn_delay);
native AddStaticPickup(model, type, Float:X, Float:Y, Float:Z, virtualworld = 0);
native CreatePickup(model, type, Float:X, Float:Y, Float:Z, virtualworld = 0);
native DestroyPickup(pickup);
native ShowNameTags(show);
native ShowPlayerMarkers(mode);
native GameModeExit();
native SetWorldTime(hour);
native GetWeaponName(weaponid, const weapon[], len);
native EnableTirePopping(enable); // deprecated function
native EnableVehicleFriendlyFire();
native AllowInteriorWeapons(allow);
native SetWeather(weatherid);
native SetGravity(Float:gravity);
native AllowAdminTeleport(allow);
native SetDeathDropAmount(amount);
native CreateExplosion(Float:X, Float:Y, Float:Z, type, Float:Radius);
native EnableZoneNames(enable);
native UsePlayerPedAnims();		// Will cause the players to use CJ running/walking animations
native DisableInteriorEnterExits();  // will disable all interior enter/exits in the game.
native SetNameTagDrawDistance(Float:distance); // Distance at which nametags will start rendering on the client.
native DisableNameTagLOS(); // Disables the nametag Line-Of-Sight checking
native LimitGlobalChatRadius(Float:chat_radius);
native LimitPlayerMarkerRadius(Float:marker_radius);

// Npc
native ConnectNPC(name[], script[]);
native IsPlayerNPC(playerid);

// Admin
native IsPlayerAdmin(playerid);
native Kick(playerid);
native Ban(playerid);
native BanEx(playerid, const reason[]);
native SendRconCommand(command[]);
native GetServerVarAsString(const varname[], buffer[], len);
native GetServerVarAsInt(const varname[]);
native GetServerVarAsBool(const varname[]);
native GetPlayerNetworkStats(playerid, retstr[], retstr_size);
native GetNetworkStats(retstr[], retstr_size);
native GetPlayerVersion(playerid, const version[], len); // Returns the SA-MP client revision as reported by the player
native BlockIpAddress(ip_address[], timems);
native UnBlockIpAddress(ip_address[]);

// Extended admin network stats
native GetServerTickRate();
native NetStats_GetConnectedTime(playerid);
native NetStats_MessagesReceived(playerid);
native NetStats_BytesReceived(playerid);
native NetStats_MessagesSent(playerid);
native NetStats_BytesSent(playerid);
native NetStats_MessagesRecvPerSecond(playerid);
native Float:NetStats_PacketLossPercent(playerid);
native NetStats_ConnectionStatus(playerid);
native NetStats_GetIpPort(playerid, ip_port[], ip_port_len);

// Menu
native Menu:CreateMenu(const title[], columns, Float:x, Float:y, Float:col1width, Float:col2width = 0.0);
native DestroyMenu(Menu:menuid);
native AddMenuItem(Menu:menuid, column, const menutext[]);
native SetMenuColumnHeader(Menu:menuid, column, const columnheader[]);
native ShowMenuForPlayer(Menu:menuid, playerid);
native HideMenuForPlayer(Menu:menuid, playerid);
native IsValidMenu(Menu:menuid);
native DisableMenu(Menu:menuid);
native DisableMenuRow(Menu:menuid, row);
native Menu:GetPlayerMenu(playerid);

// Text Draw
#define TEXT_DRAW_FONT_SPRITE_DRAW	 4
#define TEXT_DRAW_FONT_MODEL_PREVIEW 5

native Text:TextDrawCreate(Float:x, Float:y, text[]);
native TextDrawDestroy(Text:text);
native TextDrawLetterSize(Text:text, Float:x, Float:y);
native TextDrawTextSize(Text:text, Float:x, Float:y);
native TextDrawAlignment(Text:text, alignment);
native TextDrawColor(Text:text, color);
native TextDrawUseBox(Text:text, use);
native TextDrawBoxColor(Text:text, color);
native TextDrawSetShadow(Text:text, size);
native TextDrawSetOutline(Text:text, size);
native TextDrawBackgroundColor(Text:text, color);
native TextDrawFont(Text:text, font);
native TextDrawSetProportional(Text:text, set);
native TextDrawSetSelectable(Text:text, set);
native TextDrawShowForPlayer(playerid, Text:text);
native TextDrawHideForPlayer(playerid, Text:text);
native TextDrawShowForAll(Text:text);
native TextDrawHideForAll(Text:text);
native TextDrawSetString(Text:text, string[]);
native TextDrawSetPreviewModel(Text:text, modelindex);
native TextDrawSetPreviewRot(Text:text, Float:fRotX, Float:fRotY, Float:fRotZ, Float:fZoom = 1.0);
native TextDrawSetPreviewVehCol(Text:text, color1, color2);

// Gang Zones
native GangZoneCreate(Float:minx, Float:miny, Float:maxx, Float:maxy);
native GangZoneDestroy(zone);
native GangZoneShowForPlayer(playerid, zone, color);
native GangZoneShowForAll(zone, color);
native GangZoneHideForPlayer(playerid, zone);
native GangZoneHideForAll(zone);
native GangZoneFlashForPlayer(playerid, zone, flashcolor);
native GangZoneFlashForAll(zone, flashcolor);
native GangZoneStopFlashForPlayer(playerid, zone);
native GangZoneStopFlashForAll(zone);

// Global 3D Text Labels
native Text3D:Create3DTextLabel(text[], color, Float:X, Float:Y, Float:Z, Float:DrawDistance, virtualworld, testLOS=0);
native Delete3DTextLabel(Text3D:id);
native Attach3DTextLabelToPlayer(Text3D:id, playerid, Float:OffsetX, Float:OffsetY, Float:OffsetZ);
native Attach3DTextLabelToVehicle(Text3D:id, vehicleid, Float:OffsetX, Float:OffsetY, Float:OffsetZ);
native Update3DTextLabelText(Text3D:id, color, text[]);

// Per-player 3D Text Labels
native PlayerText3D:CreatePlayer3DTextLabel(playerid, text[], color, Float:X, Float:Y, Float:Z, Float:DrawDistance, attachedplayer=INVALID_PLAYER_ID, attachedvehicle=INVALID_VEHICLE_ID, testLOS=0);
native DeletePlayer3DTextLabel(playerid, PlayerText3D:id);
native UpdatePlayer3DTextLabelText(playerid, PlayerText3D:id, color, text[]);

// Player GUI Dialog
#define DIALOG_STYLE_MSGBOX		0
#define DIALOG_STYLE_INPUT		1
#define DIALOG_STYLE_LIST		2
#define DIALOG_STYLE_PASSWORD	3

native ShowPlayerDialog(playerid, dialogid, style, caption[], info[], button1[], button2[]);

// --------------------------------------------------
// Defines
// --------------------------------------------------

// States
#define PLAYER_STATE_NONE						(0)
#define PLAYER_STATE_ONFOOT						(1)
#define PLAYER_STATE_DRIVER						(2)
#define PLAYER_STATE_PASSENGER					(3)
#define PLAYER_STATE_EXIT_VEHICLE				(4) // (used internally)
#define PLAYER_STATE_ENTER_VEHICLE_DRIVER		(5) // (used internally)
#define PLAYER_STATE_ENTER_VEHICLE_PASSENGER	(6) // (used internally)
#define PLAYER_STATE_WASTED						(7)
#define PLAYER_STATE_SPAWNED					(8)
#define PLAYER_STATE_SPECTATING					(9)

// Marker modes used by ShowPlayerMarkers()
#define PLAYER_MARKERS_MODE_OFF			(0)
#define PLAYER_MARKERS_MODE_GLOBAL		(1)
#define PLAYER_MARKERS_MODE_STREAMED	(2)

// Weapons
#define WEAPON_BRASSKNUCKLE				(1)
#define WEAPON_GOLFCLUB					(2)
#define WEAPON_NITESTICK				(3)
#define WEAPON_KNIFE					(4)
#define WEAPON_BAT						(5)
#define WEAPON_SHOVEL					(6)
#define WEAPON_POOLSTICK				(7)
#define WEAPON_KATANA					(8)
#define WEAPON_CHAINSAW					(9)
#define WEAPON_DILDO					(10)
#define WEAPON_DILDO2					(11)
#define WEAPON_VIBRATOR					(12)
#define WEAPON_VIBRATOR2				(13)
#define WEAPON_FLOWER					(14)
#define WEAPON_CANE						(15)
#define WEAPON_GRENADE					(16)
#define WEAPON_TEARGAS					(17)
#define WEAPON_MOLTOV					(18)
#define WEAPON_COLT45					(22)
#define WEAPON_SILENCED					(23)
#define WEAPON_DEAGLE					(24)
#define WEAPON_SHOTGUN					(25)
#define WEAPON_SAWEDOFF					(26)
#define WEAPON_SHOTGSPA					(27)
#define WEAPON_UZI						(28)
#define WEAPON_MP5						(29)
#define WEAPON_AK47						(30)
#define WEAPON_M4						(31)
#define WEAPON_TEC9						(32)
#define WEAPON_RIFLE					(33)
#define WEAPON_SNIPER					(34)
#define WEAPON_ROCKETLAUNCHER			(35)
#define WEAPON_HEATSEEKER				(36)
#define WEAPON_FLAMETHROWER				(37)
#define WEAPON_MINIGUN					(38)
#define WEAPON_SATCHEL					(39)
#define WEAPON_BOMB						(40)
#define WEAPON_SPRAYCAN					(41)
#define WEAPON_FIREEXTINGUISHER			(42)
#define WEAPON_CAMERA					(43)
#define WEAPON_PARACHUTE				(46)
#define WEAPON_VEHICLE					(49)
#define WEAPON_DROWN					(53)
#define WEAPON_COLLISION				(54)

// Keys
#define KEY_ACTION				(1)
#define KEY_CROUCH				(2)
#define KEY_FIRE				(4)
#define KEY_SPRINT				(8)
#define KEY_SECONDARY_ATTACK	(16)
#define KEY_JUMP				(32)
#define KEY_LOOK_RIGHT			(64)
#define KEY_HANDBRAKE			(128)
#define KEY_LOOK_LEFT			(256)
#define KEY_SUBMISSION			(512)
#define KEY_LOOK_BEHIND			(512)
#define KEY_WALK				(1024)
#define KEY_ANALOG_UP			(2048)
#define KEY_ANALOG_DOWN			(4096)
#define KEY_ANALOG_LEFT			(8192)
#define KEY_ANALOG_RIGHT		(16384)
#define KEY_YES					(65536)
#define KEY_NO					(131072)
#define KEY_CTRL_BACK			(262144)

#define KEY_UP					(-128)
#define KEY_DOWN				(128)
#define KEY_LEFT				(-128)
#define KEY_RIGHT				(128)

// --------------------------------------------------
// Forwards (Callback declarations)
// --------------------------------------------------

forward OnGameModeInit();
forward OnGameModeExit();
forward OnFilterScriptInit();
forward OnFilterScriptExit();
forward OnPlayerConnect(playerid);
forward OnPlayerDisconnect(playerid, reason);
forward OnPlayerSpawn(playerid);
forward OnPlayerDeath(playerid, killerid, reason);
forward OnVehicleSpawn(vehicleid);
forward OnVehicleDeath(vehicleid, killerid);
forward OnPlayerText(playerid, text[]);
forward OnPlayerCommandText(playerid, cmdtext[]);
forward OnPlayerRequestClass(playerid, classid);
forward OnPlayerEnterVehicle(playerid, vehicleid, ispassenger);
forward OnPlayerExitVehicle(playerid, vehicleid);
forward OnPlayerStateChange(playerid, newstate, oldstate);
forward OnPlayerEnterCheckpoint(playerid);
forward OnPlayerLeaveCheckpoint(playerid);
forward OnPlayerEnterRaceCheckpoint(playerid);
forward OnPlayerLeaveRaceCheckpoint(playerid);
forward OnRconCommand(cmd[]);
forward OnPlayerRequestSpawn(playerid);
forward OnObjectMoved(objectid);
forward OnPlayerObjectMoved(playerid, objectid);
forward OnPlayerPickUpPickup(playerid, pickupid);
forward OnVehicleMod(playerid, vehicleid, componentid);
forward OnEnterExitModShop(playerid, enterexit, interiorid);
forward OnVehiclePaintjob(playerid, vehicleid, paintjobid);
forward OnVehicleRespray(playerid, vehicleid, color1, color2);
forward OnVehicleDamageStatusUpdate(vehicleid, playerid);
forward OnUnoccupiedVehicleUpdate(vehicleid, playerid, passenger_seat, Float:new_x, Float:new_y, Float:new_z, Float:vel_x, Float:vel_y, Float:vel_z);
forward OnPlayerSelectedMenuRow(playerid, row);
forward OnPlayerExitedMenu(playerid);
forward OnPlayerInteriorChange(playerid, newinteriorid, oldinteriorid);
forward OnPlayerKeyStateChange(playerid, newkeys, oldkeys);
forward OnRconLoginAttempt( ip[], password[], success );
forward OnPlayerUpdate(playerid);
forward OnPlayerStreamIn(playerid, forplayerid);
forward OnPlayerStreamOut(playerid, forplayerid);
forward OnVehicleStreamIn(vehicleid, forplayerid);
forward OnVehicleStreamOut(vehicleid, forplayerid);
forward OnDialogResponse(playerid, dialogid, response, listitem, inputtext[]);
forward OnPlayerTakeDamage(playerid, issuerid, Float:amount, weaponid, bodypart);
forward OnPlayerGiveDamage(playerid, damagedid, Float:amount, weaponid, bodypart);
forward OnPlayerClickMap(playerid, Float:fX, Float:fY, Float:fZ);
forward OnPlayerClickTextDraw(playerid, Text:clickedid);
forward OnPlayerClickPlayerTextDraw(playerid, PlayerText:playertextid);
forward OnIncomingConnection(playerid, ip_address[], port);
forward OnTrailerUpdate(playerid, vehicleid);

#define CLICK_SOURCE_SCOREBOARD		0
forward OnPlayerClickPlayer(playerid, clickedplayerid, source);

#define EDIT_RESPONSE_CANCEL		0
#define EDIT_RESPONSE_FINAL			1
#define EDIT_RESPONSE_UPDATE		2

forward OnPlayerEditObject( playerid, playerobject, objectid, response, 
Float:fX, Float:fY, Float:fZ, Float:fRotX, Float:fRotY, Float:fRotZ );

forward OnPlayerEditAttachedObject( playerid, response, index, modelid, boneid,
Float:fOffsetX, Float:fOffsetY, Float:fOffsetZ,
Float:fRotX, Float:fRotY, Float:fRotZ,
Float:fScaleX, Float:fScaleY, Float:fScaleZ );

#define SELECT_OBJECT_GLOBAL_OBJECT	1
#define SELECT_OBJECT_PLAYER_OBJECT 2

forward OnPlayerSelectObject(playerid, type, objectid, modelid, Float:fX, Float:fY, Float:fZ);

#define BULLET_HIT_TYPE_NONE			0
#define BULLET_HIT_TYPE_PLAYER			1
#define BULLET_HIT_TYPE_VEHICLE			2
#define BULLET_HIT_TYPE_OBJECT			3
#define BULLET_HIT_TYPE_PLAYER_OBJECT	4

forward OnPlayerWeaponShot(playerid, weaponid, hittype, hitid, Float:fX, Float:fY, Float:fZ);

// --------------------------------------------------