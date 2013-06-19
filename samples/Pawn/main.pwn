#include <a_samp>

main() {}

public OnGameModeInit()
{
    return 1;
}

public OnGameModeExit()
{
    return 1;
}

public OnPlayerConnect(playerid)
{
    if (IsPlayerNPC(playerid)) {
        return 1;
    }
    return 1;
}

public OnPlayerDisconnect(playerid, reason)
{
    if (IsPlayerNPC(playerid)) {
        return 1;
    }
    return 1;
}

public OnDialogResponse(playerid, dialogid, response, listitem, inputtext[])
{
    return 1;
}

public OnPlayerClickPlayer(playerid, clickedplayerid, source)
{
    return 1;
}

public OnPlayerPickUpPickup(playerid, pickupid)
{
    return 1;
}

public OnPlayerEnterCheckpoint(playerid)
{
    return 1;
}

public OnPlayerEnterRaceCheckpoint(playerid)
{
    return 1;
}

public OnPlayerDeath(playerid, killerid, reason)
{
    if (IsPlayerNPC(playerid)) {
        return 1;
    }
    return 1;
}

public OnPlayerSpawn(playerid)
{
    if (IsPlayerNPC(playerid)) {
        return 1;
    }
    return 1;
}

public OnPlayerRequestClass(playerid, classid)
{
    return 1;
}

public OnPlayerRequestSpawn(playerid)
{
    return 1;
}

public OnPlayerCommandText(playerid, cmdtext[])
{
    return 1;
}

public OnPlayerText(playerid, text[])
{
    return 0;
}

public OnPlayerUpdate(playerid)
{
    return 1;
}

public OnPlayerKeyStateChange(playerid, newkeys, oldkeys)
{
    return 1;
}

public OnPlayerSelectedMenuRow(playerid, row)
{
    return 1;
}

public OnPlayerExitedMenu(playerid)
{
    return 1;
}

public OnPlayerStateChange(playerid, newstate, oldstate)
{
    return 1;
}

public OnPlayerExitVehicle(playerid, vehicleid)
{
    return 1;
}

public OnPlayerInteriorChange(playerid, newinteriorid, oldinteriorid)
{
    return 1;
}

public OnPlayerEnterVehicle(playerid, vehicleid, ispassenger)
{
    return 1;
}

public OnPlayerStreamIn(playerid, forplayerid)
{
    return 1;
}

public OnPlayerStreamOut(playerid, forplayerid)
{
    return 1;
}

public OnPlayerClickMap(playerid, Float:fX, Float:fY, Float:fZ)
{
    return 1;
}

public OnPlayerTakeDamage(playerid, issuerid, Float:amount, weaponid)
{
    return 1;
}

public OnVehicleDamageStatusUpdate(vehicleid, playerid)
{
    return 1;
}

public OnVehicleStreamIn(vehicleid, forplayerid)
{
    return 1;
}

public OnVehicleStreamOut(vehicleid, forplayerid)
{
    return 1;
}

public OnVehicleSpawn(vehicleid)
{
    return 1;
}

public OnVehicleDeath(vehicleid, killerid)
{
    return 1;
}

public OnVehiclePaintjob(playerid, vehicleid, paintjobid)
{
    return 1;
}

public OnVehicleRespray(playerid, vehicleid, color1, color2)
{
    return 1;
}

public OnVehicleMod(playerid, vehicleid, componentid)
{
    return 1;
}

public OnEnterExitModShop(playerid, enterexit, interiorid)
{
    return 1;
}

public OnRconLoginAttempt(ip[], password[], success)
{
    return 1;
}
