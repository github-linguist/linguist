_h = execVM "gc\client\GC_Cfg_Anticheat.sqf";
waitUntil {scriptDone _h};
sleep 0.5;

private["_h", "_i", "_script", "_loaded", "_scripts"];
enableSaving [false, false];
WEST setFriend [EAST, 1];
EAST setFriend [WEST, 1];
Resistance setFriend [East, 0];
east setFriend [Resistance, 0];

version         = "1.62";
compatible      = 1.62;
debug           = false;
addonversion    = false;
dedicatedServer = false;
copscount       = 18;
civscount       = 42;
playercount     = 60;
debugarray      = [];
StartMoney      = 49999;

// Disable greetings
{ _x setVariable ["BIS_noCoreConversations", true]; } forEach allUnits;

execVM "briefing.sqf";
call compile preprocessfile "triggers.sqf";

if(!debug)then{["basicintro"]execVM "introcam.sqf";};

if(local server)then{execVM "targets.sqf";};

waitUntil { ( (time > 1) and ((alive player) or (local server)) ) };

player commandChat "Initiating Global Chaos";

_h = [] execVM "playerarrays.sqf";
waitUntil{scriptDone  _h};

call compile preprocessFile "pmc_whitelist.sqf";

_h = [playercount, rolenumber] execVM "initfuncs.sqf";
waitUntil{scriptDone  _h};

_h = [playerarray, playerstringarray, !iscop] execVM "INVvars.sqf";
waituntil{scriptDone  _h};

_h = [] execVM "gc\global\FNC_Loading.sqf";
waituntil {scriptDone _h};
sleep 0.5;

_scripts = [
  "bankexec.sqf",
  "initWPmissions.sqf",
  "gfx.sqf",
  "animList.sqf",
  "miscfunctions.sqf"
];
_loaded = 0;
for [{_i = 0}, {_i < count(_scripts)}, {_i = _i + 1}] do {
    _line = format["%1 - %2 of %3...", ([] call GC_Cfg_RandomLoadingMessage), _i + 1, count(_scripts)];
    if (isDedicated) then {
        diag_log _line;
    } else {
        player commandChat _line;
    };
    _script = _scripts select _i;
    _h = execVM _script;
    waitUntil {scriptDone _h};
    _loaded = _loaded + 1;
};

_h = [] execVM "variables.sqf";
waitUntil{scriptDone  _h};
[SkipTimeDay, SkipTimeNight, 1] execVM "skiptime.sqf";
[] execVM "weather.sqf";

setPitchBank = compile preprocessfile "setPitchBank.sqf";

publicvariable "station1robbed";
publicvariable "station2robbed";
publicvariable "station3robbed";
publicvariable "station4robbed";

// Client
if(!dedicatedserver) then
{
  ["clientloop"] execVM "clientloop.sqf";
  [0,0,0,["clientloop"]] execVM "gangs.sqf";
  [] execVM "respawn.sqf";
  [] execVM "itemactions.sqf";
  [] execVM "petrolactions.sqf";
  [] execVM "nametags.sqf";
  [] execVM "initplayer.sqf";
  [] execVM "power.sqf";
  [2] execVM "markers.sqf";
  [] execVM "salaries.sqf";
  [] execVM "actions.sqf";
  [] execVM "pistolControl.sqf";
  [] execVM "gangmarkers.sqf";
  [] execVM "initNPCs.sqf";

  "FFWarnPvEh" addPublicVariableEventHandler {
    _warnarray  = _this select 1;
    _warntarget = (_warnarray select 0);
    _warning    = (_warnarray select 1);

    if (name player != _warntarget && _warntarget != "__EVERYONE__") exitWith {};

    0 = [_warning] spawn {
      _warning = _this select 0;
      titleText [_warning,"BLACK IN",15];
      player globalChat format ["ADMIN WARNING: %1",_warning];
      hint format ["%1: %2",name player,_warning];
      sleep 0.60;
      player sideChat format ["ADMIN WARNING: %1",_warning];
      hint format ["%1: %2",name player,_warning];
      sleep 0.60;
      player commandChat format ["ADMIN WARNING: %1",_warning];
      hint format ["%1: %2",name player,_warning];
      removeAllWeapons player;
    };
    player commandChat "Client Initialized";
  };

  for [{_i=0}, {_i < (count INV_ItemFabriken)}, {_i=_i+1}] do {_i execVM "facqueue.sqf"};

  player addEventHandler ["fired", {["fired", (_this select 4), (_this select 1)] execVM "stun.sqf";}];
  player addEventHandler ["handleDamage", {_this call compile preprocessfile "sethit.sqf"}];

  waituntil{vehicle player == player};
  player addEventHandler ["fired",{_this execVM "fired.sqf"}];

  []spawn {
    while{true}do
      {
        sleep 1;
        if (player getVariable "flashed" and isciv) then
          {
            _fadeInTime   = 0;
            _fadeOutTime  = 5;
            if (not(alive player)) exitWith {};
            titleCut ["","WHITE OUT",0];
            sleep _fadeOutTime;
            titleCut ["","WHITE IN",0];
            sleep 1;
            player setVariable ["flashed",false, true];

          };
      };
  };

  onKeyPress = compile preprocessFile "onKeyPress.sqf";
  waituntil {!(IsNull (findDisplay 46))};
  (findDisplay 46) displaySetEventHandler ["KeyDown", "_this call onKeyPress"];
};

// Server
if (isServer) then
{
  diag_log "Loading Server Init";
  [] execVM "servermissionfile\loadaccounts.sqf";
  ["serverloop1"] execVM "servermissionfile\commonloop.sqf";
  ["SchleifeDatei"] execVM "servermissionfile\gangsserverloop.sqf";
  [] execVM "servermissionfile\druguse.sqf";
  [] execVM "servermissionfile\drugreplenish.sqf";
  [] execVM "servermissionfile\robpool.sqf";
  [] execVM "servermissionfile\hunting.sqf";
  [] execVM "servermissionfile\setobjectpitches.sqf";
  [] execVM "servermissionfile\governmentconvoy.sqf";

  //=======================rob gas station init and variables================
  [] execVM "stationrobloop.sqf";
  station1money = 5000;
  publicvariable "station1money";

  station2money = 5000;
  publicvariable "station2money";

  station3money = 5000;
  publicvariable "station3money";

  station4money = 5000;
  publicvariable "station4money";

  for [{_i=0}, {_i < (count INV_ItemShops)}, {_i=_i+1}] do {((INV_ItemShops select _i) select 0) execVM "nomove.sqf"; sleep 0.2;};
  for [{_i=0}, {_i < (count workplacejob_deliveryflagarray)}, {_i=_i+1}] do {(workplacejob_deliveryflagarray select _i) execVM "nomove.sqf"; sleep 0.2;};
  diag_log "Server Init Finished";
};

if (count (configFile / "CfgVehicles" / "Car" / "UserActions") > 0 or count (configFile / "CfgVehicles" / "Motorcycle" / "UserActions") > 0 or count (configFile / "CfgVehicles" / "Tank" / "UserActions") > 0 or count (configFile / "CfgVehicles" / "Man" / "UserActions") > 0 or count (configFile / "CfgVehicles" / "Air" / "UserActions") > 0 or count (configFile / "CfgVehicles" / "Ship" / "UserActions") > 0) exitWith {
  [] execVM "nc.sqf";
};

player commandChat "Global Chaos Initialized - Enjoy";
