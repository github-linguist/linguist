/* 
 * ===============
 *  INTRODUCTION: 
 * ===============
 * 
 * fixes.inc - Community patch for buggy SA:MP functions.
 * 
 * ================
 *  LIST OF FIXES: 
 * ================
 * 
 * FIX:      GetPlayerColor
 * PROBLEM:  Returns "0" if "SetPlayerColor" has never been called.
 * SOLUTION: Call "SetPlayerColor" in "OnPlayerConnect".
 * SEE:      "OnPlayerConnect".
 * AUTHOR:   KoczkaHUN
 * POST:     http://forum.sa-mp.com/showpost.php?p=1486048
 * 
 * FIX:      FILTERSCRIPT
 * PROBLEM:  Despite the fact that is in every new script, many people don't
 *           define "FILTERSCRIPT" where appropriate.
 * SOLUTION: Provide an "IS_FILTERSCRIPT" variable (note the naming to match
 *           the original macro).
 * AUTHOR:   Y_Less
 * SEE:      "OnFilterScriptInit" and "OnGameModeInit".
 * 
 * FIX:      SpawnPlayer
 * PROBLEM:  Kills the player if they are in a vehicle.
 * SOLUTION: Remove the from the vehicle.
 * SEE:      "FIXES_SpawnPlayer".
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetPlayerName
 * PROBLEM:  Using "SetPlayerName" when the new name only differs from the old
 *           name in case does not alter the name at all.
 * SOLUTION: Change their name twice - once to "_FIXES TEMP NAME" and then to
 *           the actual required name.
 * SEE:      "OnPlayerConnect" and "FIXES_SetPlayerName".
 * AUTHOR:   Y_Less/Slice/xX_Simon_Xx
 * POST:     https://github.com/Y-Less/sa-mp-fixes/issues/3
 * 
 * FIX:      GetPlayerSkin
 * PROBLEM:  Returns the new skin after "SetSpawnInfo" is called but before the
 *           player actually respawns to get the new skin.
 * SOLUTION: Record the skin in "OnPlayerSpawn" and always return that one.
 * SEE:      "OnPlayerSpawn", "FIXES_GetPlayerSkin" and "FIXES_SetPlayerSkin".
 * AUTHOR:   Y_Less
 * 
 * FIX:      GetWeaponName
 * PROBLEM:  Returns nothing for 18, 44, and 45.
 * SOLUTION: Return the correct names ("Molotov Cocktail", "Thermal Goggles",
 *           and "Night vision Goggles").
 * SEE:      "FIXES_GetWeaponName".
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetPlayerWorldBounds
 * PROBLEM:  Aiming can bypass the edge.
 * SOLUTION: Check for the player leaving the area and reset them to their last
 *           good position if they leave the area (aiming or not).
 * SEE:      "OnPlayerUpdate" and "FIXES_SetPlayerWorldBounds".
 * AUTHOR:   Y_Less
 * 
 * FIX:      TogglePlayerControllable
 * PROBLEM:  Other players see you moving on the spot.
 * SOLUTION: Return 0 in OnPlayerUpdate.
 * SEE:      "FIXES_TogglePlayerControllable" and "OnPlayerUpdate".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=876854
 * 
 * FIX:      HydraSniper
 * PROBLEM:  Entering military aircraft with a sniper rifle messes up views.
 * SOLUTION: Set their armed weapon to fists.
 * SEE:      "OnPlayerStateChange", "FIXES_GivePlayerWeapon",
 *           "FIXES_SetPlayerArmedWeapon".
 * AUTHOR:   funky1234
 * POST:     http://forum.sa-mp.com/showpost.php?p=965644
 * 
 * FIX:      IsPlayerInCheckpoint
 * PROBLEM:  Function returns an undefined value if it is called before any
 *           other checkpoint functions are called to initialise the value.
 * SOLUTION: Call "DisablePlayerCheckpoint" when they connect.
 * SEE:      "OnPlayerConnect".
 * AUTHOR:   Y_Less
 * 
 * FIX:      IsPlayerInRaceCheckpoint
 * PROBLEM:  Function returns an undefined value if it is called before any
 *           other race checkpoint functions are called to initialise the value.
 * SOLUTION: Call "DisablePlayerRaceCheckpoint" when they connect.
 * SEE:      "OnPlayerConnect".
 * AUTHOR:   Y_Less
 * 
 * FIX:      GetPlayerWeapon
 * PROBLEM:  Returns the old value after using "SetPlayerArmedWeapon" when they
 *           are in a vehicle.
 * SOLUTION: If "SetPlayerArmedWeapon" is called in a vehicle, store the new
 *           value and return that instead.
 * SEE:      "OnPlayerStateChange", "FIXES_SetPlayerArmedWeapon", and
 *           "FIXES_GetPlayerWeapon".
 * AUTHOR:   Y_Less
 * 
 * FIX:      PutPlayerInVehicle
 * PROBLEM:  If this is used on a passenger the driver of their old vehicle
 *           doesn't see them in their new vehicle.
 * SOLUTION: Remove them from the vehicle first.
 * SEE:      "OnPlayerStateChange" and "FIXES_PutPlayerInVehicle".
 * AUTHOR:   leong124/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1265965
 * 
 * FIX:      KEY_AIM
 * PROBLEM:  "KEY_AIM" isn't defined by default.
 * SOLUTION: Define it.
 * SEE:      N/A.
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetPlayerCheckpoint
 * PROBLEM:  If a checkpoint is already set it will use the size of that
 *           checkpoint instead of the new one.
 * SOLUTION: Call "DisablePlayerCheckpoint" before setting the checkpoint.
 * SEE:      "FIXES_SetPlayerCheckpoint".
 * AUTHOR:   KoczkaHUN
 * POST:     http://forum.sa-mp.com/showpost.php?p=1482401
 * 
 * FIX:      SetPlayerRaceCheckpoint
 * PROBLEM:  If a checkpoint is already set it will use the size of that
 *           checkpoint instead of the new one.
 * SOLUTION: Call "DisablePlayerRaceCheckpoint" before setting the checkpoint.
 * SEE:      "FIXES_SetPlayerRaceCheckpoint".
 * AUTHOR:   KoczkaHUN
 * POST:     http://forum.sa-mp.com/showpost.php?p=1482401
 * 
 * FIX:      TextDrawCreate
 * PROBLEM:  Crashes on a blank string.
 * SOLUTION: Intercept blank strings.
 * SEE:      "FIXES_TextDrawCreate".
 * AUTHOR:   wups
 * POST:     http://forum.sa-mp.com/showpost.php?p=1484008
 * 
 * FIX:      TextDrawSetString
 * PROBLEM:  Crashes on a blank string and size greater than 1024.
 * SOLUTION: Intercept blank strings and truncate long strings.
 * SEE:      "FIXES_TextDrawSetString".
 * AUTHOR:   TomTrox
 * POST:     http://forum.sa-mp.com/showpost.php?p=1487870
 * 
 * FIX:      CreatePlayerTextDraw
 * PROBLEM:  Crashes on a blank string.
 * SOLUTION: Intercept blank strings.
 * SEE:      "FIXES_CreatePlayerTextDraw".
 * AUTHOR:   wups/Y_Less
 * 
 * FIX:      PlayerTextDrawSetString
 * PROBLEM:  Crashes on a blank string and size greater than 1024.
 * SOLUTION: Intercept blank strings and truncate long strings.
 * SEE:      "FIXES_PlayerTextDrawSetString".
 * AUTHOR:   TomTrox/Y_Less
 * 
 * FIX:      AllowInteriorWeapons
 * PROBLEM:  Does nothing.
 * SOLUTION: Set the player's weapon to fists in an interior.
 * SEE:      "FIXES_AllowInteriorWeapons", "OnGameModeInit" and
 *           "OnPlayerUpdate".
 * AUTHOR:   KoczkaHUN
 * POST:     http://forum.sa-mp.com/showpost.php?p=1502696
 * 
 * FIX:      OnPlayerEnterVehicle
 * PROBLEM:  Crashes other players when people enter an invalid seat.
 * SOLUTION: Desync the people with invalid seats.
 * SEE:      "OnPlayerStateChange" and "OnPlayerUpdate".
 * AUTHOR:   RyDeR`/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1410296
 * 
 * FIX:      OnPlayerEnterVehicle_2
 * PROBLEM:  Crashes the server when hacks enter an invalid vehicle.
 * SOLUTION: Desync the people with invalid vehicles.
 * SEE:      "OnPlayerEnterVehicle".
 * AUTHOR:   im/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=2340900
 * 
 * FIX:      AllowTeleport
 * PROBLEM:  0.3dRC9 removed "AllowPlayerTeleport" and "AllowAdminTeleport" in
 *           favour of "OnPlayerClickMap".  Some scripts used the old code and.
 * SOLUTION: Teleport the player in "OnPlayerClickMap".
 * SEE:      "OnPlayerClickMap", "FIXES_AllowPlayerTeleport", and
 *           "FIXES_AllowAdminTeleport".
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetPlayerSpecialAction
 * PROBLEM:  Removing jetpacks from players by setting their special action to 0
 *           causes the sound to stay until death.
 * SOLUTION: Call "ClearAnimations" before "SetPlayerSpecialAction".
 * SEE:      "FIXES_SetPlayerSpecialAction".
 * AUTHOR:   MP2
 * POST:     Private message from MP2.
 * 
 * FIX:      OnDialogResponse
 * PROBLEM:  Cheaters can spoof the dialogid they are using to respond to ones
 *           they can't actually see.
 * SOLUTION: Store the displayed dialogid and use that instead.
 * SEE:      "FIXES_OnDialogResponse", "FIXES_ShowPlayerDialog".
 * AUTHOR:   Y_Less
 * FIXED IN: 0.3e RC6
 * 
 * FIX:      GetPlayerDialog
 * PROBLEM:  This function doesn't exist.  Fixed for hidden dialogs.
 * SOLUTION: Add it.  DEFAULTS TO FALSE!
 * SEE:      "FIXES_GetPlayerDialog".
 * AUTHOR:   Y_Less/xX_Simon_Xx
 * POST:     http://forum.sa-mp.com/showpost.php?p=2141254
 * 
 * FIX:      PlayerDialogResponse
 * PROBLEM:  A player's dialog doesn't hide when the gamemode restarts, causing
 *           the server to print "Warning: PlayerDialogResponse PlayerId: 0
 *           dialog ID doesn't match last sent dialog ID".
 * SOLUTION: Hide it.
 * SEE:      "OnPlayerConnect", "OnGameModeExit".
 * AUTHOR:   xX_Simon_Xx
 * POST:     https://github.com/Y-Less/sa-mp-fixes/issues/2
 * 
 * FIX:      SetSpawnInfo
 * PROBLEM:  Kicks the player if "SpawnPlayer" is called before "SetSpawnInfo".
 * SOLUTION: Call "SetSpawnInfo" at least once.
 * SEE:      "OnPlayerConnect".
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetPlayerSkin
 * PROBLEM:  Breaks sitting on bikes.
 * SOLUTION: Put them back in the vehicle after setting their skin.
 * SEE:      "FIXES_SetPlayerSkin".
 * AUTHOR:   CyNiC
 * POST:     http://forum.sa-mp.com/showpost.php?p=1756094
 * 
 * FIX:      HideMenuForPlayer
 * PROBLEM:  Crashes when passed an invalid menu ID.
 * SOLUTION: Don't hide it when passed an invalid menu.
 * SEE:      "FIXES_HideMenuForPlayer".
 * AUTHOR:   Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1787297
 * 
 * FIX:      valstr
 * PROBLEM:  Crashes on large numbers.
 * SOLUTION: Use "format" instead.
 * SEE:      "FIXES_valstr".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fclose
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fclose".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fwrite
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fwrite".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fread
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fread".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fputchar
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fputchar".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fgetchar
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fgetchar".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fblockwrite
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fblockwrite".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fblockread
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fblockread".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      fseek
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_fseek".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      flength
 * PROBLEM:  Crashes on an invalid handle.
 * SOLUTION: Check for an invalid handle.
 * SEE:      "FIXES_flength".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1790300
 * 
 * FIX:      file_inc
 * PROBLEM:  Includes or excludes all the file function fixes together (can
 *           cause major overhead).
 * SOLUTION: Optionally group them all under one define.  DEFAULTS TO FALSE!
 * SEE:      "FIX_file_inc".
 * AUTHOR:   Y_Less
 * 
 * FIX:      IsPlayerAttachedObjectSlotUsed
 * PROBLEM:  Doesn't work in OnPlayerDisconnect.
 * SOLUTION: Maintain an internal record of slots used.
 * SEE:      "FIXES_SetPlayerAttachedObject",
 *           "FIXES_RemovePlayerAttachedObject",
 *           "FIXES_IsPAttachedObjectSlotUsed", and
 *           "OnPlayerDisconnect".
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetPlayerAttachedObject
 * PROBLEM:  Doesn't remove objects when the mode ends.
 * SOLUTION: Remove them.
 * SEE:      "FIXES_SetPlayerAttachedObject",
 *           "FIXES_RemovePlayerAttachedObject", and
 *           "OnPlayerDisconnect".
 * AUTHOR:   Y_Less
 * 
 * FIX:      OnPlayerDeath
 * PROBLEM:  Clients get stuck when they die with an animation applied.
 * SOLUTION: Clear their animations.
 * SEE:      "OnPlayerDeath" and "OnPlayerUpdate".
 * AUTHOR:   h02
 * POST:     http://forum.sa-mp.com/showpost.php?p=1641144
 * 
 * FIX:      strins
 * PROBLEM:  Ignores the "maxlength" parameter causing possible crashes.
 * SOLUTION: Manually check the length.
 * SEE:      "FIXES_strins".
 * AUTHOR:   Slice/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1860495
 * POST:     http://forum.sa-mp.com/showpost.php?p=1864706
 * 
 * FIX:      IsPlayerConnected
 * PROBLEM:  Only uses the lower two bytes of a passed ID.
 * SOLUTION: Mask the numbers.
 * SEE:      "FIXES_IsPlayerConnected".
 * AUTHOR:   Slice
 * POST:     http://forum.sa-mp.com/showpost.php?p=1860464
 * 
 * FIX:      OnPlayerCommandText
 * PROBLEM:  Can crash ZCMD when passed a null string.
 * SOLUTION: Pass NULL if invalid inputs given.
 * SEE:      "OnPlayerCommandText".
 * AUTHOR:   Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1909511
 * 
 * FIX:      TrainExit
 * PROBLEM:  When getting out of a train entered by "PutPlayerInVehicle", the
 *           camera does not reset properly.
 * SOLUTION: Reset the camera.
 * SEE:      "FIXES_PutPlayerInVehicle", "FIXES_OnPlayerStateChange".
 * AUTHOR:   Terminator3/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1980214
 * 
 * FIX:      Kick
 * PROBLEM:  Calling "Kick" in "OnPlayerConnect" doesn't work properly.
 * SOLUTION: Defer it.
 * SEE:      "OnPlayerConnect", "FIXES_Kick".
 * AUTHOR:   Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1989453
 * FIXED IN: 0.3x
 * 
 * FIX:      OnVehicleMod
 * PROBLEM:  Crashes other players when invalid mods are applied.
 * SOLUTION: Desync the player.
 * SEE:      "OnVehicleMod".
 * AUTHOR:   JernejL/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1671500
 * 
 * FIX:      random
 * PROBLEM:  Doesn't work with negative numbers.
 * SOLUTION: Invert then reinvert.  DEFAULTS TO FALSE!
 * SEE:      "FIXES_random".
 * AUTHOR:   xX_Simon_Xx
 * POST:     http://forum.sa-mp.com/showpost.php?p=2141254
 * 
 * FIX:      sleep
 * PROBLEM:  Leaks bytes from the stack.
 * SOLUTION: Call a function to store the correct value.
 * SEE:      "FIXES_sleep".
 * AUTHOR:   Y_Less
 * 
 * FIX:      AddMenuItem
 * PROBLEM:  Crashes when passed an invalid menu ID.
 * SOLUTION: Don't hide it when passed an invalid menu.
 * SEE:      "FIXES_AddMenuItem".
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetMenuColumnHeader
 * PROBLEM:  Crashes when passed an invalid menu ID.
 * SOLUTION: Don't hide it when passed an invalid menu.
 * SEE:      "FIXES_SetMenuColumnHeader".
 * AUTHOR:   Y_Less
 * 
 * FIX:      ShowMenuForPlayer
 * PROBLEM:  Crashes when passed an invalid menu ID.
 * SOLUTION: Don't hide it when passed an invalid menu.
 * SEE:      "FIXES_ShowMenuForPlayer".
 * AUTHOR:   Y_Less
 * 
 * FIX:      HideMenuForPlayer
 * PROBLEM:  Crashes when passed an invalid menu ID.
 * SOLUTION: Don't hide it when passed an invalid menu.
 * SEE:      "FIXES_HideMenuForPlayer".
 * AUTHOR:   Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=1787297
 * 
 * FIX:      HideMenuForPlayer_2
 * PROBLEM:  Ignores the "menuid" parameter.
 * SOLUTION: Only hide the correct menu.  DEFAULTS TO FALSE!
 * SEE:      "FIXES_HideMenuForPlayer".
 * AUTHOR:   Y_Less
 * 
 * FIX:      DisableMenu
 * PROBLEM:  Crashes when passed an invalid menu ID.
 * SOLUTION: Don't hide it when passed an invalid menu.
 * SEE:      "FIXES_DisableMenu".
 * AUTHOR:   Y_Less
 * 
 * FIX:      DisableMenuRow
 * PROBLEM:  Crashes when passed an invalid menu ID.
 * SOLUTION: Don't hide it when passed an invalid menu.
 * SEE:      "FIXES_DisableMenuRow".
 * AUTHOR:   Y_Less
 * 
 * FIX:      Menus
 * PROBLEM:  All menu function fixes are included separately for major overhead.
 * SOLUTION: Optionally group them all under one define.
 * SEE:      "FIX_Menus", "_FIX_Menus"
 * AUTHOR:   Y_Less
 * 
 * FIX:      GetPlayerMenu
 * PROBLEM:  Returns previous menu when none is displayed.
 * SOLUTION: Return the correct value.
 * SEE:      "FIXES_GetPlayerMenu", "OnPlayerSelectedMenuRow".
 * AUTHOR:   Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=2295867
 * 
 * FIX:      GetPlayerInterior
 * PROBLEM:  Always returns 0 for NPCs.
 * SOLUTION: Return the correct value.
 * SEE:      "FIXES_GetPlayerInterior", "FIXES_SetPlayerInterior".
 * AUTHOR:   Y_Less/xX_Simon_Xx
 * POST:     http://forum.sa-mp.com/showpost.php?p=2309246
 * 
 * FIX:      ClearAnimations
 * PROBLEM:  Use ClearAnimation while you are in a vehicle cause the player exit
 *           from it.
 * SOLUTION: Apply an animation instead of clear animation.
 * SEE:      "FIXES_ClearAnimations".
 * AUTHOR:   xX_Simon_Xx
 * POST:     https://github.com/Y-Less/sa-mp-fixes/issues/4
 * 
 * FIX:      ClearAnimations_2
 * PROBLEM:  ClearAnimations doesn't do anything when the animation ends if we
 *           pass 1 for the freeze parameter in ApplyAnimation.
 * SOLUTION: Apply an idle animation for stop and then use ClearAnimation.
 * SEE:      "FIXES_ClearAnimations".
 * AUTHOR:   xX_Simon_Xx
 * POST:     https://github.com/Y-Less/sa-mp-fixes/issues/4
 * 
 * FIX:      DriveBy
 * PROBLEM:  If you press KEY_CROUCH while you're passenger and if you are
 *           armed, the player start to aim; if you repress KEY_CROUCH the
 *           player don't return in vehicle.
 * SOLUTION: Apply the animation to return the player in the vehicle.
 * SEE:      "OnPlayerKeyStateChange".
 * AUTHOR:   xX_Simon_Xx
 * POST:     https://github.com/Y-Less/sa-mp-fixes/issues/13
 * 
 * FIX:      GangZoneCreate
 * PROBLEM:  Gang zones bug on the main map for players at certain angles
 *           relative to them.
 * SOLUTION: Set a non floating value for the gang zone co-ordinate.
 * SEE:      "FIXES_GangZoneCreate".
 * AUTHOR:   xX_Simon_Xx/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=2144109
 * 
 * FIX:      SPECIAL_ACTION_PISSING
 * PROBLEM:  "SPECIAL_ACTION_PISSING" isn't defined by default.
 * SOLUTION: Define it.
 * SEE:      N/A.
 * AUTHOR:   xX_Simon_Xx
 * POST:     https://github.com/Y-Less/sa-mp-fixes/issues/6
 * 
 * FIX:      IsValidVehicle
 * PROBLEM:  "IsValidVehicle" isn't defined by default.
 * SOLUTION: Define it.
 * SEE:      N/A.
 * AUTHOR:   xX_Simon_Xx
 * POST:     http://forum.sa-mp.com/showpost.php?p=2693650
 * 
 * FIX:      ApplyAnimation
 * PROBLEM:  Passing an invalid animation library in ApplyAnimation causes a
 *           client crash for streamed in players.
 * SOLUTION: Block ApplyAnimation when an invalid library is passed.
 * SEE:      "FIXES_ApplyAnimation".
 * AUTHOR:   xX_Simon_Xx
 * POST:     https://github.com/Y-Less/sa-mp-fixes/issues/5
 * 
 * FIX:      ApplyAnimation_2
 * PROBLEM:  First time a library is used, it does nothing.
 * SOLUTION: Apply animations twice when first using a library.
 * SEE:      "FIXES_ApplyAnimation".
 * AUTHOR:   xX_Simon_Xx/Lordzy/Y_Less
 * POST:     http://forum.sa-mp.com/showpost.php?p=3052627
 * 
 * FIX:      OnPlayerSpawn
 * PROBLEM:  San Andreas deducts $100 from players.
 * SOLUTION: Give them it back.
 * SEE:      "OnPlayerSpawn".
 * AUTHOR:   Y_Less
 * 
 * FIX:      GetGravity
 * PROBLEM:  "GetGravity" isn't defined by default.
 * SOLUTION: Define it.
 * SEE:      N/A.
 * AUTHOR:   Whitetiger
 * POST:     http://forum.sa-mp.com/showpost.php?p=1706447
 * 
 * FIX:      gpci
 * PROBLEM:  "gpci" isn't defined by default.
 * SOLUTION: Define it.
 * SEE:      N/A.
 * AUTHOR:   xX_Simon_Xx
 * POST:     http://pastebin.com/VQSGpbSm
 * 
 * FIX:      Natives
 * PROBLEM:  Several natives are included by default, this enables or disables
 *           them all.  Therefore this is an umbrella fix for several fixes.
 * SOLUTION: Define them.
 * SEE:      "FIX_GetGravity", "FIX_gpci", "FIX_IsValidVehicle".
 * AUTHOR:   Y_Less
 * 
 * FIX:      OnPlayerConnect
 * PROBLEM:  This function isn't called for players when a filterscript starts.
 * SOLUTION: Call it for all connected players.
 * SEE:      "FIXES_OnFilterScriptInit".
 * AUTHOR:   Y_Less
 * 
 * FIX:      OnPlayerDisconnect
 * PROBLEM:  This function isn't called for players when a filterscript ends.
 * SOLUTION: Call it for all connected players.
 * SEE:      "FIXES_OnFilterScriptExit".
 * AUTHOR:   Y_Less
 * 
 * FIX:      GameText
 * PROBLEM:  Several styles do not display for the time specified.
 * SOLUTION: Recreate the styles in Text Draws and use those instead.
 * SEE:      "FIXES_GameTextForAll", "FIXES_GameTextForPlayer".
 * AUTHOR:   Y_Less
 * 
 * FIX:      GameTextStyles
 * PROBLEM:  San Andreas has fixed styles for area and vehicle names, but they
 *           are not included in the GameText styles list.
 * SOLUTION: Add them.  DEFAULTS TO FALSE.
 * SEE:      "FIXES_GameTextForAll", "FIXES_GameTextForPlayer".
 * AUTHOR:   Y_Less
 * 
 * FIX:      HideGameText
 * PROBLEM:  There is no "HideGameTextForXYZ" function.
 * SOLUTION: Show a single space for a game text.  DEFAULTS TO FALSE.
 * SEE:      "FIXES_HideGameTextForAll", "FIXES_HideGameTextForPlayer".
 * AUTHOR:   Y_Less
 * 
 * FIX:      BODYPARTS
 * PROBLEM:  The bodyparts to be used in OnPlayer(Take/Give)Damage are not
 *           defined by default
 * SOLUTION: Define it.
 * SEE:      N/A.
 * AUTHOR:   Whitetiger
 * 
 * FIX:      CAMERAMODES
 * PROBLEM:  The camera modes for GetPlayerCameraMode are not defined by
 *           default.
 * SOLUTION: Define it.
 * SEE:      N/A.
 * AUTHOR:   Whitetiger
 * POST:     http://forum.sa-mp.com/showpost.php?p=1309730
 * 
 * FIX:      SetPlayerCamera
 * PROBLEM:  Using the camera functions directly after enabling spectator mode
 *           doesn't work.
 * SOLUTION: Defer them.
 * SEE:      "FIXES_SetPlayerCameraPos", "FIXES_SetPlayerCameraLookAt",
 *           "FIXES_TogglePlayerSpectating".
 * AUTHOR:   Emmet_
 * 
 * FIX:      SetPlayerTime
 * PROBLEM:  Using this function under "OnPlayerConnect" doesn't work.
 * SOLUTION: Defer it.
 * SEE:      "FIXES_SetPlayerTime"
 * AUTHOR:   Emmet_
 * 
 * FIX:      OnPlayerRequestClass
 * PROBLEM:  Random blunts and bottles sometimes appear in class selection.
 * SOLUTION: Call "RemoveBuildingForPlayer".
 * SEE:      "OnPlayerRequestClass"
 * AUTHOR:   Y_Less
 * 
 * FIX:      SetPlayerColor
 * PROBLEM:  If used under OnPlayerConnect, the affecting player will not
 *           see the color in the TAB menu.
 * SOLUTION: Defer it.
 * SEE:      "FIXES_SetPlayerColor"
 * AUTHOR:   Emmet_
 * POST:     http://forum.sa-mp.com/showthread.php?t=452407  
 * 
 * FIX:      FileMaths
 * PROBLEM:  You can write gibberish like "File:a; ++a;".
 * SOLUTION: Remove the operators.
 * SEE:      "File:operator"
 * AUTHOR:   Y_Less
 * 
 * FIX:      GetPlayerWeaponData
 * PROBLEM:  Old weapons with no ammo left are still returned.
 * SOLUTION: Set "weapons" to 0.  DEFAULTS TO FALSE!
 * SEE:      "FIXES_GetPlayerWeaponData"
 * AUTHOR:   Y_Less
 * POST:     http://forum.sa-mp.com/showthread.php?t=567400
 * 
 * ==============
 *  STYLE RULES: 
 * ==============
 * 
 * All globals should be "static stock" whenever possible (so they can only be
 *     accessed from this one file).
 * 
 * Statics must start with "FIXES_gs", and all other globals with "FIXES_g".
 * 
 * All functions not overriding existing functions must start with "FIXES_".
 * 
 * Macros must be upper case, use underscores, and start "FIXES_":
 *     "FIXES_LIKE_THIS".
 * 
 * Functions should be upper camel case (as the original functions are)
 *     "FIXES_LikeThis".
 * 
 * Globals (after the prefix) should be upper camel case "LikeThis", and locals
 *     lower camel case "likeThis".
 * 
 * ALS should be used to hook functions and callbacks.  See this topic for more
 *    details:
 *    
 *        http://forum.sa-mp.com/showthread.php?t=441293
 * 
 * The ALS prefix for chaining is "FIXES_".
 * 
 * When redefining a native, add a "BAD_" external name declaration with the
 *     "_ALS_" definition so that others may use the original native if they so
 *     desire (with the caveat that it may break all fixes).  Note the "BAD_"
 *     name is meant to indicate the possibility of breaking the fix, not a
 *     comment on the original native function.
 * 
 * The ALS hook defines used here are a little different to the normal ones as
 *     this file assumes that it is always first.  The pattern is:
 *     
 *         #if defined _ALS_NameOfFixHere
 *             #error _ALS_NameOfFixHere defined
 *         #endif
 *         native BAD_NameOfFixHere(params) = NameOfFixHere;
 *         
 *         #if FIX_NameOfFixHere
 *             stock FIXES_NameOfFixHere(params)
 *             {
 *                 return 0;
 *             }
 *             
 *             #define _ALS_NameOfFixHere
 *             #define NameOfFixHere FIXES_NameOfFixHere
 *         #endif
 *    
 *    A copyable version of this pattern is at the end of the file.
 * 
 * Enums start with "E_" or "e_" depending on type, then follow rules for
 *     macros.
 * 
 * NO libraries should be included - not even the default SA:MP ones.  Let the
 *     user do it.
 * 
 * Due to the above rule, you cannot assume any third party libraries AT ALL, so
 *     do not use them.  This can lead to some code duplication, but also means
 *     that the version of the code used here can be tailored for optimisations.
 * 
 * Certain terms may be shortened when dealing with long callback names to avoid
 *     compile truncation warnings (max symbol length is 31).  Current list:
 *     
 *         "Checkpoint" -> "CP"
 *         "Update"     -> "Upd"
 *         "TextDraw"   -> "TD"
 *         "Object"     -> "Obj"
 *         "Player"     -> "P"
 * 
 * Document all fixes at the top of the file, and highlight code.
 * 
 * 4 space TABS - do not edit this file in PAWNO unless you know how to correct
 *     the indentation.
 * 
 * All rules have exceptions, but they must be justifiable.  For example
 *     "IS_FILTERSCRIPT" is a global variable, but is not called
 *     "FIXES_gIsFilterscript" to better match the "FILTERSCRIPT" macro it
 *     replaces.  Now a macro for "_FIXES_gIsFilterscript".
 * 
 * Variables which need to be fully global (i.e. not "static"), but should not
 *     actually be used by other people (e.g. appear inside a macro) should be
 *     prefixed with "_FIXES" instead of "FIXES" to indicate their private use.
 * 
 * No comments beyond the end of column 80 (where the line in "PAWNO" is).
 * 
 * If a bug is fixed in some version of the server it can be conditionally
 *     included here.  This is done by checking for the existance of a native
 *     function introduced in the same server version.  For example
 *     "TogglePlayerControllable" was fixed in 0.3eRC6, the same time as the
 *     "SetObjectMaterial" native was introduced, thus the inclusion becomes:
 *     
 *         #if !defined FIX_TogglePlayerControllable
 *             #if defined SetObjectMaterial
 *                 #define FIX_TogglePlayerControllable (0)
 *             #else
 *                 #define FIX_TogglePlayerControllable (1)
 *             #endif
 *         #elseif _FIXES_IS_UNSET(FIX_TogglePlayerControllable)
 *             #undef FIX_TogglePlayerControllable
 *             #define FIX_TogglePlayerControllable     (2)
 *         #endif
 *     
 *     This only includes this fix if that native doesn't exist.  A copyable
 *     version of this pattern is at the end of the file.
 * 
 * To reduce general memory consumption, strings in this include are stored
 *     globally in constant arrays and referenced.  This is EXACTLY as fast as
 *     using the string constants directly, but means that strings are not
 *     stored in the assembly multiple times (unless the string is only used
 *     once, in which case it's more work for no gain).  See this post for more
 *     details:
 *     
 *         http://forum.sa-mp.com/showpost.php?p=1795601
 * 
 * DO NOT EDIT THIS FILE IN PAWNO OR ON GITHUB.  Both mess up the spacing -
 *     GitHub in an almost irreversible way (except for the fact that nothing is
 *     irreversible in source control.  Pawno spacing is less tricky to solve,
 *     and can be worked around if you know when it uses spaces and tabs - in
 *     short, always write code first THEN indent and you won't have a problem
 *     (in that editor at least, that is normally a bad way to write code).
 * 
 * Documentation explanation:
 *     
 *         FIX:      <Short name>
 *         PROBLEM:  <Description of problem>
 *         SOLUTION: <Description of solution>
 *         SEE:      <List of relevant functions>
 *         AUTHOR:   <Person who wrote the fix>
 *         POST:     <Optional link to the original post where applicable>
 *         FIXED IN: <Optional server version of official fix where applicable>
 * 
 */

#if defined _INC_SAMP_Community_fixes
	#endinput
#endif
#define _INC_SAMP_Community_fixes

#if !defined _inc_fixes
	#define _inc_fixes
#endif

#define _FIXES_IS_UNSET(%0) ((2*%0-1+1)==-1)

// We can add server version compiler code here to only compile fixes that apply
// to the version of the includes for which the user is compiling.
#if !defined FIX_GetPlayerColour
	#if defined FIX_GetPlayerColor
		#if _FIXES_IS_UNSET(FIX_GetPlayerColor)
			#define FIX_GetPlayerColour   (2)
		#else
			#define FIX_GetPlayerColour   (FIX_GetPlayerColor)
		#endif
	#else
		#define FIX_GetPlayerColour       (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_GetPlayerColour)
	#undef FIX_GetPlayerColour
	#define FIX_GetPlayerColour           (2)
#endif

#if !defined FIX_FILTERSCRIPT
	#define FIX_FILTERSCRIPT             (1)
#elseif _FIXES_IS_UNSET(FIX_FILTERSCRIPT)
	#undef FIX_FILTERSCRIPT
	#define FIX_FILTERSCRIPT             (2)
#endif

#if !defined FIX_SpawnPlayer
	#define FIX_SpawnPlayer              (1)
#elseif _FIXES_IS_UNSET(FIX_SpawnPlayer)
	#undef FIX_SpawnPlayer
	#define FIX_SpawnPlayer              (2)
#endif

#if !defined FIX_SetPlayerName
	#define FIX_SetPlayerName            (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerName)
	#undef FIX_SetPlayerName
	#define FIX_SetPlayerName            (2)
#endif

#if !defined FIX_GetPlayerSkin
	#define FIX_GetPlayerSkin            (1)
#elseif _FIXES_IS_UNSET(FIX_GetPlayerSkin)
	#undef FIX_GetPlayerSkin
	#define FIX_GetPlayerSkin            (2)
#endif

#if !defined FIX_GetWeaponName
	#define FIX_GetWeaponName            (1)
#elseif _FIXES_IS_UNSET(FIX_GetWeaponName)
	#undef FIX_GetWeaponName
	#define FIX_GetWeaponName            (2)
#endif

#if !defined FIX_SetPlayerWorldBounds
	#define FIX_SetPlayerWorldBounds     (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerWorldBounds)
	#undef FIX_SetPlayerWorldBounds
	#define FIX_SetPlayerWorldBounds     (2)
#endif

#if !defined FIX_TogglePlayerControllable
	#if defined SetObjectMaterial
		#define FIX_TogglePlayerControllable (0)
	#else
		#define FIX_TogglePlayerControllable (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_TogglePlayerControllable)
	#undef FIX_TogglePlayerControllable
	#define FIX_TogglePlayerControllable (2)
#endif

#if !defined FIX_HydraSniper
	#define FIX_HydraSniper              (1)
#elseif _FIXES_IS_UNSET(FIX_HydraSniper)
	#undef FIX_HydraSniper
	#define FIX_HydraSniper              (2)
#endif

#if !defined FIX_IsPlayerInCheckpoint
	#define FIX_IsPlayerInCheckpoint     (1)
#elseif _FIXES_IS_UNSET(FIX_IsPlayerInCheckpoint)
	#undef FIX_IsPlayerInCheckpoint
	#define FIX_IsPlayerInCheckpoint     (2)
#endif

#if !defined FIX_IsPlayerInRaceCheckpoint
	#define FIX_IsPlayerInRaceCheckpoint (1)
#elseif _FIXES_IS_UNSET(FIX_IsPlayerInRaceCheckpoint)
	#undef FIX_IsPlayerInRaceCheckpoint
	#define FIX_IsPlayerInRaceCheckpoint (2)
#endif

#if !defined FIX_GetPlayerWeapon
	#define FIX_GetPlayerWeapon          (1)
#elseif _FIXES_IS_UNSET(FIX_GetPlayerWeapon)
	#undef FIX_GetPlayerWeapon
	#define FIX_GetPlayerWeapon          (2)
#endif

#if !defined FIX_PutPlayerInVehicle
	#define FIX_PutPlayerInVehicle       (1)
#elseif _FIXES_IS_UNSET(FIX_PutPlayerInVehicle)
	#undef FIX_PutPlayerInVehicle
	#define FIX_PutPlayerInVehicle       (2)
#endif

#if !defined FIX_KEY_AIM
	#if defined KEY_AIM
		#define FIX_KEY_AIM              (0)
	#else
		#define FIX_KEY_AIM              (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_KEY_AIM)
	#undef FIX_KEY_AIM
	#define FIX_KEY_AIM                  (2)
#endif

#if !defined FIX_SPECIAL_ACTION_PISSING
	#if defined SPECIAL_ACTION_PISSING
		#define FIX_SPECIAL_ACTION_PISSING (0)
	#else
		#define FIX_SPECIAL_ACTION_PISSING (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_SPECIAL_ACTION_PISSING)
	#undef FIX_SPECIAL_ACTION_PISSING
	#define FIX_SPECIAL_ACTION_PISSING   (2)
#endif

#if !defined FIX_Natives
	#define FIX_Natives                  (1)
#elseif _FIXES_IS_UNSET(FIX_Natives)
	#undef FIX_Natives
	#define FIX_Natives                  (2)
#endif

#if !defined FIX_IsValidVehicle
	#if defined IsValidVehicle
		#define FIX_IsValidVehicle       (0)
	#else
		#define FIX_IsValidVehicle       (FIX_Natives)
	#endif
#elseif _FIXES_IS_UNSET(FIX_IsValidVehicle)
	#undef FIX_IsValidVehicle
	#define FIX_IsValidVehicle           (2)
#endif

#if !defined FIX_GetGravity
	#if defined GetGravity
		#define FIX_GetGravity           (0)
	#else
		#define FIX_GetGravity           (FIX_Natives)
	#endif
#elseif _FIXES_IS_UNSET(FIX_GetGravity)
	#undef FIX_GetGravity
	#define FIX_GetGravity               (2)
#endif

#if !defined FIX_gpci
	#if defined gpci
		#define FIX_gpci                 (0)
	#else
		#define FIX_gpci                 (FIX_Natives)
	#endif
#elseif _FIXES_IS_UNSET(FIX_gpci)
	#undef FIX_gpci
	#define FIX_gpci                     (2)
#endif

#if !defined FIX_BODYPARTS
	#if defined BODY_PART_TORSO
		#define FIX_BODYPARTS            (0)
	#else
		#define FIX_BODYPARTS            (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_BODYPARTS)
	#undef FIX_BODYPARTS
	#define FIX_BODYPARTS                (2)
#endif

#if !defined FIX_CAMERAMODES
	#if defined CAM_MODE_NONE
		#define FIX_CAMERAMODES          (0)
	#else
		#define FIX_CAMERAMODES          (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_CAMERAMODES)
	#undef FIX_CAMERAMODES
	#define FIX_CAMERAMODES              (2)
#endif

#if !defined FIX_DriveBy
	#define FIX_DriveBy                  (1)
#elseif _FIXES_IS_UNSET(FIX_DriveBy)
	#undef FIX_DriveBy
	#define FIX_DriveBy                  (2)
#endif

#if !defined FIX_SetPlayerCheckpoint
	#define FIX_SetPlayerCheckpoint      (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerCheckpoint)
	#undef FIX_SetPlayerCheckpoint
	#define FIX_SetPlayerCheckpoint      (2)
#endif

#if !defined FIX_SetPlayerRaceCheckpoint
	#define FIX_SetPlayerRaceCheckpoint  (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerRaceCheckpoint)
	#undef FIX_SetPlayerRaceCheckpoint
	#define FIX_SetPlayerRaceCheckpoint  (2)
#endif

#if !defined FIX_TextDrawCreate
	#define FIX_TextDrawCreate           (1)
#elseif _FIXES_IS_UNSET(FIX_TextDrawCreate)
	#undef FIX_TextDrawCreate
	#define FIX_TextDrawCreate           (2)
#endif

#if !defined FIX_TextDrawSetString
	#define FIX_TextDrawSetString        (1)
#elseif _FIXES_IS_UNSET(FIX_TextDrawSetString)
	#undef FIX_TextDrawSetString
	#define FIX_TextDrawSetString        (2)
#endif

#if !defined FIX_AllowInteriorWeapons
	#define FIX_AllowInteriorWeapons     (1)
#elseif _FIXES_IS_UNSET(FIX_AllowInteriorWeapons)
	#undef FIX_AllowInteriorWeapons
	#define FIX_AllowInteriorWeapons     (2)
#endif

#if !defined FIX_OnPlayerEnterVehicle
	#if defined OnPlayerClickMap
		#define FIX_OnPlayerEnterVehicle (0)
	#else
		#define FIX_OnPlayerEnterVehicle (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_OnPlayerEnterVehicle)
	#undef FIX_OnPlayerEnterVehicle
	#define FIX_OnPlayerEnterVehicle     (2)
#endif

#if !defined FIX_OnPlayerEnterVehicle_2
	#define FIX_OnPlayerEnterVehicle_2   (1)
#elseif _FIXES_IS_UNSET(FIX_OnPlayerEnterVehicle)
	#undef FIX_OnPlayerEnterVehicle_2
	#define FIX_OnPlayerEnterVehicle_2   (2)
#endif

#if !defined FIX_AllowTeleport
	#if defined OnPlayerClickMap
		#define FIX_AllowTeleport        (1)
	#else
		#define FIX_AllowTeleport        (0)
	#endif
#elseif _FIXES_IS_UNSET(FIX_AllowTeleport)
	#undef FIX_AllowTeleport
	#define FIX_AllowTeleport            (2)
#endif

#if !defined FIX_SetPlayerSpecialAction
	#define FIX_SetPlayerSpecialAction   (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerSpecialAction)
	#undef FIX_SetPlayerSpecialAction
	#define FIX_SetPlayerSpecialAction   (2)
#endif

#if !defined FIX_ClearAnimations
	#define FIX_ClearAnimations          (1)
#elseif _FIXES_IS_UNSET(FIX_ClearAnimations)
	#undef FIX_ClearAnimations
	#define FIX_ClearAnimations          (2)
#endif

#if !defined FIX_ClearAnimations_2
	#define FIX_ClearAnimations_2        (1)
#elseif _FIXES_IS_UNSET(FIX_ClearAnimations_2)
	#undef FIX_ClearAnimations_2
	#define FIX_ClearAnimations_2        (2)
#endif

#if !defined FIX_GangZoneCreate
	#define FIX_GangZoneCreate           (1)
#elseif _FIXES_IS_UNSET(FIX_GangZoneCreate)
	#undef FIX_GangZoneCreate
	#define FIX_GangZoneCreate           (2)
#endif

#if !defined FIX_OnDialogResponse
	#if defined GetVehicleModelInfo
		#define FIX_OnDialogResponse     (0)
	#else
		#define FIX_OnDialogResponse     (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_OnDialogResponse)
	#undef FIX_OnDialogResponse
	#define FIX_OnDialogResponse         (2)
#endif

#if !defined FIX_GetPlayerDialog
	#define FIX_GetPlayerDialog          (0)
#elseif _FIXES_IS_UNSET(FIX_GetPlayerDialog)
	#undef FIX_GetPlayerDialog
	#define FIX_GetPlayerDialog          (2)
#endif

#if !defined FIX_PlayerDialogResponse
	#define FIX_PlayerDialogResponse     (1)
#elseif _FIXES_IS_UNSET(FIX_PlayerDialogResponse)
	#undef FIX_PlayerDialogResponse
	#define FIX_PlayerDialogResponse     (2)
#endif

#if !defined FIX_SetSpawnInfo
	#if defined GetPlayerVersion
		#define FIX_SetSpawnInfo         (0)
	#else
		#define FIX_SetSpawnInfo         (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_SetSpawnInfo)
	#undef FIX_SetSpawnInfo
	#define FIX_SetSpawnInfo             (2)
#endif

#if !defined FIX_SetPlayerSkin
	#define FIX_SetPlayerSkin            (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerSkin)
	#undef FIX_SetPlayerSkin
	#define FIX_SetPlayerSkin            (2)
#endif

#if !defined FIX_HideMenuForPlayer
	#define FIX_HideMenuForPlayer        (1)
#elseif _FIXES_IS_UNSET(FIX_HideMenuForPlayer)
	#undef FIX_HideMenuForPlayer
	#define FIX_HideMenuForPlayer        (2)
#endif

#if !defined FIX_valstr
	#define FIX_valstr                   (1)
#elseif _FIXES_IS_UNSET(FIX_valstr)
	#undef FIX_valstr
	#define FIX_valstr                   (2)
#endif

#if !defined FIX_file_inc
	#define FIX_file_inc                 (0)
#elseif _FIXES_IS_UNSET(FIX_file_inc)
	#undef FIX_file_inc
	#define FIX_file_inc                 (2)
#endif

#if !defined FIX_fclose
	#define FIX_fclose                   (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fclose)
	#undef FIX_fclose
	#define FIX_fclose                   (2)
#endif

#if !defined FIX_fwrite
	#define FIX_fwrite                   (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fwrite)
	#undef FIX_fwrite
	#define FIX_fwrite                   (2)
#endif

#if !defined FIX_fread
	#define FIX_fread                    (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fread)
	#undef FIX_fread
	#define FIX_fread                    (2)
#endif

#if !defined FIX_fputchar
	#define FIX_fputchar                 (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fputchar)
	#undef FIX_fputchar
	#define FIX_fputchar                 (2)
#endif

#if !defined FIX_fgetchar
	#define FIX_fgetchar                 (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fgetchar)
	#undef FIX_fgetchar
	#define FIX_fgetchar                 (2)
#endif

#if !defined FIX_fblockwrite
	#define FIX_fblockwrite              (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fblockwrite)
	#undef FIX_fblockwrite
	#define FIX_fblockwrite              (2)
#endif

#if !defined FIX_fblockread
	#define FIX_fblockread               (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fblockread)
	#undef FIX_fblockread
	#define FIX_fblockread               (2)
#endif

#if !defined FIX_fseek
	#define FIX_fseek                    (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_fseek)
	#undef FIX_fseek
	#define FIX_fseek                    (2)
#endif

#if !defined FIX_flength
	#define FIX_flength                  (FIX_file_inc)
#elseif _FIXES_IS_UNSET(FIX_flength)
	#undef FIX_flength
	#define FIX_flength                  (2)
#endif

#if !defined FIX_IsPlayerAttachedObjSlotUsed
	#define FIX_IsPlayerAttachedObjSlotUsed (1)
#elseif _FIXES_IS_UNSET(FIX_IsPlayerAttachedObjSlotUsed)
	#undef FIX_IsPlayerAttachedObjSlotUsed
	#define FIX_IsPlayerAttachedObjSlotUsed (2)
#endif

#if !defined FIX_SetPlayerAttachedObject
	#define FIX_SetPlayerAttachedObject  (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerAttachedObject)
	#undef FIX_SetPlayerAttachedObject
	#define FIX_SetPlayerAttachedObject  (2)
#endif

#if !defined FIX_OnPlayerDeath
	#define FIX_OnPlayerDeath            (1)
#elseif _FIXES_IS_UNSET(FIX_OnPlayerDeath)
	#undef FIX_OnPlayerDeath
	#define FIX_OnPlayerDeath            (2)
#endif

#if !defined FIX_strins
	#define FIX_strins                   (1)
#elseif _FIXES_IS_UNSET(FIX_strins)
	#undef FIX_strins
	#define FIX_strins                   (2)
#endif

#if !defined FIX_IsPlayerConnected
	#define FIX_IsPlayerConnected        (1)
#elseif _FIXES_IS_UNSET(FIX_IsPlayerConnected)
	#undef FIX_IsPlayerConnected
	#define FIX_IsPlayerConnected        (2)
#endif

#if !defined FIX_OnPlayerCommandText
	#define FIX_OnPlayerCommandText      (1)
#elseif _FIXES_IS_UNSET(FIX_OnPlayerCommandText)
	#undef FIX_OnPlayerCommandText
	#define FIX_OnPlayerCommandText      (2)
#endif

#if !defined FIX_TrainExit
	#define FIX_TrainExit                (1)
#elseif _FIXES_IS_UNSET(FIX_TrainExit)
	#undef FIX_TrainExit
	#define FIX_TrainExit                (2)
#endif

#if !defined FIX_Kick
	#if defined EnableVehicleFriendlyFire
		#define FIX_Kick                 (0)
	#else
		#define FIX_Kick                 (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_Kick)
	#undef FIX_Kick
	#define FIX_Kick                     (2)
#endif

#if !defined FIX_OnVehicleMod
	#define FIX_OnVehicleMod             (1)
#elseif _FIXES_IS_UNSET(FIX_OnVehicleMod)
	#undef FIX_OnVehicleMod
	#define FIX_OnVehicleMod             (2)
#endif

#if !defined FIX_random
	#define FIX_random                   (0)
#elseif _FIXES_IS_UNSET(FIX_random)
	#undef FIX_random
	#define FIX_random                   (2)
#endif

#if !defined FIX_sleep
	#define FIX_sleep                    (1)
#elseif _FIXES_IS_UNSET(FIX_sleep)
	#undef FIX_sleep
	#define FIX_sleep                    (2)
#endif

#if !defined FIX_Menus
	#define FIX_Menus                    (1)
#elseif _FIXES_IS_UNSET(FIX_Menus)
	#undef FIX_Menus
	#define FIX_Menus                    (2)
#endif

#if !defined FIX_AddMenuItem
	#define FIX_AddMenuItem              (FIX_Menus)
#elseif _FIXES_IS_UNSET(FIX_AddMenuItem)
	#undef FIX_AddMenuItem
	#define FIX_AddMenuItem              (2)
#endif

#if !defined FIX_SetMenuColumnHeader
	#define FIX_SetMenuColumnHeader      (FIX_Menus)
#elseif _FIXES_IS_UNSET(FIX_SetMenuColumnHeader)
	#undef FIX_SetMenuColumnHeader
	#define FIX_SetMenuColumnHeader      (2)
#endif

#if !defined FIX_ShowMenuForPlayer
	#define FIX_ShowMenuForPlayer        (FIX_Menus)
#elseif _FIXES_IS_UNSET(FIX_ShowMenuForPlayer)
	#undef FIX_ShowMenuForPlayer
	#define FIX_ShowMenuForPlayer        (2)
#endif

#if !defined FIX_HideMenuForPlayer
	#define FIX_HideMenuForPlayer        (FIX_Menus)
#elseif _FIXES_IS_UNSET(FIX_HideMenuForPlayer)
	#undef FIX_HideMenuForPlayer
	#define FIX_HideMenuForPlayer        (2)
#endif

#if !defined FIX_GetPlayerMenu
	#define FIX_GetPlayerMenu            (1)
#elseif _FIXES_IS_UNSET(FIX_GetPlayerMenu)
	#undef FIX_GetPlayerMenu
	#define FIX_GetPlayerMenu            (2)
#endif

#if !defined FIX_HideMenuForPlayer_2
	#define FIX_HideMenuForPlayer_2      (0)
#elseif _FIXES_IS_UNSET(FIX_HideMenuForPlayer_2)
	#undef FIX_HideMenuForPlayer_2
	#define FIX_HideMenuForPlayer_2      (2)
#elseif FIX_HideMenuForPlayer_2 && !FIX_GetPlayerMenu
	#error FIX_HideMenuForPlayer_2 requires FIX_GetPlayerMenu
#endif

#if !defined FIX_DisableMenu
	#define FIX_DisableMenu              (FIX_Menus)
#elseif _FIXES_IS_UNSET(FIX_DisableMenu)
	#undef FIX_DisableMenu
	#define FIX_DisableMenu              (2)
#endif

#if !defined FIX_DisableMenuRow
	#define FIX_DisableMenuRow           (FIX_Menus)
#elseif _FIXES_IS_UNSET(FIX_DisableMenuRow)
	#undef FIX_DisableMenuRow
	#define FIX_DisableMenuRow           (2)
#endif

#if FIX_Menus || FIX_AddMenuItem || FIX_SetMenuColumnHeader || FIX_ShowMenuForPlayer || FIX_HideMenuForPlayer || FIX_DisableMenu || FIX_DisableMenuRow
	#define _FIX_Menus                   (1)
#else
	#define _FIX_Menus                   (2)
#endif

#if !defined FIX_GetPlayerInterior
	#define FIX_GetPlayerInterior        (1)
#elseif _FIXES_IS_UNSET(FIX_GetPlayerInterior)
	#undef FIX_GetPlayerInterior
	#define FIX_GetPlayerInterior        (2)
#endif

#if !defined FIX_ApplyAnimation
	#define FIX_ApplyAnimation           (1)
#elseif _FIXES_IS_UNSET(FIX_ApplyAnimation)
	#undef FIX_ApplyAnimation
	#define FIX_ApplyAnimation           (2)
#endif

#if !defined FIX_ApplyAnimation_2
	#define FIX_ApplyAnimation_2         (1)
#elseif _FIXES_IS_UNSET(FIX_ApplyAnimation_2)
	#undef FIX_ApplyAnimation_2
	#define FIX_ApplyAnimation_2         (2)
#endif

#if !defined FIX_OnPlayerSpawn
	#define FIX_OnPlayerSpawn            (1)
#elseif _FIXES_IS_UNSET(FIX_OnPlayerSpawn)
	#undef FIX_OnPlayerSpawn
	#define FIX_OnPlayerSpawn            (2)
#endif

#if !defined FIX_GameText
	#define FIX_GameText                 (1)
#elseif _FIXES_IS_UNSET(FIX_GameText)
	#undef FIX_GameText
	#define FIX_GameText                 (2)
#endif

#if !defined FIX_HideGameText
	#define FIX_HideGameText             (0)
#elseif _FIXES_IS_UNSET(FIX_HideGameText)
	#undef FIX_HideGameText
	#define FIX_HideGameText             (2)
#endif

#if !defined FIX_GameTextStyles
	#define FIX_GameTextStyles           (0)
#elseif _FIXES_IS_UNSET(FIX_GameTextStyles)
	#undef FIX_GameTextStyles
	#define FIX_GameTextStyles           (2)
#elseif FIX_GameTextStyles && !FIX_GameText
	#error FIX_GameTextStyles requires FIX_GameText
#endif

#if !defined FIX_OnPlayerConnect
	#define FIX_OnPlayerConnect          (1)
#elseif _FIXES_IS_UNSET(FIX_OnPlayerConnect)
	#undef FIX_OnPlayerConnect
	#define FIX_OnPlayerConnect          (2)
#endif

#if !defined FIX_OnPlayerDisconnect
	#define FIX_OnPlayerDisconnect       (1)
#elseif _FIXES_IS_UNSET(FIX_OnPlayerDisconnect)
	#undef FIX_OnPlayerDisconnect
	#define FIX_OnPlayerDisconnect       (2)
#endif

#if !defined FIX_CreatePlayerTextDraw
	#define FIX_CreatePlayerTextDraw     (1)
#elseif _FIXES_IS_UNSET(FIX_CreatePlayerTextDraw)
	#undef FIX_CreatePlayerTextDraw
	#define FIX_CreatePlayerTextDraw     (2)
#endif

#if !defined FIX_PlayerTextDrawSetString
	#define FIX_PlayerTextDrawSetString  (1)
#elseif _FIXES_IS_UNSET(FIX_PlayerTextDrawSetString)
	#undef FIX_PlayerTextDrawSetString
	#define FIX_PlayerTextDrawSetString  (2)
#endif

#if !defined FIX_SetPlayerCamera
	#define FIX_SetPlayerCamera          (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerCamera)
	#undef FIX_SetPlayerCamera
	#define FIX_SetPlayerCamera          (2)
#endif

#if !defined FIX_SetPlayerTime
	#define FIX_SetPlayerTime            (1)
#elseif _FIXES_IS_UNSET(FIX_SetPlayerTime)
	#undef FIX_SetPlayerTime
	#define FIX_SetPlayerTime            (2)
#endif

#if !defined FIX_OnPlayerRequestClass
	#define FIX_OnPlayerRequestClass     (1)
#elseif _FIXES_IS_UNSET(FIX_OnPlayerRequestClass)
	#undef FIX_OnPlayerRequestClass
	#define FIX_OnPlayerRequestClass     (2)
#endif

#if !defined FIX_SetPlayerColour
	#if defined FIX_SetPlayerColor
		#if _FIXES_IS_UNSET(FIX_SetPlayerColor)
			#define FIX_SetPlayerColour  (2)
		#else
			#define FIX_SetPlayerColour   (FIX_SetPlayerColor)
		#endif
	#else
		#define FIX_SetPlayerColour      (1)
	#endif
#elseif _FIXES_IS_UNSET(FIX_SetPlayerColour)
	#undef FIX_SetPlayerColour
	#define FIX_SetPlayerColour          (2)
#endif

#if !defined FIX_FileMaths
	#define FIX_FileMaths                (1)
#elseif _FIXES_IS_UNSET(FIX_FileMaths)
	#undef FIX_FileMaths
	#define FIX_FileMaths                (2)
#endif

#if !defined FIX_GetPlayerWeaponData
	#define FIX_GetPlayerWeaponData      (0)
#elseif _FIXES_IS_UNSET(FIX_GetPlayerWeaponData)
	#undef FIX_GetPlayerWeaponData
	#define FIX_GetPlayerWeaponData      (2)
#endif

/* 
 * CHAIN_ORDER
 * 
 * Allows y_hooks to call things in the correct order when it exists.
 */

#if !defined CHAIN_ORDER
	#define CHAIN_ORDER() 0
#endif

/* 
 * PRE_HOOK
 * 
 * A method of pre-hooking callbacks to still have them called before any ones
 * re-written by y_hooks.
 */

#define PRE_HOOK(%0) forward @CO_%0();public @CO_%0(){return CHAIN_ORDER()+1;}

PRE_HOOK(FIXES)
#undef CHAIN_ORDER
#define CHAIN_ORDER @CO_FIXES

/* 
 * _FIXES_IncludeStates
 * 
 * Define the "_ALS" states used by hooks.
 */

static stock _FIXES_IncludeStates() <_ALS : _ALS_x0, _ALS : _ALS_x1, _ALS : _ALS_x2, _ALS : _ALS_x3>
{
}

static stock _FIXES_IncludeStates() <_ALS : _ALS_go>
{
}

/* 
 * FIXES_GT_STYLE_COUNT
 * 
 * The number of GameText styles recreated using TextDraw functions.  This is 4
 * by default, but can be extended to 6 if you want location and vehicle styles.
 */

#if FIX_GameText
	#if FIX_GameTextStyles
		#define FIXES_GT_STYLE_COUNT (14)
	#else
		#define FIXES_GT_STYLE_COUNT (7)
	#endif
#endif

/* 
 * FIXES_SilentKick
 * 
 * If this define is set to 1, then players will not be given a message when
 * they are kicked for cheats, instead they will just loose connection to the
 * server.
 */

#if !defined FIXES_SilentKick
	#define FIXES_SilentKick 0
#elseif _FIXES_IS_UNSET(FIXES_SilentKick)
	#undef FIXES_SilentKick
	#define FIXES_SilentKick 2
#endif

/* 
 * FIXES_Debug
 * 
 * If this define is set to 1, then debug printing is turned on for any
 * functions which may use it.  Otherwise, the compiler entirely removes the
 * code to print anything, leaving 0 run-time overhead.
 */

#if defined FIXES_Debug
	#if _FIXES_IS_UNSET(FIXES_Debug)
		#undef FIXES_Debug
	#elseif FIXES_Debug == 1
		#define FIXES_PRINTF(%0); print(_FIXES_gIsFilterscript ? ("* FIXES_PRINTF (FS):") : ("* FIXES_PRINTF (GM):")),printf("*** " %0);
	#else
		#undef FIXES_Debug
	#endif
#endif

/* 
 * FIXES_UseStateHooks
 * 
 * A marker to indicate that this version of fixes.inc uses state-based hooks.
 */
#define FIXES_UseStateHooks

/* 
 * FIXES_PRINTF
 * 
 * A special "printf" function only compiled when "FIXES_Debug" is set.
 */

#if !defined FIXES_PRINTF
	#define FIXES_Debug 0
	#define FIXES_PRINTF(%0);
#endif

/* 
 * INVALID_DIALOG_ID
 * 
 * Set when a player can't see a dialog.
 */

#if !defined INVALID_DIALOG_ID
	#define INVALID_DIALOG_ID (-1)
#endif

/* 
 * FIXES_Single
 * 
 * If this define is set to 1, then the old style include is used, with no
 * support for multiple scripts running at the same time on the server (i.e. no
 * gamemodes and filter scripts at the same time).  You can only have one or the
 * other or bugs.
 */

#if !defined FIXES_Single
	#define FIXES_Single (1)
#elseif _FIXES_IS_UNSET(FIXES_Single)
	#undef FIXES_Single
	#define FIXES_Single (2)
#endif

/* 
 * _FIXES_IS_IN_CHARGE
 * 
 * Test to see if this script is in command or not (master).
 */

#if FIXES_Single
	#define _FIXES_IS_IN_CHARGE(%0)
#else
	#define _FIXES_IS_IN_CHARGE(%0) if (%0(FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE))
#endif

/* 
 * E_FIXES_WORLDBOUND_DATA
 * 
 * Store data for each player on their worldbound data.
 */
enum E_FIXES_WORLDBOUND_DATA
{
	// "Previous".
	Float:E_FIXES_WORLDBOUND_DATA_PX,
	Float:E_FIXES_WORLDBOUND_DATA_PY,
	Float:E_FIXES_WORLDBOUND_DATA_PZ,
	// "Lower".
	Float:E_FIXES_WORLDBOUND_DATA_LX,
	Float:E_FIXES_WORLDBOUND_DATA_LY,
	// "Upper".
	Float:E_FIXES_WORLDBOUND_DATA_UX,
	Float:E_FIXES_WORLDBOUND_DATA_UY
}

/* 
 * e_FIXES_BOOLS
 * 
 * Collection of boolean values for players, designed to collect multiple checks
 * in to one variable to reduce memory consumption.
 */
enum e_FIXES_BOOLS (<<= 1)
{
	// Handy definition for nothing set.
	e_FIXES_BOOLS_NONE = 0,
	// Does this player have worldbounds enabled?
	e_FIXES_BOOLS_WORLDBOUNDS = 1,
	e_FIXES_BOOLS_UNCONTROLLABLE,
	e_FIXES_BOOLS_PUT_IN_VEHICLE,
	e_FIXES_BOOLS_BLOCK,
	e_FIXES_BOOLS_TELEPORT,
	e_FIXES_BOOLS_CONNECTED,
	e_FIXES_BOOLS_INTERIOR,
	e_FIXES_BOOLS_PUT_IN_TRAIN,
	e_FIXES_BOOLS_KICKED,
	e_FIXES_BOOLS_ON_PLAYER_CONNECT,
	e_FIXES_BOOLS_DRIVE_BY,
	e_FIXES_BOOLS_FIRST_SPAWN,
	e_FIXES_BOOLS_FIRST_CLASS,
	e_FIXES_BOOLS_SPECTATING,
	e_FIXES_BOOLS_CP_DELAYED,
	e_FIXES_BOOLS_RACE_CP_DELAYED
}

/* 
 * e_FIXES_SETTINGS
 * 
 * Collection of boolean values for the script as a whole.  Stores a variety of
 * settings that are true/false.
 */
enum e_FIXES_SETTINGS (<<= 1)
{
	// Handy definition for nothing set.
	e_FIXES_SETTINGS_NONE = 0,
	e_FIXES_SETTINGS_INTERIOR = 1,
	e_FIXES_SETTINGS_ADMIN_TELEPORT,
	e_FIXES_SETTINGS_IN_CHARGE,
	e_FIXES_SETTINGS_DROP_ALL_DATA,
	e_FIXES_SETTINGS_MENU_SET,
	e_FIXES_SETTINGS_ENDING,
	e_FIXES_SETTINGS_ENDED,
	e_FIXES_SETTINGS_NO_GAME_TEXT,
	e_FIXES_SETTINGS_SECOND_USE
}

/* 
 * _FIXES_CEILDIV
 * 
 * Do a ceiling division of the first number by the second number.
 */
#define _FIXES_CEILDIV(%0,%1) (((%0) + (%1) - 1) / (%1))

/* 
 * _FIXES_INFINITY
 * 
 * IEEE 754 definition of infinity.
 */
#define _FIXES_INFINITY (Float:0x7F800000)

/* 
 * _FIXES_N_INFINITY
 * 
 * IEEE 754 definition of negative infinity.
 */
#define _FIXES_N_INFINITY (Float:0xFF800000)

/* 
 * _FIXES_ATTACHMENTS
 * 
 * The number of players whose attachment data (10-bits) can be stored in one
 * cell.
 */
#define _FIXES_ATTACHMENTS (cellbits / MAX_PLAYER_ATTACHED_OBJECTS)

/* 
 * _FIXES_FOREACH
 * 
 * A new version of "foreach", but just for this library.
 */
#define _FIXES_FOREACH(%1,%0) for (new %0 = MAX_PLAYERS; (%0 = %1[%0]) != MAX_PLAYERS; )

/* 
 * _FIXES_IN_RANGE
 * 
 * Checks if the first parameter is between the other two: %1 <= %0 < %2 (but
 * faster).
 */
#define _FIXES_IN_RANGE(%0,%1,%2) (((%0) - ((%1) + cellmin)) < ((%2) - ((%1) + cellmin)))

/* 
 * _FIXES_NO_RANGE
 * 
 * Checks if the first parameter is not between the other two: !(%1 <= %0 < %2)
 * (but faster).
 */
#define _FIXES_NO_RANGE(%0,%1,%2) (((%0) - ((%1) + cellmin)) >= ((%2) - ((%1) + cellmin)))

/* 
 * _FIXES_FORWARD
 * 
 * Forwards a hooked callbacks, and defines the default state implementations.
 */
#define _FIXES_FORWARD%0(%1); \
	forward%0(%1); \
	public%0(%1) <_ALS : _ALS_x0, _ALS : _ALS_x1> { return 1; } \
	public%0(%1) <> { return 1; }

/* 
 * _FIXES_IS_PLAYER_CONNECTED
 * 
 * Always valid "IsPlayerConnected" check.
 */
#define _FIXES_IS_PLAYER_CONNECTED(%0) (_FIXES_IN_RANGE((%0), 0, MAX_PLAYERS) && IsPlayerConnected((%0)))

// These varaibles are NOT pre-processor dependent as they are stock.  It's just
// simpler than trying to figure out when or if a semi-colon is needed.
#if !FIX_FILTERSCRIPT
static
#endif
stock
	/* 
	 * bool:_FIXES_gIsFilterscript
	 * 
	 * Runtime equivalent of "FILTERSCRIPT" for when it is not set by the user.
	 */
	bool:_FIXES_gIsFilterscript;

stock
	/* 
	 * FIXES_gscSpace[]
	 * 
	 * A single re-usable space.
	 */
	FIXES_gscSpace[] = " ";

#if FIXES_Debug
new
#else
static stock
#endif
	/* 
	 * FIXES_gsValidMenus[_FIXES_CEILDIV(MAX_MENUS, cellbits)]
	 * 
	 * A record of which menus have and haven't been shown yet.  We ensure that
	 * this only exists when required, since it depends on add and remove
	 * functions to be called at the right time.
	 */
#if _FIX_Menus || FIX_GetPlayerMenu || FIX_OnPlayerDisconnect || FIX_GameText || FIX_AllowInteriorWeapons || FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable
	FIXES_gsPlayersIterator[MAX_PLAYERS + 1] = {0, 1, ...},
#endif
	/* 
	 * FIXES_gsValidMenus[_FIXES_CEILDIV(MAX_MENUS, cellbits)]
	 * 
	 * A record of which menus have and haven't been shown yet.
	 */
	FIXES_gsValidMenus[_FIXES_CEILDIV(MAX_MENUS, cellbits)],
	/* 
	 * FIXES_gsPlayerIP[MAX_PLAYERS]
	 * 
	 * A player's IP as a 32-bit integer.
	 */
	FIXES_gsPlayerIP[MAX_PLAYERS] = {-1, ...},
	/* 
	 * FIXES_gsPlayerSkin[MAX_PLAYERS]
	 * 
	 * The skin the player is currently using.
	 */
	FIXES_gsPlayerSkin[MAX_PLAYERS],
	/* 
	 * e_FIXES_BOOLEAN_DATA:FIXES_gsPlayerBools[MAX_PLAYERS]
	 * 
	 * Collection of boolean values for players.
	 */
	e_FIXES_BOOLS:FIXES_gsPlayerBools[MAX_PLAYERS],
	/* 
	 * FIXES_gsWorldbounds[MAX_PLAYERS][E_FIXES_WORLDBOUND_DATA]
	 * 
	 * All data for players on where their worldbounds are and where they last
	 * were before they went through the bounds by crouching.
	 */
#if FIX_SetPlayerWorldBounds
	FIXES_gsWorldbounds[MAX_PLAYERS][E_FIXES_WORLDBOUND_DATA],
#endif
	/* 
	 * FIXES_gsPlayerWeapon[MAX_PLAYERS]
	 * 
	 * Stores the weapon set by "SetPlayerArmedWeapon" when in a vehicle.
	 */
	FIXES_gsPlayerWeapon[MAX_PLAYERS],
	/* 
	 * FIXES_gsVehicleSeatData[MAX_PLAYERS]
	 * 
	 * What vehicle the player is due to go in.
	 */
	FIXES_gsVehicleSeatData[MAX_PLAYERS],
	/* 
	 * FIXES_gsVehicleLocked[(MAX_PLAYERS * MAX_VEHICLES + cellbits - 1) / cellbits]
	 * 
	 * Is this vehicle locked for a player.  This is a compressed 2D binary
	 * array, made by concatenating many bits together (saves 1kb-3kb)!  Is
	 * there a reason this array exists?
	 */
	//FIXES_gsVehicleLocked[_FIXES_CEILDIV(MAX_PLAYERS * MAX_VEHICLES, cellbits)],
	/* 
	 * FIXES_gsDialogID[MAX_PLAYERS]
	 * 
	 * Stores the true ID of the dialog the player is looking at to prevent
	 * spoofing.
	 */
	FIXES_gsDialogID[MAX_PLAYERS] = {-1, ...},
	/* 
	 * FIXES_gsInterior[MAX_PLAYERS]
	 * 
	 * The player's current interior.
	 */
	FIXES_gsInterior[MAX_PLAYERS],
	/* 
	 * e_FIXES_SETTINGS:FIXES_gsSettings
	 * 
	 * A collection of 1-bit options, compressed together to save space.
	 */
	e_FIXES_SETTINGS:FIXES_gsSettings = e_FIXES_SETTINGS_SECOND_USE,
	/* 
	 * FIXES_gsObjectSlots[_FIXES_CEILDIV(MAX_PLAYERS, _FIXES_ATTACHMENTS)]
	 * 
	 * A record of which attached object slots a player has used.
	 */
	FIXES_gsObjectSlots[_FIXES_CEILDIV(MAX_PLAYERS, _FIXES_ATTACHMENTS)],
	/* 
	 * FIXES_gsLastAnimation[MAX_PLAYERS]
	 * 
	 * The last animation a player used.
	 */
	FIXES_gsLastAnimation[MAX_PLAYERS],
	/* 
	 * FIXES_gsLastCash[MAX_PLAYERS]
	 * 
	 * A player's cash before dying.
	 */
	FIXES_gsLastCash[MAX_PLAYERS],
	/* 
	 * FIXES_gsDriveByWeapon[MAX_PLAYERS]
	 * 
	 * The weapon to set after re-enter to vehicle.
	 */
	FIXES_gsDriveByWeapon[MAX_PLAYERS],
	/* 
	 * FIXES_gsCurrentMenu[MAX_PLAYERS]
	 * 
	 * The menu the player can currently see.
	 */
	Menu:FIXES_gsCurrentMenu[MAX_PLAYERS] = {Menu:INVALID_MENU, ...},
	/* 
	 * Text:FIXES_gsGTStyle[FIXES_GT_STYLE_COUNT]
	 * 
	 * The pre-defined TextDraw styles used to recreate the bugged GameText
	 * styles for GameTextForAll.
	 */
#if FIX_GameText
	Text:FIXES_gsGTStyle[FIXES_GT_STYLE_COUNT],
#endif
	/* 
	 * FIXES_gsPlayerPGTShown[FIXES_GT_STYLE_COUNT][MAX_PLAYERS + 1]
	 * 
	 * A linked list of which players can see a certain GameText style.
	 */
#if FIX_GameText
	FIXES_gsPlayerPGTShown[FIXES_GT_STYLE_COUNT][MAX_PLAYERS + 1],
#endif
	/* 
	 * PlayerText:FIXES_gsPGTStyle[FIXES_GT_STYLE_COUNT][MAX_PLAYERS]
	 * 
	 * The predefined TextDraw styles used to recreate the bugged GameText
	 * styles for GameTextForPlayer.
	 */
#if FIX_GameText
	PlayerText:FIXES_gsPGTStyle[MAX_PLAYERS][FIXES_GT_STYLE_COUNT],
#endif
	/* 
	 * FIXES_gsPGTTimer[FIXES_GT_STYLE_COUNT][MAX_PLAYERS]
	 * 
	 * The timers used to hide per-player GameText messages.
	 */
#if FIX_GameText
	FIXES_gsGTTimer[FIXES_GT_STYLE_COUNT][MAX_PLAYERS + 1],
#endif
	/* 
	 * FIXES_gsClassAnimTimer[MAX_PLAYERS],
	 * 
	 * Used for storing the timer ID for deferring class selection animation
	 * application.
	 */
	FIXES_gsAnimTimer[MAX_PLAYERS],
	/* 
	 * FIXES_gsClassAnimName[MAX_PLAYERS][60],
	 * 
	 * Used for storing the animation name.
	 */
#if FIX_ApplyAnimation_2
	FIXES_gsClassAnimName[MAX_PLAYERS][60],
#endif
	/* 
	 * FIXES_gsPlayerAnimLibs[MAX_PLAYERS][_FIXES_CEILDIV(135, cellbits)],
	 * 
	 * Which animation libraries the player has synced.
	 */
#if FIX_ApplyAnimation_2
	FIXES_gsPlayerAnimLibs[MAX_PLAYERS][_FIXES_CEILDIV(135, cellbits)],
#endif
	/* 
	 * FIXES_pvarNotNewPlayer[]
	 * 
	 * This variable records wether or not this player is brand new to the
	 * server, and not just having "OnPlayerConnect" called due to a script
	 * load.  There are certain functions that need applying only the very first
	 * time they connect, then this gets set.
	 */
	FIXES_pvarNotNewPlayer[] = "FIXES_pvarNotNewPlayer",
	/* 
	 * FIXES_pvarPlayerWeapon[]
	 * 
	 * Name of the pvar in which to store the player's current weapon.
	 */
	FIXES_pvarPlayerWeapon[] = "FIXES_pvarPlayerWeapon",
	/* 
	 * FIXES_pvarPlayerSkin[]
	 * 
	 * Name of the pvar in which to store the player's current skin.
	 */
	FIXES_pvarPlayerSkin[] = "FIXES_pvarPlayerSkin",
	/* 
	 * FIXES_pvarPlayerSpectate[]
	 * 
	 * Name of the pvar in which to store the player's spectator mode.
	 */
	FIXES_pvarPlayerSpectate[] = "FIXES_pvarPlayerSpectate",
	/* 
	 * FIXES_gscKick[]
	 * 
	 * Name of the kick timer function.
	 */
	FIXES_gscKick[] = "_FIXES_Kick",
	/* 
	 * FIXES_pvarKick[]
	 * 
	 * Name of the pvar in which to store a player's kick timer.
	 */
	FIXES_pvarKick[] = "FIXES_pvarKick",
	/* 
	 * FIXES_pvarPlayerDialog[]
	 * 
	 * Name of the pvar in which to store the player's current dialogid.
	 */
	FIXES_pvarPlayerDialog[] = "FIXES_pvarPlayerDialog",
	/* 
	 * FIXES_pvarPlayerInterior[]
	 * 
	 * A player's current interior.
	 */
	FIXES_pvarPlayerInterior[] = "FIXES_pvarPlayerInterior",
	/* 
	 * FIXES_pvarCurrentDialog[]
	 * 
	 * Used in OnDialogResponse to be able to get the correct ID in multiple
	 * scripts while still correctly resetting the ID for future use.
	 */
	FIXES_pvarCurrentDialog[] = "FIXES_pvarCurrentDialog",
	/* 
	 * FIXES_pvarPlayerLastCash[]
	 * 
	 * Used in OnPlayerDeath to save a player's cash before death.
	 */
	FIXES_pvarPlayerLastCash[] = "FIXES_pvarPlayerLastCash",
	/* 
	 * FIXES_gscHideGameTextTimer[]
	 * 
	 * Name of the GameText hide timer function.
	 */
	FIXES_gscHideGameTextTimer[] = "_FIXES_HideGameTextTimer",
	/* 
	 * FIXES_gscDriveBy[]
	 * 
	 * Name of the Drive-By timer function.
	 */
	FIXES_gscDriveBy[] = "_FIXES_DriveBy",
	/* 
	 * FIXES_gscSetCamera[]
	 * 
	 * Name of the SetCamera timer function.
	 */
	FIXES_gscSetCamera[] = "_FIXES_SetCamera",
	/* 
	 * FIXES_gscSetTime[]
	 * 
	 * Name of the SetTime timer function.
	 */
	FIXES_gscSetTime[] = "_FIXES_SetTime",
	/* 
	 * FIXES_gscSetColor[]
	 * 
	 * Name of the SetColor timer function.
	 */
	FIXES_gscSetColor[] = "_FIXES_SetColor",
	/* 
	 * FIXES_gscSetCheckpoint[]
	 * 
	 * Name of the SetCheckpoint timer function.
	 */
	FIXES_gscSetCheckpoint[] = "_FIXES_SetCheckpoint",
	/* 
	 * FIXES_pvarPlayerCheckpoint[]
	 * 
	 * Name of the pvar in which to store the player's checkpoint state.
	 */
	FIXES_pvarPlayerCheckpoint[] = "FIXES_pvarPlayerCheckpoint",
	/* 
	 * FIXES_gscSetRaceCheckpoint[]
	 * 
	 * Name of the SetRaceCheckpoint timer function.
	 */
	FIXES_gscSetRaceCheckpoint[] = "_FIXES_SetRaceCheckpoint",
	/* 
	 * FIXES_pvarPlayerRaceCheckpoint[]
	 * 
	 * Name of the pvar in which to store the player's race checkpoint state.
	 */
	FIXES_pvarPlayerRaceCheckpoint[] = "FIXES_pvarPlayerRaceCheckpoint",
	/* 
	 * FIXES_gscNULL[]
	 * 
	 * NULL.  This is stored as a global string to reduce memory usage.
	 */
	FIXES_gscNULL[] = "\1";

static stock const
	/* 
	 * FIXES_gscPlayerColours[100]
	 * 
	 * There are only 100 colours used by default, with SA:MP looping through
	 * them repeatedly.
	 */
#if FIX_GetPlayerColour
	FIXES_gscPlayerColours[100] =
		{
			0xFF8C13FF, 0xC715FFFF, 0x20B2AAFF, 0xDC143CFF, 0x6495EDFF,
			0xF0E68CFF, 0x778899FF, 0xFF1493FF, 0xF4A460FF, 0xEE82EEFF,
			0xFFD720FF, 0x8B4513FF, 0x4949A0FF, 0x148B8BFF, 0x14FF7FFF,
			0x556B2FFF, 0x0FD9FAFF, 0x10DC29FF, 0x534081FF, 0x0495CDFF,
			0xEF6CE8FF, 0xBD34DAFF, 0x247C1BFF, 0x0C8E5DFF, 0x635B03FF,
			0xCB7ED3FF, 0x65ADEBFF, 0x5C1ACCFF, 0xF2F853FF, 0x11F891FF,
			0x7B39AAFF, 0x53EB10FF, 0x54137DFF, 0x275222FF, 0xF09F5BFF,
			0x3D0A4FFF, 0x22F767FF, 0xD63034FF, 0x9A6980FF, 0xDFB935FF,
			0x3793FAFF, 0x90239DFF, 0xE9AB2FFF, 0xAF2FF3FF, 0x057F94FF,
			0xB98519FF, 0x388EEAFF, 0x028151FF, 0xA55043FF, 0x0DE018FF,
			0x93AB1CFF, 0x95BAF0FF, 0x369976FF, 0x18F71FFF, 0x4B8987FF,
			0x491B9EFF, 0x829DC7FF, 0xBCE635FF, 0xCEA6DFFF, 0x20D4ADFF,
			0x2D74FDFF, 0x3C1C0DFF, 0x12D6D4FF, 0x48C000FF, 0x2A51E2FF,
			0xE3AC12FF, 0xFC42A8FF, 0x2FC827FF, 0x1A30BFFF, 0xB740C2FF,
			0x42ACF5FF, 0x2FD9DEFF, 0xFAFB71FF, 0x05D1CDFF, 0xC471BDFF,
			0x94436EFF, 0xC1F7ECFF, 0xCE79EEFF, 0xBD1EF2FF, 0x93B7E4FF,
			0x3214AAFF, 0x184D3BFF, 0xAE4B99FF, 0x7E49D7FF, 0x4C436EFF,
			0xFA24CCFF, 0xCE76BEFF, 0xA04E0AFF, 0x9F945CFF, 0xDCDE3DFF,
			0x10C9C5FF, 0x70524DFF, 0x0BE472FF, 0x8A2CD7FF, 0x6152C2FF,
			0xCF72A9FF, 0xE59338FF, 0xEEDC2DFF, 0xD8C762FF, 0xD8C762FF
		},
#endif
	/* 
	 * FIXES_gscMaxPassengers[]
	 * 
	 * This is a compressed (4-bit) list of the maximum number of passengers in
	 * any vehicle, confirmed by a number of sources. "F" (15) means invalid
	 * vehicle.
	 */
#if FIX_OnPlayerEnterVehicle
	FIXES_gscMaxPassengers[] =
		{
			0x10331113, 0x11311131, 0x11331313, 0x80133301, 0x1381F110, 0x10311103, 0x10001F10, 0x11113311, 0x13113311,
			0x31101100, 0x30001301, 0x11031311, 0x11111331, 0x10013111, 0x01131100, 0x11111110, 0x11100031, 0x11130221,
			0x33113311, 0x11111101, 0x33101133, 0x101001F0, 0x03133111, 0xFF11113F, 0x13330111, 0xFF131111, 0x0000FF3F
		},
#endif
	/* 
	 * FIXES_gscVehicleMods[]
	 * 
	 * This is a bit array of all the valid mods (-1000) for all vehicles (-400)
	 * EXCEPT for vehicle 576 (Tornado), which has just TWO extra mods on it,
	 * that should spill over in to an extra cell of data (requiring an extra
	 * 848 bytes of data total to make the array work).  Instead this (hopefully
	 * rare) case is handled explicitly in "OnVehicleMod".  It seems that most
	 * vehicles are:
	 * 
	 *   0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
	 * 
	 * I don't know yet if this can be used to our advantage to improve the code
	 * somehow - it seems like that would require more explicit model handling.
	 */
#if FIX_OnVehicleMod
	FIXES_gscVehicleMods[] =
		{
			0x033C2700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x021A27FA, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x023B2785, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02BC4703, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x03BA278A, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x028E078A, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02310744, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x0228073A, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02BD4701, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x023A2780, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x0228077A, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x027A27CA, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x0282278A, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x023E07C0, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x03703730, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x031D2775, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02BE4788, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x02010771, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x029A0FCE, 0x00000000, 0x00FFFE00, 0x00000007, 0x0000C000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x03382700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x023F8795, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x029F078C, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x029627EA, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x0236C782, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x029E1FCA, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0xFC000437, 0x00000000, 0x021C0000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x03FE6007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00001B87, 0x00000001, 0x01E00000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x039E07D2, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x023CC700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00030000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x038E07D6, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x023D0709, 0x00000000, 0x00FFFE00, 0x00000007, 0x0000C000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x029E1F8A, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x029C077A, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x02BD076C, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0xFFFFFE00, 0x00000007, 0x00000000, 0x000001F8,
			0x02000700, 0x00000000, 0x00FFFFFE, 0x00000007, 0xC0000000, 0x00002007,
			0xFE000700, 0x00000003, 0x00FFFE00, 0x00000007, 0x00003C00, 0x00000600,
			0xCE000700, 0xFF800000, 0x00FFFE01, 0x00000007, 0x3C000000, 0x00000000,
			0x02000700, 0x000003FC, 0x00FFFE00, 0x00000007, 0x003C0000, 0x00001800,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x007FE000, 0x00FFFE00, 0x00000007, 0x03C00000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000047, 0x0000003E, 0x3C000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00001C00, 0x00FFFE00, 0x0000000F, 0x00000000, 0x0003C000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x000003C0, 0xC0000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x029607C2, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x03FFE7CD, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x031727F1, 0x00000000, 0x00FFFE00, 0x00000007, 0x00030000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x025627F0, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x039E07C2, 0x00000000, 0x00FFFE00, 0x00000007, 0x0003C000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000,
			0x02000700, 0x00000000, 0x00FFFE00, 0x00000007, 0x00000000, 0x00000000
		},
#endif
	/* 
	 * FIXES_gscAnimIndexes[]
	 * 
	 * Which index each letter starts at.  Sometimes crashes the compiler...
	 */
#if FIX_ApplyAnimation || FIX_ApplyAnimation_2
	FIXES_gscAnimIndexes[24] =
		{
			0, 2, 21, 35, 42, 42, 53, 62, 64, 67, 68, 71, 75, 81, 82, 84, 94, 96, 104, 122, 127, 128, 131, 135
		},
#endif
	/* 
	 * FIXES_gscAnimLib[]
	 * 
	 * List of valid animation libraries.
	 */
#if FIX_ApplyAnimation || FIX_ApplyAnimation_2
	FIXES_gscAnimLib[135][] =
		{
			"AIRPORT",      "ATTRACTORS",   "BAR",          "BASEBALL",     "BD_FIRE",
			"BEACH",        "BENCHPRESS",   "BF_INJECTION", "BIKE_DBZ",     "BIKED",
			"BIKEH",        "BIKELEAP",     "BIKES",        "BIKEV",        "BLOWJOBZ",
			"BMX",          "BOMBER",       "BOX",          "BSKTBALL",     "BUDDY",
			"BUS",          "CAMERA",       "CAR",          "CAR_CHAT",     "CARRY",
			"CASINO",       "CHAINSAW",     "CHOPPA",       "CLOTHES",      "COACH",
			"COLT45",       "COP_AMBIENT",  "COP_DVBYZ",    "CRACK",        "CRIB",
			"DAM_JUMP",     "DANCING",      "DEALER",       "DILDO",        "DODGE",
			"DOZER",        "DRIVEBYS",     "FAT",          "FIGHT_B",      "FIGHT_C",
			"FIGHT_D",      "FIGHT_E",      "FINALE",       "FINALE2",      "FLAME",
			"FLOWERS",      "FOOD",         "FREEWEIGHTS",  "GANGS",        "GFUNK",
			"GHANDS",       "GHETTO_DB",    "GOGGLES",      "GRAFFITI",     "GRAVEYARD",
			"GRENADE",      "GYMNASIUM",    "HAIRCUTS",     "HEIST9",       "INT_HOUSE",
			"INT_OFFICE",   "INT_SHOP",     "JST_BUISNESS", "KART",         "KISSING",
			"KNIFE",        "LAPDAN1",      "LAPDAN2",      "LAPDAN3",      "LOWRIDER",
			"MD_CHASE",     "MD_END",       "MEDIC",        "MISC",         "MTB",
			"MUSCULAR",     "NEVADA",       "ON_LOOKERS",   "OTB",          "PARACHUTE",
			"PARK",         "PAULNMAC",     "PED",          "PLAYER_DVBYS", "PLAYIDLES",
			"POLICE",       "POOL",         "POOR",         "PYTHON",       "QUAD",
			"QUAD_DBZ",     "RAPPING",      "RIFLE",        "RIOT",         "ROB_BANK",
			"ROCKET",       "RUNNINGMAN",   "RUSTLER",      "RYDER",        "SAMP",
			"SCRATCHING",   "SEX",          "SHAMAL",       "SHOP",         "SHOTGUN",
			"SILENCED",     "SKATE",        "SMOKING",      "SNIPER",       "SNM",
			"SPRAYCAN",     "STRIP",        "SUNBATHE",     "SWAT",         "SWEET",
			"SWIM",         "SWORD",        "TANK",         "TATTOOS",      "TEC",
			"TRAIN",        "TRUCK",        "UZI",          "VAN",          "VENDING",
			"VORTEX",       "WAYFARER",     "WEAPONS",      "WOP",          "WUZI"
		},
#endif
	/* 
	 * FIXES_gscDot[]
	 * 
	 * ".".
	 */
	FIXES_gscDot[] = ".",
	/* 
	 * FIXES_gscSpec@[]
	 * 
	 * Specifier "".
	 */
	FIXES_gscSpec@[] = "",
	/* 
	 * FIXES_gscSpec@i[]
	 * 
	 * Specifier "i".
	 */
	FIXES_gscSpec@i[] = "i",
	/* 
	 * FIXES_gscSpec@ii[]
	 * 
	 * Specifier "ii".
	 */
	FIXES_gscSpec@ii[] = "ii",
	/* 
	 * FIXES_gscSpec@ai[]
	 * 
	 * Specifier "ai".
	 */
	FIXES_gscSpec@ai[] = "ai",
	/* 
	 * FIXES_gscSpec@is[]
	 * 
	 * Specifier "is".
	 */
	FIXES_gscSpec@is[] = "is",
	/* 
	 * FIXES_gscSpec@iii[]
	 * 
	 * Specifier "iii".
	 */
	FIXES_gscSpec@iii[] = "iii",
	/* 
	 * FIXES_gscSpec@isii[]
	 * 
	 * Specifier "isii".
	 */
	FIXES_gscSpec@isii[] = "isii",
	/* 
	 * FIXES_gscSpec@ifff[]
	 * 
	 * Specifier "ifff".
	 */
	FIXES_gscSpec@ifff[] = "ifff",
	/* 
	 * FIXES_gscSpec@iifff[]
	 * 
	 * Specifier "iifff".
	 */
	FIXES_gscSpec@iifff[] = "iifff",
	/* 
	 * FIXES_gscSpec@iifffffff[]
	 * 
	 * Specifier "iifffffff".
	 */
	FIXES_gscSpec@iifffffff[] = "iifffffff",
	/* 
	 * FIXES_gscSpec@iffff[]
	 * 
	 * Specifier "iffff".
	 */
	FIXES_gscSpec@iffff[] = "iffff",
	/* 
	 * FIXES_gscSpec@iiiis[]
	 * 
	 * Specifier "iiiis".
	 */
	FIXES_gscSpec@iiiis[] = "iiiis",
	/* 
	 * FIXES_gscSpec@iiiii[]
	 * 
	 * Specifier "iiiii".
	 */
	FIXES_gscSpec@iiiii[] = "iiiii",
	/* 
	 * FIXES_gscSpec@iiiiii[]
	 * 
	 * Specifier "iiiiii".
	 */
	FIXES_gscSpec@iiiiii[] = "iiiiii",
	/* 
	 * FIXES_gscTempName[]
	 * 
	 * The temporary name to give when renaming people.
	 */
	FIXES_gscTempName[] = "FIXES_TEMP_NAME",
	/* 
	 * FIXES_gscOrderProperty[]
	 * 
	 * The property to check for script ordering.
	 */
	FIXES_gscOrderProperty[] = "FIXES_gscOrderProperty",
	/* 
	 * FIXES_gscNoGMProperty[]
	 * 
	 * Call "_FIXES_DetermineOrder" in the GM or not?
	 */
	FIXES_gscNoGMProperty[] = "FIXES_gscNoGMProperty",
	/* 
	 * FIXES_gscDetermineOrder[]
	 * 
	 * Name of the ordering callback.
	 */
	FIXES_gscDetermineOrder[] = "_FIXES_DetermineOrder",
	/* 
	 * FIXES_gscSetPlayerMenu[]
	 * 
	 * Name of the remote set player menu function.
	 */
	FIXES_gscSetPlayerMenu[] = "_FIXES_SetPlayerMenu",
	/* 
	 * FIXES_gscClearPlayerMenu[]
	 * 
	 * Name of the remote function to clear a player's menu set.
	 */
	FIXES_gscClearPlayerMenu[] = "_FIXES_ClearPlayerMenu",
	/* 
	 * FIXES_gscAllowTeleport[]
	 * 
	 * Name of the remote allow teleport function.
	 */
	FIXES_gscAllowTeleport[] = "_FIXES_AllowTeleport",
	/* 
	 * FIXES_gscPutPlayerInVehicle[]
	 * 
	 * Name of the remote vehicle entry function.
	 */
	FIXES_gscPutPlayerInVehicle[] = "_FIXES_PutPlayerInVehicle",
	/* 
	 * FIXES_gscAllowInteriorWeapons[]
	 * 
	 * Name of the remote interior weapons function.
	 */
	FIXES_gscAllowInteriorWeapons[] = "_FIXES_AllowInteriorWeapons",
	/* 
	 * FIXES_gscSetPlayerAttachedObj[]
	 * 
	 * Name of the remote attache object function.
	 */
	FIXES_gscSetPlayerAttachedObj[] = "_FIXES_SetPlayerAttachedObject",
	/* 
	 * FIXES_gscTogglePlayerControl[]
	 * 
	 * Name of the remote player toggle function.
	 */
	FIXES_gscTogglePlayerControl[] = "_FIXES_TogglePlayerControllable",
	/* 
	 * FIXES_gscSetPlayerWorldBounds[]
	 * 
	 * Name of the remote world bounds function.
	 */
	FIXES_gscSetPlayerWorldBounds[] = "_FIXES_SetPlayerWorldBounds",
	/* 
	 * FIXES_gscGameTextShow[]
	 * 
	 * Name of the remote GameText show function.
	 */
	FIXES_gscGameTextShow[] = "_FIXES_GameTextShow",
	/* 
	 * FIXES_gscReturnProperty[]
	 * 
	 * The property for accurate returns.
	 */
	FIXES_gscReturnProperty[] = "FIXES_gscReturnProperty",
	/* 
	 * FIXES_gscSingleProperty[]
	 * 
	 * The property for checking this is the only running script with fixes in.
	 */
	FIXES_gscSingleProperty[] = "FIXES_gscSingleProperty",
	/* 
	 * FIXES_gscMenuProperty[]
	 * 
	 * The property for a player's current menu.
	 */
	FIXES_gscMenuProperty[] = "FIXES_gscMenuProperty",
	/* 
	 * FIXES_gscFixesError[]
	 * 
	 * Error shown when multiple scripts are detected.
	 */
	FIXES_gscFixesError[] = "\7\7\7\7\7"                                                     "\n"   \
		"*** fixes.inc error: Running multiple scripts compiled with \"fixes.inc\"..."       "\n"   \
		"***     Please compile your modes with \"#define FIXES_Single 0\" at the top, as"   "\n"   \
		"***     this setting is no longer the default (to improve the more common case)."          ;

/* 
 * _FIXES_KEY_AIM
 * KEY_AIM
 * 
 * Because the default SA:MP includes missed this one.
 */

#define _FIXES_KEY_AIM                   (128)
#if FIX_KEY_AIM
	#define KEY_AIM                      _FIXES_KEY_AIM
#endif

/* 
 * _FIXES_SPECIAL_ACTION_PISSING
 * SPECIAL_ACTION_PISSING
 * 
 * Because the default SA:MP includes missed this one.
 */

#define _FIXES_SPECIAL_ACTION_PISSING    (68)
#if FIX_SPECIAL_ACTION_PISSING
	#define SPECIAL_ACTION_PISSING       _FIXES_SPECIAL_ACTION_PISSING
#endif

/* 
 * IsValidVehicle
 * 
 * Because the default SA:MP includes missed this one.
 */

#if FIX_IsValidVehicle
	native IsValidVehicle(vehicleid);
#endif

/* 
 * GetGravity
 * 
 * Because the default SA:MP includes missed this one.
 */

#if FIX_GetGravity
	native Float:GetGravity();
#endif

/* 
 * gpci
 * 
 * Because the default SA:MP includes missed this one.
 */

#if FIX_gpci
	native gpci(playerid, serial[], maxlen);
#endif

/* 
 * BODYPARTS
 * 
 * Because the default SA:MP includes missed these.
 */

#if FIX_BODYPARTS
	#define BODY_PART_TORSO                     (3)
	#define BODY_PART_GROIN                     (4)
	#define BODY_PART_LEFT_ARM                  (5)
	#define BODY_PART_RIGHT_ARM                 (6)
	#define BODY_PART_LEFT_LEG                  (7)
	#define BODY_PART_RIGHT_LEG                 (8)
	#define BODY_PART_HEAD                      (9)
#endif

/* 
 * CAMERAMODES
 * 
 * Because the default SA:MP includes missed these.
 */

#if FIX_CAMERAMODES
	#define CAM_MODE_DISCONNECTED               (-1)
	#define CAM_MODE_NONE                       (0)
	#define CAM_MODE_BEHINDCAR                  (3)
	#define CAM_MODE_FOLLOWPED                  (4)
	#define CAM_MODE_SNIPER                     (7)
	#define CAM_MODE_ROCKETLAUNCHER             (8)
	#define CAM_MODE_FIXED                      (15)
	#define CAM_MODE_1STPERSON                  (16)
	#define CAM_MODE_CAM_ON_A_STRING            (18)
	#define CAM_MODE_BEHINDBOAT                 (22)
	#define CAM_MODE_CAMERA                     (46)
	#define CAM_MODE_ROCKETLAUNCHER_HS          (51)
	#define CAM_MODE_AIMWEAPON                  (53)
	#define CAM_MODE_AIMWEAPON_FROMCAR          (55)
	#define CAM_MODE_DW_HELI_CHASE              (56)
#endif

/* 
 * IS_FILTERSCRIPT
 * 
 * "FILTERSCRIPT" can't always be relied on to be set.  This is not a pre-
 * processor macro, but may be better than nothing (also used internally).
 */

#if FIX_FILTERSCRIPT
	#define IS_FILTERSCRIPT _FIXES_gIsFilterscript
#endif

/* 
 * File operators
 * 
 * Forwards for operators that shouldn't exist, so they won't exist.
 */

#if FIX_FileMaths
	forward File:operator++(File:a);
	forward File:operator--(File:a);
	forward File:operator-(File:a);
	forward File:operator+(File:a, File:b);
	forward File:operator+(File:a, _:b);
	forward File:operator-(File:a, File:b);
	forward File:operator-(_:a, File:b);
	forward File:operator-(File:a, _:b);
	forward File:operator*(File:a, File:b);
	forward File:operator*(File:a, _:b);
	forward File:operator/(File:a, File:b);
	forward File:operator/(_:a, File:b);
	forward File:operator/(File:a, _:b);
	forward File:operator%(File:a, File:b);
	forward File:operator%(_:a, File:b);
	forward File:operator%(File:a, _:b);
	forward bool:operator<(File:a, File:b);
	forward bool:operator<(_:a, File:b);
	forward bool:operator<(File:a, _:b);
	forward bool:operator<=(File:a, File:b);
	forward bool:operator<=(_:a, File:b);
	forward bool:operator<=(File:a, _:b);
	forward bool:operator>(File:a, File:b);
	forward bool:operator>(_:a, File:b);
	forward bool:operator>(File:a, _:b);
	forward bool:operator>=(File:a, File:b);
	forward bool:operator>=(_:a, File:b);
	forward bool:operator>=(File:a, _:b);
#endif

/* 
 * FIXES_DetermineOrder()
 * 
 * Figure out which script is called first by callbacks.
 */

#if !FIXES_Single
	forward _FIXES_DetermineOrder();
	
	static FIXES_DetermineOrder()
	{
		deleteproperty(5, FIXES_gscOrderProperty),
		// Called in the Game Mode first (thus needs correcting).
		setproperty(5, FIXES_gscNoGMProperty, 1),
		CallRemoteFunction(FIXES_gscDetermineOrder, FIXES_gscSpec@),
		deleteproperty(5, FIXES_gscNoGMProperty),
		CallRemoteFunction(FIXES_gscDetermineOrder, FIXES_gscSpec@);
	}
#endif

/* 
 * FIXES_IsPlayerConnected(playerid)
 * 
 * FIXES:
 *     IsPlayerConnected
 */

#if defined _ALS_IsPlayerConnected
	#error _ALS_IsPlayerConnected defined
#endif
native BAD_IsPlayerConnected(playerid) = IsPlayerConnected;

#if FIX_IsPlayerConnected
	stock FIXES_IsPlayerConnected(playerid)
	{
		return _FIXES_IS_PLAYER_CONNECTED(playerid);
	}
	
	#define _ALS_IsPlayerConnected
	#define IsPlayerConnected FIXES_IsPlayerConnected
#endif

/* 
 * _FIXES_CreateGameTextDraws(playerid, test)
 * 
 * Create the text draws used to replicate game texts.
 * 
 * FIXES:
 *     GameText
 */

#if FIX_GameText
	static _FIXES_CreateGameTextDraws(const playerid)
	{
		if (playerid == INVALID_PLAYER_ID)
		{
				new
					Text:t;
				
			#if FIX_GameTextStyles
				
				// Global style 7 (vehicle name).
				t = FIXES_gsGTStyle[7] = TextDrawCreate(608.000000, 344.000000, FIXES_gscSpace),
				TextDrawLetterSize(t, 1.000000, 3.000000),
				TextDrawAlignment(t, 3),
				TextDrawColor(t, 0x36682CFF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 2),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 10.0, 200.0);
				
				// Global style 8 (location name).
				t = FIXES_gsGTStyle[8] = TextDrawCreate(608.000000, 386.500000, FIXES_gscSpace),
				TextDrawLetterSize(t, 1.200000, 3.799998),
				TextDrawAlignment(t, 3),
				TextDrawColor(t, 0xACCBF1FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 0),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 10.0, 200.0);
				
				// Global style 9 (radio name).
				t = FIXES_gsGTStyle[9] = TextDrawCreate(320.000000, 22.000000, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.600000, 1.899999),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0x906210FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 1),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 2),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
				
				// Global style 10 (radio switch).
				t = FIXES_gsGTStyle[10] = TextDrawCreate(320.000000, 22.000000, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.600000, 1.899999),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0x969696FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 1),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 2),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
				
				// Global style 11 (positive money).
				t = FIXES_gsGTStyle[11] = TextDrawCreate(607.000000, 78.000000, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.550000, 2.150000),
				TextDrawAlignment(t, 3),
				TextDrawColor(t, 0x36682CFF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000FF),
				TextDrawFont(t, 3),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 10.0, 200.0);
				
				// Global style 12 (negative money).
				t = FIXES_gsGTStyle[12] = TextDrawCreate(607.000000, 78.000000, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.550000, 2.150000),
				TextDrawAlignment(t, 3),
				TextDrawColor(t, 0xB4191DFF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000FF),
				TextDrawFont(t, 3),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 10.0, 200.0);
				
				// Global style 13 (stunt).
				t = FIXES_gsGTStyle[13] = TextDrawCreate(380.000000, 341.000000, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.579999, 2.400000),
				TextDrawTextSize(t, 40.000000, 460.000000),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0xD7D3CCFF),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0),
				TextDrawSetShadow(t, 2),
				TextDrawSetOutline(t, 0),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 1),
				TextDrawSetProportional(t, 1),
				
			#endif
				
				// Global style 0.
				t = FIXES_gsGTStyle[0] = TextDrawCreate(320.0, 214.0, FIXES_gscSpace),
				TextDrawLetterSize(t, 1.3, 3.6),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0x906210FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 3),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
				
				// Global style 1.
				t = FIXES_gsGTStyle[1] = TextDrawCreate(620.0, 310.0, FIXES_gscSpace),
				TextDrawLetterSize(t, 1.0, 2.6),
				TextDrawAlignment(t, 3),
				TextDrawColor(t, 0x906210FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 3),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 10.0, 200.0);
				
				// Global style 2.
				t = FIXES_gsGTStyle[2] = TextDrawCreate(320.0, 156.0, FIXES_gscSpace),
				TextDrawLetterSize(t, 2.1, 4.2),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0xE1E1E1FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 3),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 0),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
				
				// Global style 3.
				t = FIXES_gsGTStyle[3] = TextDrawCreate(320.000000, 154.500000, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.600000, 2.750000),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0x906210FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 2),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
				
				// Global style 4.
				t = FIXES_gsGTStyle[4] = TextDrawCreate(320.000000, 115.500000, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.600000, 2.750000),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0x906210FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 2),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
				
				// Global style 5.
				t = FIXES_gsGTStyle[5] = TextDrawCreate(320.0, 217.0, FIXES_gscSpace),
				TextDrawLetterSize(t, 0.6, 2.75),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0xE1E1E1FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 2),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
				
				// Global style 6.
				t = FIXES_gsGTStyle[6] = TextDrawCreate(320.000000, 60.000000, FIXES_gscSpace),
				TextDrawLetterSize(t, 1.000000, 3.599998),
				TextDrawAlignment(t, 2),
				TextDrawColor(t, 0xACCBF1FF),
				TextDrawSetShadow(t, 0),
				TextDrawSetOutline(t, 2),
				TextDrawBackgroundColor(t, 0x000000AA),
				TextDrawFont(t, 3),
				TextDrawSetProportional(t, 1),
				TextDrawUseBox(t, true),
				TextDrawBoxColor(t, 0x00000000),
				TextDrawTextSize(t, 200.0, 620.0);
		}
		else
		{
				new
					PlayerText:t;
				
			#if FIX_GameTextStyles
				
				// Global style 7 (playerid, vehicle name).
				t = FIXES_gsPGTStyle[playerid][7] = CreatePlayerTextDraw(playerid, 608.000000, 344.000000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 1.000000, 3.000000),
				PlayerTextDrawAlignment(playerid, t, 3),
				PlayerTextDrawColor(playerid, t, 0x36682CFF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 2),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 10.0, 200.0);
				
				// Global style 8 (playerid, location name).
				t = FIXES_gsPGTStyle[playerid][8] = CreatePlayerTextDraw(playerid, 608.000000, 386.500000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 1.200000, 3.799998),
				PlayerTextDrawAlignment(playerid, t, 3),
				PlayerTextDrawColor(playerid, t, 0xACCBF1FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 0),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 10.0, 200.0);
				
				// Global style 9 (playerid, radio name).
				t = FIXES_gsPGTStyle[playerid][9] = CreatePlayerTextDraw(playerid, 320.000000, 22.000000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.600000, 1.899999),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0x906210FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 1),
				PlayerTextDrawBackgroundColor(playerid, t, 170),
				PlayerTextDrawFont(playerid, t, 2),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
				
				// Global style 10 (playerid, radio switch).
				t = FIXES_gsPGTStyle[playerid][10] = CreatePlayerTextDraw(playerid, 320.000000, 22.000000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.600000, 1.899999),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0x969696FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 1),
				PlayerTextDrawBackgroundColor(playerid, t, 170),
				PlayerTextDrawFont(playerid, t, 2),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
				
				// Global style 11 (playerid, positive money).
				t = FIXES_gsPGTStyle[playerid][11] = CreatePlayerTextDraw(playerid, 607.000000, 78.000000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.550000, 2.150000),
				PlayerTextDrawAlignment(playerid, t, 3),
				PlayerTextDrawColor(playerid, t, 0x36682CFF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000FF),
				PlayerTextDrawFont(playerid, t, 3),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 10.0, 200.0);
				
				// Global style 12 (playerid, negative money).
				t = FIXES_gsPGTStyle[playerid][12] = CreatePlayerTextDraw(playerid, 607.000000, 78.000000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.550000, 2.150000),
				PlayerTextDrawAlignment(playerid, t, 3),
				PlayerTextDrawColor(playerid, t, 0xB4191DFF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000FF),
				PlayerTextDrawFont(playerid, t, 3),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 10.0, 200.0);
				
				// Global style 13 (playerid, stunt).
				t = FIXES_gsPGTStyle[playerid][13] = CreatePlayerTextDraw(playerid, 380.000000, 341.000000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.579999, 2.400000),
				PlayerTextDrawTextSize(playerid, t, 40.000000, 460.000000),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0xD7D3CCFF),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0),
				PlayerTextDrawSetShadow(playerid, t, 2),
				PlayerTextDrawSetOutline(playerid, t, 0),
				PlayerTextDrawBackgroundColor(playerid, t, 170),
				PlayerTextDrawFont(playerid, t, 1),
				PlayerTextDrawSetProportional(playerid, t, 1);
				
			#endif
				
				// Global style 0.
				t = FIXES_gsPGTStyle[playerid][0] = CreatePlayerTextDraw(playerid, 320.0, 214.0, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 1.3, 3.6),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0x906210FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 3),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
				
				// Global style 1.
				t = FIXES_gsPGTStyle[playerid][1] = CreatePlayerTextDraw(playerid, 620.0, 310.0, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 1.0, 2.6),
				PlayerTextDrawAlignment(playerid, t, 3),
				PlayerTextDrawColor(playerid, t, 0x906210FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 3),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 10.0, 200.0);
				
				// Global style 2.
				t = FIXES_gsPGTStyle[playerid][2] = CreatePlayerTextDraw(playerid, 320.0, 156.0, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 2.1, 4.2),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0xE1E1E1FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 3),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 0),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
				
				// Global style 3.
				t = FIXES_gsPGTStyle[playerid][3] = CreatePlayerTextDraw(playerid, 320.000000, 154.500000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.600000, 2.750000),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0x906210FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 2),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
				
				// Global style 4.
				t = FIXES_gsPGTStyle[playerid][4] = CreatePlayerTextDraw(playerid, 320.000000, 115.500000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.600000, 2.750000),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0x906210FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 2),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
				
				// Global style 5.
				t = FIXES_gsPGTStyle[playerid][5] = CreatePlayerTextDraw(playerid, 320.0, 217.0, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 0.6, 2.75),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0xE1E1E1FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 2),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
				
				// Global style 6.
				t = FIXES_gsPGTStyle[playerid][6] = CreatePlayerTextDraw(playerid, 320.000000, 60.000000, FIXES_gscSpace),
				PlayerTextDrawLetterSize(playerid, t, 1.000000, 3.599998),
				PlayerTextDrawAlignment(playerid, t, 2),
				PlayerTextDrawColor(playerid, t, 0xACCBF1FF),
				PlayerTextDrawSetShadow(playerid, t, 0),
				PlayerTextDrawSetOutline(playerid, t, 2),
				PlayerTextDrawBackgroundColor(playerid, t, 0x000000AA),
				PlayerTextDrawFont(playerid, t, 3),
				PlayerTextDrawSetProportional(playerid, t, 1),
				PlayerTextDrawUseBox(playerid, t, true),
				PlayerTextDrawBoxColor(playerid, t, 0x00000000),
				PlayerTextDrawTextSize(playerid, t, 200.0, 620.0);
		}
	}
#endif

/* 
 * OnFilterScriptInit()
 * 
 * Set "IS_FILTERSCRIPT" to true as this callback is ONLY called if this script
 * is actually a FilterScript.  Then call "FIXES_OnScriptInit".
 * 
 * FIXES:
 *     IS_FILTERSCRIPT
 *     OnPlayerConnect
 *     GameText
 */

public OnFilterScriptInit()
{
	// It is possible for this to be the only thing done in this function!
	_FIXES_gIsFilterscript = true;
	
	state _ALS : _ALS_go;
	
	#if FIXES_Single
		// Check this really IS the only script running.
		if (existproperty(5, FIXES_gscSingleProperty))
		{
			print(FIXES_gscFixesError);
		}
		else
		{
			FIXES_gsSettings &= ~e_FIXES_SETTINGS_SECOND_USE,
			setproperty(5, FIXES_gscSingleProperty, 1);
		}
	#endif
	
	// =================
	//  BEGIN: GameText 
	// =================
	#if FIX_GameText
		for (new i = 0; i != sizeof (FIXES_gsPlayerPGTShown); ++i)
		{
			FIXES_gsPlayerPGTShown[i][MAX_PLAYERS] = MAX_PLAYERS;
		}
	#endif
	#if FIXES_Single
		#if FIX_GameText
			_FIXES_CreateGameTextDraws(INVALID_PLAYER_ID);
		#endif
		// =================
		//  END:   GameText 
		// =================
	#else
		FIXES_DetermineOrder(),
		FIXES_gsSettings &= ~e_FIXES_SETTINGS_DROP_ALL_DATA;
	#endif
	
	#if !FIXES_Single && FIX_GameText && FIX_OnPlayerConnect
		FIXES_gsSettings |= e_FIXES_SETTINGS_NO_GAME_TEXT;
	#endif
	// These are all the fixes that use "_FIXES_FOREACH".
	#if FIX_OnPlayerConnect || _FIX_Menus || FIX_GetPlayerMenu || FIX_OnPlayerDisconnect || FIX_GameText || FIX_AllowInteriorWeapons || FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable
		for (new playerid = 0; playerid != MAX_PLAYERS; ++playerid)
		{
			if (BAD_IsPlayerConnected(playerid))
			{
				// No "defined" checks - if you don't want this fix because
				// you don't have an "OnPlayerConnect", just don't use it!
				// Of course, it helps that the copy of "OnPlayerConnect"
				// in this include is almost always called!
				#if FIX_OnPlayerConnect
					// ========================
					//  BEGIN: OnPlayerConnect 
					// ========================
					OnPlayerConnect(playerid);
					// ========================
					//  END:   OnPlayerConnect 
					// ========================
				#else
					_FIXES_AddInternal(FIXES_gsPlayersIterator, playerid, MAX_PLAYERS);
					
					#if FIX_GameText
						// =================
						//  BEGIN: GameText 
						// =================
						_FIXES_IS_IN_CHARGE()
						{
							_FIXES_CreateGameTextDraws(playerid);
						}
						// =================
						//  END:   GameText 
						// =================
					#endif
				#endif
			}
		}
	#endif
	#if !FIXES_Single && FIX_GameText && FIX_OnPlayerConnect
		FIXES_gsSettings &= ~e_FIXES_SETTINGS_NO_GAME_TEXT;
	#endif
	
	return FIXES_OnFilterScriptInit();
}

#if defined _ALS_OnFilterScriptInit
	#error _ALS_OnFilterScriptInit defined
#endif
#define _ALS_OnFilterScriptInit
#define OnFilterScriptInit(%0) FIXES_OnFilterScriptInit(%0) <_ALS : _ALS_go>

_FIXES_FORWARD FIXES_OnFilterScriptInit();

/* 
 * OnGameModeInit()
 * 
 * Call "FIXES_OnScriptInit" if this is not a FilterScript.
 * 
 * FIXES:
 *     IS_FILTERSCRIPT
 *     AllowInteriorWeapons
 *     AllowTeleport
 */

public OnGameModeInit()
{
	state _ALS : _ALS_go;
	
	#if FIXES_Single
		// Check this really IS the only script running.  Properties reset when
		// a gamemode restarts, so we don't have to worry about filterscipts
		// detecting themselves.
		if (FIXES_gsSettings & e_FIXES_SETTINGS_SECOND_USE)
		{
			if (existproperty(5, FIXES_gscSingleProperty))
			{
				print(FIXES_gscFixesError);
			}
			else
			{
				setproperty(5, FIXES_gscSingleProperty, 1);
			}
		}
		else
		{
			// When the server starts, we need to allow an FS to load with a GM
			// that doesn't use fixes.inc.  This is the only case where a
			// property can be set while a ganemode isn't running.  This will
			// mean that the FS won't complain if they are both using fixes, but
			// the GM still will, so all is good.
			FIXES_gsSettings |= e_FIXES_SETTINGS_SECOND_USE;
		}
	#endif
	
	// =============================
	//  BEGIN: AllowInteriorWeapons 
	// =============================
	// ======================
	//  BEGIN: AllowTeleport 
	// ======================
	#if FIX_AllowInteriorWeapons && FIX_AllowTeleport && !defined FILTERSCRIPT
		FIXES_gsSettings &= ~(e_FIXES_SETTINGS_INTERIOR | e_FIXES_SETTINGS_ADMIN_TELEPORT);
	#elseif FIX_AllowInteriorWeapons
		FIXES_gsSettings &= ~e_FIXES_SETTINGS_INTERIOR;
	#elseif FIX_AllowTeleport && !defined FILTERSCRIPT
		FIXES_gsSettings &= ~e_FIXES_SETTINGS_ADMIN_TELEPORT;
	#endif
	// ======================
	//  END:   AllowTeleport 
	// ======================
	// ===========================
	//  END: AllowInteriorWeapons 
	// ===========================
	
	#if FIX_GameText || !FIXES_Single
		if (!_FIXES_gIsFilterscript)
		{
			// =================
			//  BEGIN: GameText 
			// =================
			#if FIX_GameText
					for (new i = 0; i != sizeof (FIXES_gsPlayerPGTShown); ++i)
					{
						FIXES_gsPlayerPGTShown[i][MAX_PLAYERS] = MAX_PLAYERS;
					}
				#if FIXES_Single
					_FIXES_CreateGameTextDraws(INVALID_PLAYER_ID);
				#endif
			#endif
			// =================
			//  END:   GameText 
			// =================
			#if !FIXES_Single
				FIXES_DetermineOrder();
			#endif
		}
	#endif
	#if !FIXES_Single
		FIXES_gsSettings &= ~e_FIXES_SETTINGS_DROP_ALL_DATA;
	#endif
	
	return FIXES_OnGameModeInit();
}

#if defined _ALS_OnGameModeInit
	#error _ALS_OnGameModeInit defined
#endif
#define _ALS_OnGameModeInit
#define OnGameModeInit(%0) FIXES_OnGameModeInit(%0) <_ALS : _ALS_go>

_FIXES_FORWARD FIXES_OnGameModeInit();

/* 
 * OnGameModeExit()
 * 
 * Fast way of detecting not to retain any data.
 */

#if !FIXES_Single || FIX_PlayerDialogResponse
	public OnGameModeExit()
	{
		#if !FIXES_Single
			FIXES_gsSettings |= e_FIXES_SETTINGS_DROP_ALL_DATA;
			if (!_FIXES_gIsFilterscript && FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE)
			{
				FIXES_gsSettings |= e_FIXES_SETTINGS_ENDING,
				FIXES_DetermineOrder();
			}
		#endif
		
		// =============================
		//  BEGIN: PlayerDialogResponse 
		// =============================
		#if FIX_PlayerDialogResponse
			for (new playerid = 0; playerid != MAX_PLAYERS; ++playerid)
			{
				ShowPlayerDialog(playerid, -1, 0, FIXES_gscSpace, FIXES_gscSpace, FIXES_gscSpace, FIXES_gscSpace);
			}
		#endif
		// =============================
		//  END:   PlayerDialogResponse 
		// =============================
		
		return FIXES_OnGameModeExit();
	}
	
	#if defined _ALS_OnGameModeExit
		#error _ALS_OnGameModeExit defined
	#endif
	#define _ALS_OnGameModeExit
	#define OnGameModeExit(%0) FIXES_OnGameModeExit(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnGameModeExit();
#endif

/* 
 * OnFilterScriptExit()
 * 
 * Fast way of detecting not to retain any data.
 */

#if !FIXES_Single || FIX_GameText || FIX_OnPlayerDisconnect
	public OnFilterScriptExit()
	{
		#if FIX_OnPlayerDisconnect
			// Removal safe loop.
			for (new next, playerid = FIXES_gsPlayersIterator[MAX_PLAYERS]; playerid != MAX_PLAYERS; playerid = next)
			{
				next = FIXES_gsPlayersIterator[playerid],
				OnPlayerDisconnect(playerid, 4);
				#if FIXES_Single
					// =================
					//  BEGIN: GameText 
					// =================
					#if FIX_GameText
						#if FIX_GameTextStyles
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][13]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][12]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][11]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][10]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][9]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][8]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][7]),
						#endif
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][6]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][5]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][4]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][3]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][2]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][1]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][0]);
					#endif
				#endif
			}
		#endif
		#if FIXES_Single
			// =================
			//  BEGIN: GameText 
			// =================
			#if FIX_GameText
				#if defined FIXES_OnFilterScriptExit
					FIXES_OnFilterScriptExit();
				#endif
				#if !FIX_OnPlayerDisconnect
					_FIXES_FOREACH(FIXES_gsPlayersIterator, playerid)
					{
						#if FIX_GameTextStyles
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][13]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][12]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][11]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][10]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][9]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][8]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][7]),
						#endif
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][6]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][5]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][4]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][3]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][2]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][1]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][0]);
					}
				#endif
					return
						#if FIX_GameTextStyles
							TextDrawDestroy(FIXES_gsGTStyle[13]),
							TextDrawDestroy(FIXES_gsGTStyle[12]),
							TextDrawDestroy(FIXES_gsGTStyle[11]),
							TextDrawDestroy(FIXES_gsGTStyle[10]),
							TextDrawDestroy(FIXES_gsGTStyle[9]),
							TextDrawDestroy(FIXES_gsGTStyle[8]),
							TextDrawDestroy(FIXES_gsGTStyle[7]),
						#endif
							TextDrawDestroy(FIXES_gsGTStyle[6]),
							TextDrawDestroy(FIXES_gsGTStyle[5]),
							TextDrawDestroy(FIXES_gsGTStyle[4]),
							TextDrawDestroy(FIXES_gsGTStyle[3]),
							TextDrawDestroy(FIXES_gsGTStyle[2]),
							TextDrawDestroy(FIXES_gsGTStyle[1]),
							TextDrawDestroy(FIXES_gsGTStyle[0]);
			#else
					return FIXES_OnFilterScriptExit();
			#endif
			// =================
			//  END:   GameText 
			// =================
		#else
			if (FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE)
			{
				FIXES_gsSettings |= e_FIXES_SETTINGS_ENDING,
				FIXES_DetermineOrder();
			}
			return FIXES_OnFilterScriptExit();
		#endif
	}
	
	#if defined _ALS_OnFilterScriptExit
		#error _ALS_OnFilterScriptExit defined
	#endif
	#define _ALS_OnFilterScriptExit
	#define OnFilterScriptExit(%0) FIXES_OnFilterScriptExit(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnFilterScriptExit();
#endif

/* 
 * OnPlayerCommandText(playerid, cmdtext[])
 * 
 * FIXES:
 *     OnPlayerCommandText
 */

#if FIX_OnPlayerCommandText
	public OnPlayerCommandText(playerid, cmdtext[])
	{
		// ============================
		//  BEGIN: OnPlayerCommandText 
		// ============================
		#if FIX_OnPlayerCommandText
			// If this "#if" is around the whole function, it doesn't always
			// compile properly.
			return FIXES_OnPlayerCommandText(playerid, (cmdtext[0] <= '\1') ? FIXES_gscNULL : cmdtext);
		#endif
		// ============================
		//  END: OnPlayerCommandText 
		// ============================
	}
	
	#if defined _ALS_OnPlayerCommandText
		#error _ALS_OnPlayerCommandText defined
	#endif
	#define _ALS_OnPlayerCommandText
	#define OnPlayerCommandText(%0) FIXES_OnPlayerCommandText(%0) <_ALS : _ALS_go>
	
	forward FIXES_OnPlayerCommandText(playerid, cmdtext[]);
	public FIXES_OnPlayerCommandText(playerid, cmdtext[]) <_ALS : _ALS_x0, _ALS : _ALS_x1> { return 0; }
	public FIXES_OnPlayerCommandText(playerid, cmdtext[]) <> { return 0; }
#endif

/* 
 * OnPlayerConnect(playerid)
 * 
 * Almost every fix uses this callback for initialisation.  It is only
 * explicitly referenced for those fixes where this is all the code.  The
 * inclusion code was approaching the 512 line length limit, so I shrank it.
 * 
 * FIXES:
 *     TogglePlayerControllable
 *     SetPlayerWorldBounds
 *     GetPlayerColor
 *     SetPlayerName
 *     GetPlayerSkin
 *     IsPlayerInCheckpoint
 *     IsPlayerInRaceCheckpoint
 *     GetPlayerWeapon
 *     PutPlayerInVehicle
 *     OnPlayerEnterVehicle
 *     AllowTeleport
 *     OnDialogResponse
 *     SetSpawnInfo
 *     AllowInteriorWeapons
 *     TrainExit
 *     Kick
 *     OnPlayerEnterVehicle_2
 *     PlayerDialogResponse
 *     OnPlayerSpawn
 *     ApplyAnimation_2
 */

#if     FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable || FIX_SetPlayerName            || FIX_GetPlayerColour
	#define _FIXES_ON_PLAYER_CONNECT
#elseif FIX_GetPlayerSkin        || FIX_IsPlayerInCheckpoint     || FIX_IsPlayerInRaceCheckpoint || FIX_GetPlayerWeapon
	#define _FIXES_ON_PLAYER_CONNECT
#elseif FIX_PutPlayerInVehicle   || FIX_OnPlayerEnterVehicle     || FIX_AllowTeleport            || FIX_OnDialogResponse
	#define _FIXES_ON_PLAYER_CONNECT
#elseif FIX_GetPlayerDialog      || FIX_SetSpawnInfo             || FIX_AllowInteriorWeapons     || FIX_TrainExit
	#define _FIXES_ON_PLAYER_CONNECT
#elseif FIX_Kick                 || FIX_OnPlayerEnterVehicle_2   || FIX_PlayerDialogResponse     || FIX_GetPlayerInterior
	#define _FIXES_ON_PLAYER_CONNECT
#elseif FIX_OnPlayerSpawn        || FIX_ApplyAnimation_2         || FIX_GameText                 || _FIX_Menus
	#define _FIXES_ON_PLAYER_CONNECT
#elseif FIX_GetPlayerMenu        || FIX_OnPlayerDisconnect       || FIX_OnPlayerConnect          || FIX_SetPlayerTime
	#define _FIXES_ON_PLAYER_CONNECT
#elseif FIX_SetPlayerColour
	#define _FIXES_ON_PLAYER_CONNECT
#endif

#if defined _FIXES_ON_PLAYER_CONNECT
	public OnPlayerConnect(playerid)
	{
		// =========================
		//  BEGIN: ApplyAnimation_2 
		// =========================
		#if FIX_ApplyAnimation_2
			FIXES_gsPlayerAnimLibs[playerid][0] =
				FIXES_gsPlayerAnimLibs[playerid][1] =
					FIXES_gsPlayerAnimLibs[playerid][2] =
						FIXES_gsPlayerAnimLibs[playerid][3] =
							FIXES_gsPlayerAnimLibs[playerid][4] = -1;
		#endif
		// =======================
		//  END: ApplyAnimation_2 
		// =======================
		
		#if _FIX_Menus || FIX_GetPlayerMenu || FIX_OnPlayerDisconnect || FIX_GameText || FIX_AllowInteriorWeapons || FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable
			_FIXES_AddInternal(FIXES_gsPlayersIterator, playerid, MAX_PLAYERS);
		#endif
		
		FIXES_PRINTF("FIXES_OnPlayerConnect: %d", playerid);
			// This is only reset when the Game Mode changes or when a new
		#if FIX_Kick || FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable || FIX_PutPlayerInVehicle || FIX_OnPlayerEnterVehicle || FIX_AllowTeleport || FIX_AllowInteriorWeapons || FIX_TrainExit || FIX_OnPlayerEnterVehicle_2 || FIX_SetPlayerColour
			// player connects, which is what we want.  The other items are the
			// same, but must be done only once.  This variable will tend to
			// hold data relevant only in the master script (the first
			// filterscript).
			#if FIX_Kick || FIX_SetPlayerTime || FIX_SetPlayerColour
				FIXES_gsPlayerBools[playerid] = e_FIXES_BOOLS_ON_PLAYER_CONNECT;
			#else
				FIXES_gsPlayerBools[playerid] = e_FIXES_BOOLS_NONE;
			#endif
		#endif
		
		// =================
		//  BEGIN: GameText 
		// =================
		#if FIX_GameText
			#if !FIXES_Single
				if (FIXES_gsSettings & (e_FIXES_SETTINGS_NO_GAME_TEXT | e_FIXES_SETTINGS_IN_CHARGE) == e_FIXES_SETTINGS_IN_CHARGE)
			#endif
				{
					_FIXES_CreateGameTextDraws(playerid);
				}
		#endif
		// =================
		//  END:   GameText 
		// =================
		
		#if !FIXES_Single
			if (!GetPVarInt(playerid, FIXES_pvarNotNewPlayer))
		#endif
			{
				FIXES_PRINTF("FIXES_OnPlayerConnect: First (%d)", FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE);
				// ======================
				//  BEGIN: SetPlayerName 
				// ======================
				#if FIX_SetPlayerName
					new
						name[MAX_PLAYER_NAME];
					GetPlayerName(playerid, name, MAX_PLAYER_NAME);
					if (strcmp(name, FIXES_gscTempName) == 0)
					{
						Kick(playerid);
					}
				#endif
				// ======================
				//  END:   SetPlayerName 
				// ======================
				
				// =============================
				//  BEGIN: SetPlayerWorldBounds 
				// =============================
				#if FIX_SetPlayerWorldBounds && !FIXES_Single
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LX] =
						FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UX] =
							FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LY] =
								FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UY] =
									0.0;
				#endif
				// =============================
				//  END:   SetPlayerWorldBounds 
				// =============================
				
				// =======================
				//  BEGIN: GetPlayerColor 
				// =======================
				#if FIX_GetPlayerColour
					SetPlayerColor(playerid, FIXES_gscPlayerColours[playerid % 100]);
				#endif
				// =======================
				//  END:   GetPlayerColor 
				// =======================
				
				// ======================
				//  BEGIN: GetPlayerSkin 
				// ======================
				#if FIX_GetPlayerSkin
					#if FIXES_Single
						FIXES_gsPlayerSkin[playerid] = 0;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerSkin, 0);
					#endif
				#endif
				// ======================
				//  END:   GetPlayerSkin 
				// ======================
				
				// =============================
				//  BEGIN: IsPlayerInCheckpoint 
				// =============================
				#if FIX_IsPlayerInCheckpoint
					DisablePlayerCheckpoint(playerid);
				#endif
				// =============================
				//  END:   IsPlayerInCheckpoint 
				// =============================
				
				// =================================
				//  BEGIN: IsPlayerInRaceCheckpoint 
				// =================================
				#if FIX_IsPlayerInRaceCheckpoint
					DisablePlayerRaceCheckpoint(playerid);
				#endif
				// =================================
				//  END:   IsPlayerInRaceCheckpoint 
				// =================================
				
				// ========================
				//  BEGIN: GetPlayerWeapon 
				// ========================
				#if FIX_GetPlayerWeapon
					#if FIXES_Single
						FIXES_gsPlayerWeapon[playerid] = -1;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerWeapon, -1);
					#endif
				#endif
				// ========================
				//  END:   GetPlayerWeapon 
				// ========================
				
				// ==========================
				//  BEGIN: GetPlayerInterior 
				// ==========================
				#if FIX_GetPlayerInterior
					#if FIXES_Single
						FIXES_gsInterior[playerid] = 0;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerInterior, 0);
					#endif
				#endif
				// ==========================
				//  END:   GetPlayerInterior 
				// ==========================
				
				// =========================
				//  BEGIN: OnDialogResponse 
				// =========================
				#if FIX_OnDialogResponse || FIX_GetPlayerDialog
					#if FIXES_Single
						FIXES_gsDialogID[playerid] = INVALID_DIALOG_ID;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerDialog, INVALID_DIALOG_ID);
					#endif
				#endif
				// =========================
				//  END:   OnDialogResponse 
				// =========================
				
				// =============================
				//  BEGIN: PlayerDialogResponse 
				// =============================
				#if FIX_PlayerDialogResponse
					ShowPlayerDialog(playerid, -1, 0, FIXES_gscSpace, FIXES_gscSpace, FIXES_gscSpace, FIXES_gscSpace);
				#endif
				// =============================
				//  END:   PlayerDialogResponse 
				// =============================
				
				// =====================
				//  BEGIN: SetSpawnInfo 
				// =====================
				#if FIX_SetSpawnInfo
					SetSpawnInfo(playerid, NO_TEAM, 0, 0.0, 0.0, 0.0, 0.0, 0, 0, 0, 0, 0, 0);
				#endif
				// =====================
				//  END:   SetSpawnInfo 
				// =====================
				
				// ======================
				//  BEGIN: OnPlayerSpawn 
				// ======================
				#if FIX_OnPlayerSpawn
					#if FIXES_Single
						FIXES_gsLastCash[playerid] = 0;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerLastCash, 0);
					#endif
				#endif
				// ======================
				//  END:   OnPlayerSpawn 
				// ======================
				
				#if !FIXES_Single
					// No longer their first time.
					SetPVarInt(playerid, FIXES_pvarNotNewPlayer, 1);
				#endif
			}
		// =============
		//  BEGIN: Kick 
		// =============
		#if FIX_Kick || FIX_SetPlayerTime || FIX_SetPlayerColour
			new
				ret = FIXES_OnPlayerConnect(playerid);
			FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_ON_PLAYER_CONNECT;
			return ret;
		#else
			return FIXES_OnPlayerConnect(playerid);
		#endif
		// =============
		//  END:   Kick 
		// =============
	}
	
	#if defined _ALS_OnPlayerConnect
		#error _ALS_OnPlayerConnect defined
	#endif
	#define _ALS_OnPlayerConnect
	#define OnPlayerConnect(%0) FIXES_OnPlayerConnect(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerConnect(playerid);
#endif

/* 
 * OnPlayerDisconnect(playerid, reason)
 * 
 * FIXES:
 *     SetPlayerAttachedObject
 *     IsPlayerAttachedObjectSlotUsed
 *     Kick
 *     GetPlayerMenu
 */

#if FIX_SetPlayerAttachedObject || FIX_IsPlayerAttachedObjSlotUsed || FIX_Kick || (FIXES_SilentKick && (FIX_OnVehicleMod || FIX_OnPlayerEnterVehicle || FIX_OnPlayerEnterVehicle_2)) || FIX_GetPlayerMenu || _FIX_Menus || FIX_OnPlayerDisconnect || FIX_GameText || FIX_AllowInteriorWeapons || FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable || FIX_ApplyAnimation_2
	public OnPlayerDisconnect(playerid, reason)
	{
		new
			ret = FIXES_OnPlayerDisconnect(playerid, reason);
		
		// =========================
		//  BEGIN: ApplyAnimation_2 
		// =========================
		#if FIX_ApplyAnimation_2
			if (FIXES_gsAnimTimer[playerid])
			{
				KillTimer(FIXES_gsAnimTimer[playerid]),
				FIXES_gsAnimTimer[playerid] = 0;
			}
		#endif
		// =======================
		//  END: ApplyAnimation_2 
		// =======================
		
		// ======================
		//  BEGIN: GetPlayerMenu 
		// ======================
		#if FIX_GetPlayerMenu
			FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
		#endif
		// ======================
		//  END:   GetPlayerMenu 
		// ======================
		
		// ==========================
		//  BEGIN: FIXES_SilentKick 
		// ==========================
		#if FIXES_SilentKick && (FIX_OnVehicleMod || FIX_OnPlayerEnterVehicle || FIX_OnPlayerEnterVehicle_2)
			new
				shift = FIXES_gsPlayerIP[playerid];
			if (shift != -1)
			{
				// Unban the IP (timed out).
				new
					ip[16];
				format(ip, sizeof (ip), "unbanip %d.%d.%d.%d", shift >>> 24, (shift >>> 16) & 0xFF, (shift >>> 8) & 0xFF, shift & 0xFF),
				FIXES_gsPlayerIP[playerid] = -1;
			}
		#endif
		// ==========================
		//  END:   FIXES_SilentKick 
		// ==========================
		
		// ================================
		//  BEGIN: SetPlayerAttachedObject 
		// ================================
		#if FIX_SetPlayerAttachedObject
			#if !(FIXES_SilentKick && (FIX_OnVehicleMod || FIX_OnPlayerEnterVehicle || FIX_OnPlayerEnterVehicle_2))
				new
			#endif
					shift = playerid % _FIXES_ATTACHMENTS * MAX_PLAYER_ATTACHED_OBJECTS;
			#pragma tabsize 4
			new
				slot = playerid / _FIXES_ATTACHMENTS;
			#if !FIXES_Single
				if (!_FIXES_gIsFilterscript)
			#endif
				{
					for (new i = 0; i != MAX_PLAYER_ATTACHED_OBJECTS; ++i)
					{
						if (FIXES_gsObjectSlots[slot] & 1 << shift + i)
						{
							RemovePlayerAttachedObject(playerid, i);
						}
					}
				}
			#pragma tabsize 4
			FIXES_gsObjectSlots[slot] &= ~((1 << MAX_PLAYER_ATTACHED_OBJECTS) - 1 << shift);
		#endif
		// ================================
		//  END:   SetPlayerAttachedObject 
		// ================================
		
		// =======================================
		//  BEGIN: IsPlayerAttachedObjectSlotUsed 
		// =======================================
		#if FIX_IsPlayerAttachedObjSlotUsed && !FIX_SetPlayerAttachedObject
			FIXES_gsObjectSlots[playerid / _FIXES_ATTACHMENTS] &= ~((1 << MAX_PLAYER_ATTACHED_OBJECTS) - 1 << playerid % _FIXES_ATTACHMENTS * MAX_PLAYER_ATTACHED_OBJECTS);
		#endif
		// =======================================
		//  END:   IsPlayerAttachedObjectSlotUsed 
		// =======================================
		
		// =============
		//  BEGIN: Kick 
		// =============
		#if FIX_Kick
			if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_KICKED)
			{
				// Stop the timer if it is still running.
				KillTimer(GetPVarInt(playerid, FIXES_pvarKick));
			}
		#endif
		// =============
		//  END:   Kick 
		// =============
		
		#if _FIX_Menus || FIX_GetPlayerMenu || FIX_OnPlayerDisconnect || FIX_GameText || FIX_AllowInteriorWeapons || FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable
			_FIXES_RemoveInternal(FIXES_gsPlayersIterator, playerid, MAX_PLAYERS);
		#endif
		
		return ret;
	}
	
	#if defined _ALS_OnPlayerDisconnect
		#error _ALS_OnPlayerDisconnect defined
	#endif
	#define _ALS_OnPlayerDisconnect
	#define OnPlayerDisconnect(%0) FIXES_OnPlayerDisconnect(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerDisconnect(playerid, reason);
#endif

/* 
 * OnPlayerDeath(playerid, killerid, reason)
 * 
 * FIXES:
 *     OnPlayerDeath
 *     OnPlayerSpawn
 */

#if FIX_OnPlayerDeath || FIX_OnPlayerSpawn
	public OnPlayerDeath(playerid, killerid, reason)
	{
		_FIXES_IS_IN_CHARGE()
		{
			// ======================
			//  BEGIN: OnPlayerDeath 
			// ======================
			#if FIX_OnPlayerDeath
				static
					sAnimlib[32],
					sAnimname[32];
				GetAnimationName(FIXES_gsLastAnimation[playerid], sAnimlib, sizeof (sAnimlib), sAnimname, sizeof (sAnimname));
				if (strcmp(sAnimlib, "PED", true))
				{
					ClearAnimations(playerid);
				}
			#endif
			// ======================
			//  END:   OnPlayerDeath 
			// ======================
		}
		
		// ======================
		//  BEGIN: OnPlayerSpawn 
		// ======================
		#if FIX_OnPlayerSpawn
			new
				ret = FIXES_OnPlayerDeath(playerid, killerid, reason);
			
			#if FIXES_Single
				FIXES_gsLastCash[playerid] = GetPlayerMoney(playerid);
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerLastCash, GetPlayerMoney(playerid));
			#endif
			
			return ret;
		#else
			return FIXES_OnPlayerDeath(playerid, killerid, reason);
		#endif
		// ======================
		//  END:   OnPlayerSpawn 
		// ======================
	}
	
	#if defined _ALS_OnPlayerDeath
		#error _ALS_OnPlayerDeath defined
	#endif
	#define _ALS_OnPlayerDeath
	#define OnPlayerDeath(%0) FIXES_OnPlayerDeath(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerDeath(playerid, killerid, reason);
#endif

/* 
 * FIXES_SpawnPlayer(playerid)
 * 
 * FIXES:
 *     SpawnPlayer
 */

#if defined _ALS_SpawnPlayer
	#error _ALS_SpawnPlayer defined
#endif
native BAD_SpawnPlayer(playerid) = SpawnPlayer;

#if FIX_SpawnPlayer
	stock FIXES_SpawnPlayer(playerid)
	{
		// Valid "playerid" check inside "GetPlayerVehicleID".
		new
			vid = GetPlayerVehicleID(playerid);
		if (vid)
		{
			new
				Float:x,
				Float:y,
				Float:z;
			// Remove them without the animation.
			GetVehiclePos(vid, x, y, z),
			SetPlayerPos(playerid, x, y, z);
		}
		return SpawnPlayer(playerid);
	}
	
	#define _ALS_SpawnPlayer
	#define SpawnPlayer FIXES_SpawnPlayer
#endif

/* 
 * FIXES_SetPlayerName(playerid, name[])
 * 
 * FIXES:
 *     SetPlayerName
 */

#if defined _ALS_SetPlayerName
	#error _ALS_SetPlayerName defined
#endif
native BAD_SetPlayerName(playerid, const name[]) = SetPlayerName;

#if FIX_SetPlayerName
	stock FIXES_SetPlayerName(playerid, name[])
	{
		static
			sOldName[MAX_PLAYER_NAME];
		GetPlayerName(playerid, sOldName, sizeof (sOldName));
		if (!strcmp(name, sOldName, true))
		{
			if(strcmp(name, sOldName, false))
			{
				SetPlayerName(playerid, FIXES_gscTempName);
				if(SetPlayerName(playerid, name) == -1)
				{
					SetPlayerName(playerid, sOldName);
					return -1;
				}
				return 1;
			}
			else
			{
				return 0;
			}
		}
		return SetPlayerName(playerid, name);
	}
	
	#define _ALS_SetPlayerName
	#define SetPlayerName FIXES_SetPlayerName
#endif

/* 
 * OnPlayerRequestClass(playerid, classid)
 * 
 * FIXES:
 *      OnPlayerRequestClass
 */

#if FIX_OnPlayerRequestClass
	public OnPlayerRequestClass(playerid, classid)
	{
		// =============================
		//  BEGIN: OnPlayerRequestClass 
		// =============================
		#if FIX_OnPlayerRequestClass
			new
				Float:x,
				Float:y,
				Float:z;
			GetPlayerPos(playerid, x, y, z),
			RemoveBuildingForPlayer(playerid, 1484, x, y, z, 10.0),
			RemoveBuildingForPlayer(playerid, 1485, x, y, z, 10.0),
			RemoveBuildingForPlayer(playerid, 1486, x, y, z, 10.0);
		#endif
		// ===========================
		//  END: OnPlayerRequestClass 
		// ===========================
		return FIXES_OnPlayerRequestClass(playerid, classid);
	}
	
	#if defined _ALS_OnPlayerRequestClass
		#error _ALS_OnPlayerRequestClass defined
	#endif
	#define _ALS_OnPlayerRequestClass
	#define OnPlayerRequestClass(%0) FIXES_OnPlayerRequestClass(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerRequestClass(playerid, classid);
#endif

/* 
 * OnPlayerSpawn(playerid)
 * 
 * FIXES:
 *     GetPlayerSkin
 *     TogglePlayerControllable
 *     GetPlayerInterior
 *     OnPlayerSpawn
 *     GameText
 */

#if FIX_GetPlayerSkin || FIX_TogglePlayerControllable || FIX_GetPlayerInterior || FIX_OnPlayerSpawn || FIX_GameText
	public OnPlayerSpawn(playerid)
	{
		_FIXES_IS_IN_CHARGE()
		{
			// ======================
			//  BEGIN: OnPlayerSpawn 
			// ======================
			#if FIX_OnPlayerSpawn
				ResetPlayerMoney(playerid);
				#if FIXES_Single
					GivePlayerMoney(playerid, FIXES_gsLastCash[playerid]);
					FIXES_gsLastCash[playerid] = 0;
				#else
					GivePlayerMoney(playerid, GetPVarInt(playerid, FIXES_pvarPlayerLastCash));
					SetPVarInt(playerid, FIXES_pvarPlayerLastCash, 0);
				#endif
			#endif
			// ======================
			//  END:   OnPlayerSpawn 
			// ======================
			
			// ==========================
			//  BEGIN: GetPlayerInterior 
			// ==========================
			#if FIX_GetPlayerInterior
				#if FIXES_Single
					FIXES_gsInterior[playerid] = 0;
				#else
					SetPVarInt(playerid, FIXES_pvarPlayerInterior, 0);
				#endif
			#endif
			// ==========================
			//  END:   GetPlayerInterior 
			// ==========================
			
			// =================
			//  BEGIN: GameText 
			// =================
			#if FIX_GameText
				// Per-player GTs.
				
				#define _FIXES_PER_PLAYER_GT(%0) if (FIXES_gsPlayerPGTShown[%0][playerid] > playerid) PlayerTextDrawHide(playerid, FIXES_gsPGTStyle[playerid][%0]), _FIXES_RemoveInternal(FIXES_gsPlayerPGTShown[%0], playerid, MAX_PLAYERS)
				
				#if FIX_GameTextStyles
					_FIXES_PER_PLAYER_GT(13);
					_FIXES_PER_PLAYER_GT(12);
					_FIXES_PER_PLAYER_GT(11);
					_FIXES_PER_PLAYER_GT(10);
					_FIXES_PER_PLAYER_GT(9);
					_FIXES_PER_PLAYER_GT(8);
					_FIXES_PER_PLAYER_GT(7);
				#endif
				_FIXES_PER_PLAYER_GT(6);
				_FIXES_PER_PLAYER_GT(5);
				_FIXES_PER_PLAYER_GT(4);
				_FIXES_PER_PLAYER_GT(3);
				_FIXES_PER_PLAYER_GT(2);
				_FIXES_PER_PLAYER_GT(1);
				_FIXES_PER_PLAYER_GT(0);
				
				#undef _FIXES_PER_PLAYER_GT
				
				// Global GTs.
				#if FIX_GameTextStyles
					TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[13]),
					TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[12]),
					TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[11]),
					TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[10]),
					TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[9]),
					TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[8]),
					TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[7]);
				#endif
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[6]),
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[5]),
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[4]),
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[3]),
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[2]),
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[1]),
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[0]);
				
			#endif
			// =================
			//  END:   GameText 
			// =================
		}
		
		// ======================
		//  BEGIN: GetPlayerSkin 
		// ======================
		#if FIX_GetPlayerSkin
			#if FIXES_Single
				FIXES_gsPlayerSkin[playerid] = GetPlayerSkin(playerid);
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerSkin, GetPlayerSkin(playerid));
			#endif
		#endif
		// ======================
		//  END:   GetPlayerSkin 
		// ======================
		
		// =================================
		//  BEGIN: TogglePlayerControllable 
		// =================================
		#if FIX_TogglePlayerControllable
			FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_UNCONTROLLABLE;
		#endif
		// =================================
		//  END:   TogglePlayerControllable 
		// =================================
		
		return FIXES_OnPlayerSpawn(playerid);
	}
	
	#if defined _ALS_OnPlayerSpawn
		#error _ALS_OnPlayerSpawn defined
	#endif
	#define _ALS_OnPlayerSpawn
	#define OnPlayerSpawn(%0) FIXES_OnPlayerSpawn(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerSpawn(playerid);
#endif

/* 
 * OnVehicleMod(playerid, vehicleid, componentid)
 * 
 * FIXES:
 *     OnVehicleMod
 */

#if FIX_OnVehicleMod
	public OnVehicleMod(playerid, vehicleid, componentid)
	{
		#if !FIXES_Single
			if (!(FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE))
			{
				return FIXES_OnVehicleMod(playerid, vehicleid, componentid);
			}
		#endif
		
		// =====================
		//  BEGIN: OnVehicleMod 
		// =====================
		new
			modelid = GetVehicleModel(vehicleid);
		if (_FIXES_IN_RANGE(modelid, 400, 611 + 1))
		{
			switch (componentid)
			{
				case 1000 .. 1191:
				{
					if (FIXES_gscVehicleMods[(modelid - 400) * 6 + (componentid - 1000 >>> 5)] & 1 << (componentid - 1000 & 0b00011111))
					{
						return FIXES_OnVehicleMod(playerid, vehicleid, componentid);
					}
				}
				case 1192, 1193:
				{
					if (modelid == 576)
					{
						// This save a whole cell off EVERY other vehicle!  This
						// is the ONLY vehicle with any mods over "6 * 32 + 999"
						// (1191), the highest value you can fit in 6 cells of a
						// bit array (minus 1000).
						return FIXES_OnVehicleMod(playerid, vehicleid, componentid);
					}
				}
			}
		}
		// Desync the player entirely.
		FIXES_BlockUpdate(playerid, true);
		return 0;
		// =====================
		//  END:   OnVehicleMod 
		// =====================
	}
	
	#if defined _ALS_OnVehicleMod
		#error _ALS_OnVehicleMod defined
	#endif
	#define _ALS_OnVehicleMod
	#define OnVehicleMod(%0) FIXES_OnVehicleMod(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnVehicleMod(playerid, vehicleid, componentid);
#endif

/* 
 * FIXES_SetPlayerSkin(playerid, skinid)
 * 
 * FIXES:
 *     GetPlayerSkin
 *     SetPlayerSkin
 */

#if defined _ALS_SetPlayerSkin
	#error _ALS_SetPlayerSkin defined
#endif
native BAD_SetPlayerSkin(playerid, skinid) = SetPlayerSkin;

#if FIX_GetPlayerSkin || FIX_SetPlayerSkin
	stock FIXES_SetPlayerSkin(playerid, skinid)
	{
		// ======================
		//  BEGIN: SetPlayerSkin 
		// ======================
		#if FIX_SetPlayerSkin
			new
				vehicleid = GetPlayerVehicleID(playerid);
			if (vehicleid)
			{
				new
					seat = GetPlayerVehicleSeat(playerid);
				ClearAnimations(playerid);
				new
					ret = SetPlayerSkin(playerid, skinid);
				PutPlayerInVehicle(playerid, vehicleid, seat);
				
				// ======================
				//  BEGIN: GetPlayerSkin 
				// ======================
				#if FIX_GetPlayerSkin
					#if FIXES_Single
						FIXES_gsPlayerSkin[playerid] = skinid;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerSkin, skinid);
					#endif
				#endif
				// ======================
				//  END:   GetPlayerSkin 
				// ======================
				
				return ret;
			}
			
			// ======================
			//  BEGIN: GetPlayerSkin 
			// ======================
			#if FIX_GetPlayerSkin
				else
			#endif
			// ======================
			//  END:   GetPlayerSkin 
			// ======================
		#endif
		// ======================
		//  END:   SetPlayerSkin 
		// ======================
		
		// ======================
		//  BEGIN: GetPlayerSkin 
		// ======================
		#if FIX_GetPlayerSkin
			// Watch fot the "else" above if this code ever changes.
			#if FIXES_Single
				FIXES_gsPlayerSkin[playerid] = skinid;
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerSkin, skinid);
			#endif
		#endif
		// ======================
		//  END:   GetPlayerSkin 
		// ======================
		
		return SetPlayerSkin(playerid, skinid);
	}
	
	#define _ALS_SetPlayerSkin
	#define SetPlayerSkin FIXES_SetPlayerSkin
#endif

/* 
 * FIXES_GetPlayerSkin(playerid, skinid)
 * 
 * FIXES:
 *     GetPlayerSkin
 */

#if defined _ALS_GetPlayerSkin
	#error _ALS_GetPlayerSkin defined
#endif
native BAD_GetPlayerSkin(playerid) = GetPlayerSkin;

#if FIX_GetPlayerSkin
	stock FIXES_GetPlayerSkin(playerid)
	{
		#if FIXES_Single
			return FIXES_gsPlayerSkin[playerid];
		#else
			return GetPVarInt(playerid, FIXES_pvarPlayerSkin);
		#endif
	}
	
	#define _ALS_GetPlayerSkin
	#define GetPlayerSkin FIXES_GetPlayerSkin
#endif

/* 
 * FIXES_GetWeaponName(weaponid, weapon[], len)
 * 
 * FIXES:
 *     GetWeaponName
 */

#if defined _ALS_GetWeaponName
	#error _ALS_GetWeaponName defined
#endif
native BAD_GetWeaponName(weaponid, weapon[], len) = GetWeaponName;

#if FIX_GetWeaponName
	stock FIXES_GetWeaponName(weaponid, weapon[], len)
	{
		switch (weaponid)
		{
			case 18:
			{
				return
					weapon[0] = 0,
					strcat(weapon, "Molotov Cocktail", len),
					1;
			}
			case 44:
			{
				return
					weapon[0] = 0,
					strcat(weapon, "Night Vision Goggles", len),
					1;
			}
			case 45:
			{
				return
					weapon[0] = 0,
					strcat(weapon, "Thermal Goggles", len),
					1;
			}
			default:
			{
				return GetWeaponName(weaponid, weapon, len);
			}
		}
		return 0;
	}
	
	#define _ALS_GetWeaponName
	#define GetWeaponName FIXES_GetWeaponName
#endif

/* 
 * OnPlayerUpdate(playerid)
 * 
 * FIXES:
 *     SetPlayerWorldBounds
 *     TogglePlayerControllable
 *     AllowInteriorWeapons
 *     OnPlayerDeath
 *     Kick
 *     OnVehicleMod
 *     OnPlayerEnterVehicle
 *     OnPlayerEnterVehicle_2
 */

#if FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable || FIX_AllowInteriorWeapons || FIX_OnPlayerEnterVehicle || FIX_OnPlayerEnterVehicle_2 || FIX_OnPlayerDeath || FIX_Kick || FIX_OnVehicleMod
	public OnPlayerUpdate(playerid)
	{
		// =============================
		//  BEGIN: OnPlayerEnterVehicle 
		// =============================
		#if FIX_OnPlayerEnterVehicle || FIX_Kick || FIX_OnVehicleMod || FIX_OnPlayerEnterVehicle_2
			if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_BLOCK)
			{
				return 0;
			}
		#endif
		// =============================
		//  END:   OnPlayerEnterVehicle 
		// =============================
		
		#if !FIXES_Single
			if (!(FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE))
			{
				return FIXES_OnPlayerUpdate(playerid);
			}
		#endif
		
		#if FIX_OnPlayerEnterVehicle || FIX_AllowInteriorWeapons || FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable
			new
				e_FIXES_BOOLS:pbools = FIXES_gsPlayerBools[playerid];
		#endif
		
		// =============================
		//  BEGIN: AllowInteriorWeapons 
		// =============================
		#if FIX_AllowInteriorWeapons
			if (pbools & e_FIXES_BOOLS_INTERIOR)
			{
				// DONE: May need integration with "FIX_GetPlayerWeapon".
				SetPlayerArmedWeapon(playerid, 0);
			}
		#endif
		// ============================
		//  END: AllowInteriorWeapons 
		// ============================
		
		#if FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable
			new
				ud,
				lr,
				keys;
			GetPlayerKeys(playerid, keys, ud, lr);
		#endif
		
		#if FIX_SetPlayerWorldBounds && !FIX_TogglePlayerControllable
			#pragma unused ud, lr
		#endif
		
		// =============================
		//  BEGIN: SetPlayerWorldBounds 
		// =============================
		#if FIX_SetPlayerWorldBounds
			if (pbools & e_FIXES_BOOLS_WORLDBOUNDS && keys & _FIXES_KEY_AIM)
			{
				new
					Float:x,
					Float:y,
					Float:z;
				GetPlayerPos(playerid, x, y, z);
				if (FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LX] < x < FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UX] && FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LY] < y < FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UY])
				{
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PX] = x,
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PY] = y,
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PZ] = z;
				}
				else
				{
					SetPlayerPos(playerid, FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PX], FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PY], FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PZ]);
				}
			}
		#endif
		// =============================
		//  END:   SetPlayerWorldBounds 
		// =============================
		
		// ======================
		//  BEGIN: OnPlayerDeath 
		// ======================
		#if FIX_OnPlayerDeath
			FIXES_gsLastAnimation[playerid] = GetPlayerAnimationIndex(playerid);
		#endif
		// ======================
		//  END:   OnPlayerDeath 
		// ======================
		
		// =================================
		//  BEGIN: TogglePlayerControllable 
		// =================================
		#if FIX_TogglePlayerControllable
			// MUST come last.
			if (pbools & e_FIXES_BOOLS_UNCONTROLLABLE)
			{
				// Keys based sync blocking, modified based on a post by Slice:
				// http://forum.sa-mp.com/showpost.php?p=876854  Use "playerid"
				// here as it's a variable that already exists, but note that
				// after this point, you CANNOT use "playerid" as it has been
				// "clobbered" (and yes, that's the technical term).  This is
				// just to avoid "statement has no effect" warnings.  ALWAYS
				// return 0 in this case, regardless of what other callbacks
				// return.
				playerid = FIXES_OnPlayerUpdate(playerid);
				if (keys || ud || lr)
				{
					return 0;
				}
				else
				{
					return playerid;
				}
			}
		#endif
		// =================================
		//  END:   TogglePlayerControllable 
		// =================================
		
		return FIXES_OnPlayerUpdate(playerid);
	}
	
	#if defined _ALS_OnPlayerUpdate
		#error _ALS_OnPlayerUpdate defined
	#endif
	#define _ALS_OnPlayerUpdate
	#define OnPlayerUpdate(%0) FIXES_OnPlayerUpdate(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerUpdate(playerid);
#endif

/* 
 * FIXES_SetPlayerWorldBounds(playerid, Float:x_max, Float:x_min, Float:y_max, Float:y_min)
 * 
 * TODO: Modify this to remove aim animations when they get to the boundaries.
 * 
 * FIXES:
 *     SetPlayerWorldBounds
 */

#if defined _ALS_SetPlayerWorldBounds
	#error _ALS_SetPlayerWorldBounds defined
#endif
native BAD_SetPlayerWorldBounds(playerid, Float:x_max, Float:x_min, Float:y_max, Float:y_min) = SetPlayerWorldBounds;

#if FIX_SetPlayerWorldBounds
	#if FIXES_Single
		stock FIXES_SetPlayerWorldBounds(playerid, Float:x_max, Float:x_min, Float:y_max, Float:y_min)
		{
			if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
			{
				FIXES_PRINTF("Call SetPlayerWorldBounds");
				if (x_max == x_min || y_max == y_min)
				{
					// Impossibly small area, disable checks entirely.
					return
						FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_WORLDBOUNDS,
						SetPlayerWorldBounds(playerid, _FIXES_INFINITY, _FIXES_N_INFINITY, _FIXES_INFINITY, _FIXES_N_INFINITY);
				}
				// This code could do with a way to mostly remove the checks.  Maybe
				// when setting everything to FIXES_INFINITY (with default
				// parameters).
				FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_WORLDBOUNDS;
				new
					Float:tmp;
				if (x_max < x_min)
				{
					tmp = x_min,
					x_min = x_max,
					x_max = tmp;
				}
				if (y_max < y_min)
				{
					tmp = y_min,
					y_min = y_max,
					y_max = tmp;
				}
				// Give a little leway so this fix isn't noticed if you're not
				// trying to break through the world bounds.  Leway removed in
				// favour of keys.
				return
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LX] = x_min,
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UX] = x_max,
					
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LY] = y_min,
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UY] = y_max,
					
					GetPlayerPos(playerid, tmp, tmp, tmp),
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PX] = (x_max - x_min) / 2 + x_min,
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PY] = (y_max - y_min) / 2 + y_min,
					FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PZ] = tmp,
					
					SetPlayerWorldBounds(playerid, x_max, x_min, y_max, y_min);
			}
			return 0;
		}
	#else
		forward _FIXES_SetPlayerWorldBounds(playerid, Float:x_max, Float:x_min, Float:y_max, Float:y_min);
		
		stock FIXES_SetPlayerWorldBounds(playerid, Float:x_max, Float:x_min, Float:y_max, Float:y_min)
		{
			if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
			{
				FIXES_PRINTF("Call SetPlayerWorldBounds");
				return
					CallRemoteFunction(FIXES_gscSetPlayerWorldBounds, FIXES_gscSpec@iffff, playerid, x_max, x_min, y_max, y_min),
					getproperty(5, FIXES_gscReturnProperty);
			}
			return 0;
		}
		
		public _FIXES_SetPlayerWorldBounds(playerid, Float:x_max, Float:x_min, Float:y_max, Float:y_min)
		{
			FIXES_PRINTF("_FIXES_SetPlayerWorldBounds: %d %.2f %.2f %.2f %.2f", playerid, x_max, x_min, y_max, y_min);
			_FIXES_IS_IN_CHARGE()
			{
				FIXES_PRINTF("SetPlayerWorldBounds");
				//if (x_max == _FIXES_INFINITY && x_min == _FIXES_INFINITY && y_max == _FIXES_INFINITY && y_min == _FIXES_INFINITY)
				if (x_max == x_min || y_max == y_min)
				{
					// Impossibly small area, disable checks entirely.
					return
						FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_WORLDBOUNDS,
						FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LX] =
							FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UX] =
								FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LY] =
									FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UY] =
										0.0,
						setproperty(5, FIXES_gscReturnProperty, SetPlayerWorldBounds(playerid, _FIXES_INFINITY, _FIXES_N_INFINITY, _FIXES_INFINITY, _FIXES_N_INFINITY)),
						-1;
				}
				// This code could do with a way to mostly remove the checks.  Maybe
				// when setting everything to FIXES_INFINITY (with default
				// parameters).
				FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_WORLDBOUNDS;
				new
					Float:tmp;
				if (x_max < x_min)
				{
					tmp = x_min,
					x_min = x_max,
					x_max = tmp;
				}
				if (y_max < y_min)
				{
					tmp = y_min,
					y_min = y_max,
					y_max = tmp;
				}
				// Give a little leway so this fix isn't noticed if you're not
				// trying to break through the world bounds.  Leway removed in
				// favour of keys.
				FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LX] = x_min,
				FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UX] = x_max,
				
				FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_LY] = y_min,
				FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_UY] = y_max,
				
				GetPlayerPos(playerid, tmp, tmp, tmp),
				FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PX] = (x_max - x_min) / 2 + x_min,
				FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PY] = (y_max - y_min) / 2 + y_min,
				FIXES_gsWorldbounds[playerid][E_FIXES_WORLDBOUND_DATA_PZ] = tmp,
				
				setproperty(5, FIXES_gscReturnProperty, SetPlayerWorldBounds(playerid, x_max, x_min, y_max, y_min));
			}
			return 0;
		}
	#endif
	
	#define _ALS_SetPlayerWorldBounds
	#define SetPlayerWorldBounds FIXES_SetPlayerWorldBounds
#endif

/* 
 * FIXES_TogglePlayerControllable(playerid, toggle)
 * 
 * FIXES:
 *     TogglePlayerControllable
 */

#if defined _ALS_TogglePlayerControllable
	#error _ALS_TogglePlayerControllable defined
#endif
native BAD_TogglePlayerControllable(playerid, toggle) = TogglePlayerControllable;

#if FIX_TogglePlayerControllable
	#if FIXES_Single
		stock FIXES_TogglePlayerControllable(playerid, toggle)
		{
			if (TogglePlayerControllable(playerid, toggle))
			{
				if (toggle)
				{
					FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_UNCONTROLLABLE;
				}
				else
				{
					FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_UNCONTROLLABLE;
				}
				return 1;
			}
			return 0;
		}
	#else
		forward _FIXES_TogglePlayerControllable(playerid, toggle);
		
		stock FIXES_TogglePlayerControllable(playerid, toggle)
		{
			if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
			{
				return
					CallRemoteFunction(FIXES_gscTogglePlayerControl, FIXES_gscSpec@ii, playerid, toggle),
					getproperty(5, FIXES_gscReturnProperty);
			}
			return 0;
		}
		
		public _FIXES_TogglePlayerControllable(playerid, toggle)
		{
			_FIXES_IS_IN_CHARGE()
			{
				if (toggle)
				{
					FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_UNCONTROLLABLE;
				}
				else
				{
					FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_UNCONTROLLABLE;
				}
				setproperty(5, FIXES_gscReturnProperty, TogglePlayerControllable(playerid, toggle));
			}
		}
	#endif
	
	#define _ALS_TogglePlayerControllable
	#define TogglePlayerControllable FIXES_TogglePlayerControllable
#endif

/* 
 * OnPlayerInteriorChange(playerid, newinteriorid, oldinteriorid)
 * 
 * FIXES:
 *     AllowInteriorWeapons
 *     GetPlayerInterior
 */

#if FIX_AllowInteriorWeapons || FIX_GetPlayerInterior
	public OnPlayerInteriorChange(playerid, newinteriorid, oldinteriorid)
	{
		// ==========================
		//  BEGIN: GetPlayerInterior 
		// ==========================
		#if FIX_GetPlayerInterior
			#if FIXES_Single
				FIXES_gsInterior[playerid] = newinteriorid;
			#else
				_FIXES_IS_IN_CHARGE()
				{
					SetPVarInt(playerid, FIXES_pvarPlayerInterior, newinteriorid);
				}
			#endif
		#endif
		// ==========================
		//  END:   GetPlayerInterior 
		// ==========================
		
		// =============================
		//  BEGIN: AllowInteriorWeapons 
		// =============================
		#if FIX_AllowInteriorWeapons
			if (newinteriorid)
			{
				if (FIXES_gsSettings & e_FIXES_SETTINGS_INTERIOR)
				{
					// This only needs doing here if "GetPlayerWeapon" is set.
					// If it isn't, even though "AllowInteriorWeapons" can use
					// this code on its own, it doesn't as "OnPlayerUpdate"
					// takes care of it all.
					// TODO: Rewrite this code to use a single bit to indicate
					// if a player can have a weapon or not, and just use the in
					// the "OnPlayerUpdate" callback, instead of several checks.
					// DONE!
					FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_INTERIOR;
					_FIXES_IS_IN_CHARGE()
					{
						SetPlayerArmedWeapon(playerid, 0);
						#if FIX_GetPlayerWeapon
							if (IsPlayerInAnyVehicle(playerid))
							{
								#if FIXES_Single
									FIXES_gsPlayerWeapon[playerid] = 0;
								#else
									SetPVarInt(playerid, FIXES_pvarPlayerWeapon, 0);
								#endif
							}
						#endif
					}
				}
			}
			else
			{
				FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_INTERIOR;
			}
		#endif
		// ===========================
		//  END: AllowInteriorWeapons 
		// ===========================
		return FIXES_OnPlayerInteriorChange(playerid, newinteriorid, oldinteriorid);
	}
	
	#if defined _ALS_OnPlayerInteriorChange
		#error _ALS_OnPlayerInteriorChange defined
	#endif
	#define _ALS_OnPlayerInteriorChange
	#define OnPlayerInteriorChange(%0) FIXES_OnPlayerInteriorChange(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerInteriorChange(playerid, newinteriorid, oldinteriorid);
#endif

/* 
 * _FIXES_DriveBy(playerid)
 * 
 * FIXES:
 *     DriveBy
 */

#if FIX_DriveBy
	forward _FIXES_DriveBy(playerid);
	
	public  _FIXES_DriveBy(playerid)
	{
		if (GetPlayerState(playerid) == PLAYER_STATE_PASSENGER)
		{
			SetPlayerArmedWeapon(playerid, FIXES_gsDriveByWeapon[playerid]);
		}
		return FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_DRIVE_BY;
	}
#endif

/* 
 * OnPlayerKeyStateChange(playerid, newkeys, oldkeys)
 * 
 * FIXES:
 *     DriveBy
 */

#if FIX_DriveBy
	public OnPlayerKeyStateChange(playerid, newkeys, oldkeys)
	{
		// ===============================
		//  BEGIN: DriveBy 
		// ===============================
		#if FIX_DriveBy
			#if !FIXES_Single
				if (FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE)
			#endif
				{
					if ((newkeys & KEY_CROUCH) && !(oldkeys & KEY_CROUCH) && GetPlayerState(playerid) == PLAYER_STATE_PASSENGER && GetPlayerCameraMode(playerid) == 55 && !(FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_DRIVE_BY))
					{
						FIXES_gsDriveByWeapon[playerid] = GetPlayerWeapon(playerid),
						FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_DRIVE_BY,
						SetPlayerArmedWeapon(playerid, 0),
						ApplyAnimation(playerid, "PED", "CAR_GETIN_RHS", 4.1, 0, 0, 0, 0, 1, 1),
						SetTimerEx(FIXES_gscDriveBy, 500, 0, FIXES_gscSpec@i, playerid);
					}
				}
		#endif
		// =============================
		//  END: DriveBy 
		// =============================
		
		return FIXES_OnPlayerKeyStateChange(playerid, newkeys, oldkeys);
	}
	
	#if defined _ALS_OnPlayerKeyStateChange
		#error _ALS_OnPlayerKeyStateChange defined
	#endif
	#define _ALS_OnPlayerKeyStateChange
	#define OnPlayerKeyStateChange(%0) FIXES_OnPlayerKeyStateChange(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerKeyStateChange(playerid, newkeys, oldkeys);
#endif

/* 
 * OnPlayerEnterVehicle(playerid, vehicleid, ispassenger)
 * 
 * FIXES:
 *     OnPlayerEnterVehicle_2
 */

#if FIX_OnPlayerEnterVehicle_2
	public OnPlayerEnterVehicle(playerid, vehicleid, ispassenger)
	{
		// ===============================
		//  BEGIN: OnPlayerEnterVehicle_2 
		// ===============================
		#if FIX_OnPlayerEnterVehicle_2
			if (0 < vehicleid < MAX_VEHICLES)
			{
				return FIXES_OnPlayerEnterVehicle(playerid, vehicleid, ispassenger);
			}
			FIXES_BlockUpdate(playerid, true);
			return 0;
		#endif
		// =============================
		//  END: OnPlayerEnterVehicle_2 
		// =============================
	}
	
	#if defined _ALS_OnPlayerEnterVehicle
		#error _ALS_OnPlayerEnterVehicle defined
	#endif
	#define _ALS_OnPlayerEnterVehicle
	#define OnPlayerEnterVehicle(%0) FIXES_OnPlayerEnterVehicle(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerEnterVehicle(playerid, vehicleid, ispassenger);
#endif

/* 
 * OnPlayerExitVehicle(playerid, vehicleid)
 * 
 * FIXES:
 *     OnPlayerEnterVehicle_2
 */

#if FIX_OnPlayerEnterVehicle_2
	public OnPlayerExitVehicle(playerid, vehicleid)
	{
		// ===============================
		//  BEGIN: OnPlayerEnterVehicle_2 
		// ===============================
		#if FIX_OnPlayerEnterVehicle_2
			if (0 < vehicleid < MAX_VEHICLES)
			{
				return FIXES_OnPlayerExitVehicle(playerid, vehicleid);
			}
			FIXES_BlockUpdate(playerid, true);
			return 0;
		#endif
		// =============================
		//  END: OnPlayerEnterVehicle_2 
		// =============================
	}
	
	#if defined _ALS_OnPlayerExitVehicle
		#error _ALS_OnPlayerExitVehicle defined
	#endif
	#define _ALS_OnPlayerExitVehicle
	#define OnPlayerExitVehicle(%0) FIXES_OnPlayerExitVehicle(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerExitVehicle(playerid, vehicleid);
#endif

/* 
 * FIXES_BlockUpdate(playerid, bool:block)
 * 
 * Block a player from using OnPlayerUpdate at all.
 */

#if FIX_OnPlayerEnterVehicle || FIX_OnPlayerEnterVehicle_2 || FIX_Kick || FIX_OnVehicleMod
	static stock FIXES_BlockUpdate(playerid, bool:block, kick = 6)
	{
		if (kick)
		{
			#if FIXES_SilentKick
				new
					ip[22];
				GetPlayerIp(playerid, ip, sizeof (ip)),
				format(ip, sizeof (ip), "banip %s", ip),
				SendRconCommand(ip),
				FIXES_gsPlayerIP[playerid] = strval(ip[kick]) << 24,
				kick = strfind(ip, FIXES_gscDot, false, kick) + 1,
				FIXES_gsPlayerIP[playerid] |= strval(ip[kick]) << 16,
				kick = strfind(ip, FIXES_gscDot, false, kick) + 1,
				FIXES_gsPlayerIP[playerid] |= strval(ip[kick]) << 8,
				kick = strfind(ip, FIXES_gscDot, false, kick) + 1,
				FIXES_gsPlayerIP[playerid] |= strval(ip[kick]);
			#else
				Kick(playerid);
			#endif
		}
		if (block)
		{
			FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_BLOCK;
			return 0;
		}
		else
		{
			FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_BLOCK;
			return 1;
		}
	}
#endif

/* 
 * OnPlayerSelectedMenuRow(playerid, row)
 * 
 * FIXES:
 *     GetPlayerMenu
 */

#if FIX_GetPlayerMenu
	public OnPlayerSelectedMenuRow(playerid, row)
	{
		// ======================
		//  BEGIN: GetPlayerMenu 
		// ======================
		#if FIXES_Single
			FIXES_gsSettings &= ~e_FIXES_SETTINGS_MENU_SET;
			new
				ret = FIXES_OnPlayerSelectedMenuRow(playerid, row);
			if (!(FIXES_gsSettings & e_FIXES_SETTINGS_MENU_SET))
			{
				FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
			}
			return ret;
		#else
			new
				Menu:cur = FIXES_gsCurrentMenu[playerid];
			_FIXES_IS_IN_CHARGE()
			{
				setproperty(5, FIXES_gscMenuProperty, _:cur);
				CallRemoteFunction(FIXES_gscClearPlayerMenu, FIXES_gscSpec@i, playerid);
				FIXES_gsCurrentMenu[playerid] = cur;
				new
					ret = FIXES_OnPlayerSelectedMenuRow(playerid, row);
				if (!(FIXES_gsSettings & e_FIXES_SETTINGS_MENU_SET))
				{
					FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
				}
				return ret;
			}
			else
			{
				FIXES_gsCurrentMenu[playerid] = Menu:getproperty(5, FIXES_gscMenuProperty);
				FIXES_gsSettings &= ~e_FIXES_SETTINGS_MENU_SET;
				new
					ret = FIXES_OnPlayerSelectedMenuRow(playerid, row);
				if (!(FIXES_gsSettings & e_FIXES_SETTINGS_MENU_SET))
				{
					FIXES_gsCurrentMenu[playerid] = cur;
				}
				return ret;
			}
		#endif
		// ======================
		//  END:   GetPlayerMenu 
		// ======================
	}
	
	#if defined _ALS_OnPlayerSelectedMenuRow
		#error _ALS_OnPlayerSelectedMenuRow defined
	#endif
	#define _ALS_OnPlayerSelectedMenuRow
	#define OnPlayerSelectedMenuRow(%0) FIXES_OnPlayerSelectedMenuRow(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerSelectedMenuRow(playerid, row);
#endif

/* 
 * OnPlayerExitedMenu(playerid)
 * 
 * FIXES:
 *     GetPlayerMenu
 */

#if FIX_GetPlayerMenu
	public OnPlayerExitedMenu(playerid)
	{
		// ======================
		//  BEGIN: GetPlayerMenu 
		// ======================
		#if FIXES_Single
			FIXES_gsSettings &= ~e_FIXES_SETTINGS_MENU_SET;
			new
				ret = FIXES_OnPlayerExitedMenu(playerid);
			if (!(FIXES_gsSettings & e_FIXES_SETTINGS_MENU_SET))
			{
				FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
			}
			return ret;
		#else
			new
				Menu:cur = FIXES_gsCurrentMenu[playerid];
			_FIXES_IS_IN_CHARGE()
			{
				setproperty(5, FIXES_gscMenuProperty, _:cur);
				CallRemoteFunction(FIXES_gscClearPlayerMenu, FIXES_gscSpec@i, playerid);
				FIXES_gsCurrentMenu[playerid] = cur;
				new
					ret = FIXES_OnPlayerExitedMenu(playerid);
				if (!(FIXES_gsSettings & e_FIXES_SETTINGS_MENU_SET))
				{
					FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
				}
				return ret;
			}
			else
			{
				FIXES_gsCurrentMenu[playerid] = Menu:getproperty(5, FIXES_gscMenuProperty);
				FIXES_gsSettings &= ~e_FIXES_SETTINGS_MENU_SET;
				new
					ret = FIXES_OnPlayerExitedMenu(playerid);
				if (!(FIXES_gsSettings & e_FIXES_SETTINGS_MENU_SET))
				{
					FIXES_gsCurrentMenu[playerid] = cur;
				}
				return ret;
			}
		#endif
		// ======================
		//  END:   GetPlayerMenu 
		// ======================
	}
	
	#if defined _ALS_OnPlayerExitedMenu
		#error _ALS_OnPlayerExitedMenu defined
	#endif
	#define _ALS_OnPlayerExitedMenu
	#define OnPlayerExitedMenu(%0) FIXES_OnPlayerExitedMenu(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerExitedMenu(playerid);
#endif

/* 
 * FIXES_IS_VALID_MENU(Menu:menuid)
 * 
 * FIXES:
 *     Menus
 */

#if _FIX_Menus
	#define FIXES_IS_VALID_MENU(%0) (_FIXES_IN_RANGE(_:(%0), 0, MAX_MENUS) && (FIXES_gsValidMenus[_:(%0) >>> 5] & (1 << (_:(%0) & 0x1F))))
#endif

/* 
 * Menu:FIXES_CreateMenu(const title[], columns, Float:x, Float:y, Float:col1width, Float:col2width = 0.0)
 * 
 * FIXES:
 *     Menus
 */

#if defined _ALS_CreateMenu
	#error _ALS_CreateMenu defined
#endif
native Menu:BAD_CreateMenu(const title[], columns, Float:x, Float:y, Float:col1width, Float:col2width = 0.0) = CreateMenu;

#if _FIX_Menus
	#if !FIXES_Single
		forward _FIXES_CreateMenu(ret);
		
		public _FIXES_CreateMenu(ret)
		{
			FIXES_gsValidMenus[ret >>> 5] |= 1 << (ret & 0x1F);
		}
	#endif
	
	stock Menu:FIXES_CreateMenu(const title[], columns, Float:x, Float:y, Float:col1width, Float:col2width = 0.0)
	{
		new
			Menu:ret = CreateMenu(title, columns, x, y, col1width, col2width);
		if (_FIXES_IN_RANGE(_:ret, 0, MAX_MENUS))
		{
			#if FIXES_Single
				FIXES_gsValidMenus[_:ret >>> 5] |= 1 << (_:ret & 0x1F);
			#else
				CallRemoteFunction("_FIXES_CreateMenu", FIXES_gscSpec@i, _:ret);
			#endif
		}
		return ret;
	}
	
	#define _ALS_CreateMenu
	#define CreateMenu FIXES_CreateMenu
#endif

/* 
 * _FIXES_SetPlayerMenu(playerid, Menu:menuid)
 * 
 * FIXES:
 *     GetPlayerMenu
 */

#if FIX_GetPlayerMenu && !FIXES_Single
	forward _FIXES_SetPlayerMenu(playerid, Menu:menuid);
	
	public _FIXES_SetPlayerMenu(playerid, Menu:menuid)
	{
		FIXES_gsSettings |= e_FIXES_SETTINGS_MENU_SET;
		FIXES_gsCurrentMenu[playerid] = menuid;
	}
#endif

/* 
 * _FIXES_ClearPlayerMenu(playerid)
 * 
 * FIXES:
 *     GetPlayerMenu
 */

#if FIX_GetPlayerMenu && !FIXES_Single
	forward _FIXES_ClearPlayerMenu(playerid);
	
	public _FIXES_ClearPlayerMenu(playerid)
	{
		FIXES_gsSettings &= ~e_FIXES_SETTINGS_MENU_SET;
		FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
	}
#endif

/* 
 * Menu:FIXES_DestroyMenu(Menu:menuid)
 * 
 * FIXES:
 *     GetPlayerMenu
 */

#if defined _ALS_DestroyMenu
	#error _ALS_DestroyMenu defined
#endif
native BAD_DestroyMenu(Menu:menuid) = DestroyMenu;

#if _FIX_Menus || FIX_GetPlayerMenu
	#if _FIX_Menus && !FIXES_Single
		forward _FIXES_DestroyMenu(ret);
		
		public _FIXES_DestroyMenu(ret)
		{
			FIXES_gsValidMenus[ret >>> 5] &= ~(1 << (ret & 0x1F));
		}
	#endif
	
	stock FIXES_DestroyMenu(Menu:menuid)
	{
		#if _FIX_Menus
			if (FIXES_IS_VALID_MENU(menuid))
		#endif
			{
				if (DestroyMenu(Menu:menuid))
				{
					#if _FIX_Menus
						#if FIXES_Single
							FIXES_gsValidMenus[_:menuid >>> 5] &= ~(1 << (_:menuid & 0x1F));
						#else
							CallRemoteFunction("_FIXES_DestroyMenu", FIXES_gscSpec@i, _:menuid);
						#endif
					#endif
					_FIXES_FOREACH(FIXES_gsPlayersIterator, playerid)
					{
						if (FIXES_gsCurrentMenu[playerid] == menuid)
						{
							if (HideMenuForPlayer(menuid, playerid))
							{
								#if FIXES_Single
									FIXES_gsSettings &= ~e_FIXES_SETTINGS_MENU_SET;
									FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
								#else
									CallRemoteFunction(FIXES_gscSetPlayerMenu, FIXES_gscSpec@ii, playerid, INVALID_MENU);
								#endif
							}
						}
					}
					return 1;
				}
			}
			return 0;
	}
	
	#define _ALS_DestroyMenu
	#define DestroyMenu FIXES_DestroyMenu
#endif

/* 
 * FIXES_AddMenuItem(Menu:menuid, column, const menutext[])
 * 
 * FIXES:
 *     AddMenuItem
 */

#if defined _ALS_AddMenuItem
	#error _ALS_AddMenuItem defined
#endif
native BAD_AddMenuItem(Menu:menuid, column, const menutext[]) = AddMenuItem;

#if FIX_AddMenuItem
	stock FIXES_AddMenuItem(Menu:menuid, column, const menutext[])
	{
		if (FIXES_IS_VALID_MENU(menuid))
		{
			return AddMenuItem(menuid, column, menutext);
		}
		return 0;
	}
	
	#define _ALS_AddMenuItem
	#define AddMenuItem FIXES_AddMenuItem
#endif

/* 
 * FIXES_SetMenuColumnHeader(Menu:menuid, column, const columnheader[])
 * 
 * FIXES:
 *     SetMenuColumnHeader
 */

#if defined _ALS_SetMenuColumnHeader
	#error _ALS_SetMenuColumnHeader defined
#endif
native BAD_SetMenuColumnHeader(Menu:menuid, column, const columnheader[]) = SetMenuColumnHeader;

#if FIX_SetMenuColumnHeader
	stock FIXES_SetMenuColumnHeader(Menu:menuid, column, const columnheader[])
	{
		if (FIXES_IS_VALID_MENU(menuid))
		{
			return SetMenuColumnHeader(menuid, column, columnheader);
		}
		return 0;
	}
	
	#define _ALS_SetMenuColumnHeader
	#define SetMenuColumnHeader FIXES_SetMenuColumnHeader
#endif

/* 
 * FIXES_ShowMenuForPlayer(Menu:menuid, playerid)
 * 
 * FIXES:
 *     ShowMenuForPlayer
 *     GetPlayerMenu
 */

#if defined _ALS_ShowMenuForPlayer
	#error _ALS_ShowMenuForPlayer defined
#endif
native BAD_ShowMenuForPlayer(Menu:menuid, playerid) = ShowMenuForPlayer;

#if FIX_ShowMenuForPlayer || FIX_GetPlayerMenu
	stock FIXES_ShowMenuForPlayer(Menu:menuid, playerid)
	{
		#if FIX_ShowMenuForPlayer
			if (FIXES_IS_VALID_MENU(menuid))
		#endif
			{
				#if FIX_GetPlayerMenu
					if (ShowMenuForPlayer(menuid, playerid))
					{
						#if FIXES_Single
							FIXES_gsSettings |= e_FIXES_SETTINGS_MENU_SET;
							FIXES_gsCurrentMenu[playerid] = menuid;
						#else
							CallRemoteFunction(FIXES_gscSetPlayerMenu, FIXES_gscSpec@ii, playerid, _:menuid);
						#endif
						return 1;
					}
				#else
					return ShowMenuForPlayer(menuid, playerid);
				#endif
			}
		return 0;
	}
	
	#define _ALS_ShowMenuForPlayer
	#define ShowMenuForPlayer FIXES_ShowMenuForPlayer
#endif

/* 
 * FIXES_HideMenuForPlayer(Menu:menuid, playerid)
 * 
 * FIXES:
 *     HideMenuForPlayer
 *     HideMenuForPlayer_2
 *     GetPlayerMenu
 */

#if defined _ALS_HideMenuForPlayer
	#error _ALS_HideMenuForPlayer defined
#endif
native BAD_HideMenuForPlayer(Menu:menuid, playerid) = HideMenuForPlayer;

#if FIX_HideMenuForPlayer || FIX_GetPlayerMenu
	stock FIXES_HideMenuForPlayer(Menu:menuid, playerid)
	{
		#if FIX_HideMenuForPlayer
			if (FIXES_IS_VALID_MENU(menuid))
		#endif
			{
				#if FIX_GetPlayerMenu
					#if FIX_HideMenuForPlayer_2
						if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS) && FIXES_gsCurrentMenu[playerid] == menuid)
					#else
						if (FIXES_gsCurrentMenu[playerid] == menuid && HideMenuForPlayer(menuid, playerid))
					#endif
						{
							#if FIXES_Single
								FIXES_gsSettings &= ~e_FIXES_SETTINGS_MENU_SET;
								FIXES_gsCurrentMenu[playerid] = Menu:INVALID_MENU;
							#else
								CallRemoteFunction(FIXES_gscSetPlayerMenu, FIXES_gscSpec@ii, playerid, INVALID_MENU);
							#endif
							#if FIX_HideMenuForPlayer_2
								return HideMenuForPlayer(menuid, playerid);
							#else
								return 1;
							#endif
						}
				#else
					return HideMenuForPlayer(menuid, playerid);
				#endif
			}
		return 0;
	}
	
	#define _ALS_HideMenuForPlayer
	#define HideMenuForPlayer FIXES_HideMenuForPlayer
#endif

/* 
 * FIXES_DisableMenu(Menu:menuid)
 * 
 * FIXES:
 *     DisableMenu
 */

#if defined _ALS_DisableMenu
	#error _ALS_DisableMenu defined
#endif
native BAD_DisableMenu(Menu:menuid) = DisableMenu;

#if FIX_DisableMenu
	stock FIXES_DisableMenu(Menu:menuid)
	{
		if (FIXES_IS_VALID_MENU(menuid))
		{
			return DisableMenu(menuid);
		}
		return 0;
	}
	
	#define _ALS_DisableMenu
	#define DisableMenu FIXES_DisableMenu
#endif

/* 
 * FIXES_DisableMenuRow(Menu:menuid, row)
 * 
 * FIXES:
 *     DisableMenuRow
 */

#if defined _ALS_DisableMenuRow
	#error _ALS_DisableMenuRow defined
#endif
native BAD_DisableMenuRow(Menu:menuid, row) = DisableMenuRow;

#if FIX_DisableMenuRow
	stock FIXES_DisableMenuRow(Menu:menuid, row)
	{
		if (FIXES_IS_VALID_MENU(menuid))
		{
			return DisableMenuRow(menuid, row);
		}
		return 0;
	}
	
	#define _ALS_DisableMenuRow
	#define DisableMenuRow FIXES_DisableMenuRow
#endif

/* 
 * Menu:FIXES_GetPlayerMenu(playerid)
 * 
 * FIXES:
 *     GetPlayerMenu
 */

#if defined _ALS_GetPlayerMenu
	#error _ALS_GetPlayerMenu defined
#endif
native BAD_GetPlayerMenu(playerid) = GetPlayerMenu;

#if FIX_GetPlayerMenu
	stock Menu:FIXES_GetPlayerMenu(playerid)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			return FIXES_gsCurrentMenu[playerid];
		}
		return Menu:INVALID_MENU;
	}
	
	#define _ALS_GetPlayerMenu
	#define GetPlayerMenu FIXES_GetPlayerMenu
#endif

/* 
 * OnPlayerStateChange(playerid, newstate, oldstate)
 * 
 * FIXES:
 *     HydraSniper
 *     GetPlayerWeapon
 *     PutPlayerInVehicle
 *     TrainExit
 */

#if FIX_HydraSniper || FIX_GetPlayerWeapon || FIX_PutPlayerInVehicle || FIX_OnPlayerEnterVehicle || FIX_TrainExit
	public OnPlayerStateChange(playerid, newstate, oldstate)
	{
		#if !FIXES_Single
			if (!(FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE))
			{
				return FIXES_OnPlayerStateChange(playerid, newstate, oldstate);
			}
		#endif
		
		// =============================
		//  BEGIN: OnPlayerEnterVehicle 
		// =============================
		#if FIX_OnPlayerEnterVehicle
				if (newstate == PLAYER_STATE_PASSENGER)
				{
					new
						model = GetVehicleModel(GetPlayerVehicleID(playerid)) - 400;
					if (_FIXES_IN_RANGE(model, 0, 211 + 1))
					{
						new
							seat = GetPlayerVehicleSeat(playerid);
						if (seat != 128)
						{
							model = (FIXES_gscMaxPassengers[model >>> 3] >>> ((model & 7) << 2)) & 0xF;
							if (model == 0 || model == 15)
							{
								// Shouldn't be in this vehicle at all.
								FIXES_BlockUpdate(playerid, true);
							}
							else if (!(0 < seat <= model)) // Slower with "_FIXES_NO_RANGE".
							{
								// In an out of range seat.
								FIXES_BlockUpdate(playerid, true);
							}
						}
					}
					else
					{
						FIXES_BlockUpdate(playerid, true);
					}
				}
				else if (newstate == PLAYER_STATE_DRIVER)
				{
						new
							model = GetPlayerVehicleSeat(playerid);
						if (model != 128)
						{
							if (model == 0)
							{
								model = GetVehicleModel(GetPlayerVehicleID(playerid)) - 400;
								if (_FIXES_NO_RANGE(model, 0, 211 + 1) || FIXES_gscMaxPassengers[model >>> 3] >>> ((model & 7) << 2) & 0xF == 15)
								{
									// In an invalid vehicle (one you can't drive).
									FIXES_BlockUpdate(playerid, true);
								}
							}
							else
							{
								// They are a driver, but not in the driver's seat.
								FIXES_BlockUpdate(playerid, true);
							}
						}
					#if FIX_HydraSniper
						else
						{
							model = GetVehicleModel(GetPlayerVehicleID(playerid));
						}
					#endif
			#if !FIX_HydraSniper
				}
			#endif
		#endif
		// =============================
		//  END:   OnPlayerEnterVehicle 
		// =============================
		
		// ====================
		//  BEGIN: HydraSniper 
		// ====================
		#if FIX_HydraSniper
			#if !FIX_OnPlayerEnterVehicle
				// Only called if the same check above isn't called.
				if (newstate == PLAYER_STATE_DRIVER)
				{
					new
						model = GetVehicleModel(GetPlayerVehicleID(playerid));
			#endif
					if (GetPlayerWeapon(playerid) == 34 && (model == 520 || model == 425))
					{
						SetPlayerArmedWeapon(playerid, 0);
						#if FIX_GetPlayerWeapon
							// This is the first cross-dependednt fix.
							#if FIXES_Single
								FIXES_gsPlayerWeapon[playerid] = 0;
							#else
								SetPVarInt(playerid, FIXES_pvarPlayerWeapon, 0);
							#endif
						#endif
					}
				}
		#endif
		// ====================
		//  END:   HydraSniper 
		// ====================
		
		#if FIX_GetPlayerWeapon || FIX_PutPlayerInVehicle || FIX_TrainExit
			#if PLAYER_STATE_PASSENGER != PLAYER_STATE_DRIVER + 1
				#error FIX_GetPlayerWeapon/FIX_PutPlayerInVehicle state assertation failed.
			#endif
			if (_FIXES_IN_RANGE(oldstate, PLAYER_STATE_DRIVER, PLAYER_STATE_PASSENGER + 1))
			{
				#if FIX_PutPlayerInVehicle || FIX_TrainExit
					new
						e_FIXES_BOOLS:bools = FIXES_gsPlayerBools[playerid];
				#endif
				
				// ========================
				//  BEGIN: GetPlayerWeapon 
				// ========================
				#if FIX_GetPlayerWeapon
					#if FIXES_Single
						FIXES_gsPlayerWeapon[playerid] = -1;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerWeapon, -1);
					#endif
				#endif
				// ========================
				//  END:   GetPlayerWeapon 
				// ========================
				
				// ==================
				//  BEGIN: TrainExit
				// ==================
				#if FIX_TrainExit
					FIXES_PRINTF("train exit");
					if (bools & e_FIXES_BOOLS_PUT_IN_TRAIN)
					{
						FIXES_PRINTF("OK");
						SetCameraBehindPlayer(playerid),
						bools &= ~e_FIXES_BOOLS_PUT_IN_TRAIN;
					}
				#endif
				// ==================
				//  END:   TrainExit
				// ==================
				
				// ===========================
				//  BEGIN: PutPlayerInVehicle 
				// ===========================
				#if FIX_PutPlayerInVehicle
					// Update their vehicle once we KNOW the client has done the
					// removal from the vehicle.
					if (bools & e_FIXES_BOOLS_PUT_IN_VEHICLE)
					{
						new
							vid = FIXES_gsVehicleSeatData[playerid] & 0x00FFFFFF;
						// Limited to "only" 16777216 vehicles and 256 seats.
						PutPlayerInVehicle(playerid, vid, FIXES_gsVehicleSeatData[playerid] >>> 24),
						bools &= ~e_FIXES_BOOLS_PUT_IN_VEHICLE;
						#if FIX_TrainExit
							switch (GetVehicleModel(vid))
							{
								case 449, 537, 538:
								{
									bools |= e_FIXES_BOOLS_PUT_IN_TRAIN;
								}
							}
						#endif
					}
				#endif
				// ===========================
				//  END:   PutPlayerInVehicle 
				// ===========================
				
				#if FIX_PutPlayerInVehicle || FIX_TrainExit
					FIXES_gsPlayerBools[playerid] = bools;
				#endif
			}
		#endif
		
		return FIXES_OnPlayerStateChange(playerid, newstate, oldstate);
	}
	
	#if defined _ALS_OnPlayerStateChange
		#error _ALS_OnPlayerStateChange defined
	#endif
	#define _ALS_OnPlayerStateChange
	#define OnPlayerStateChange(%0) FIXES_OnPlayerStateChange(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerStateChange(playerid, newstate, oldstate);
#endif

/* 
 * FIXES_GivePlayerWeapon(playerid, weaponid, ammo)
 * 
 * FIXES:
 *     HydraSniper
 */

#if defined _ALS_GivePlayerWeapon
	#error _ALS_GivePlayerWeapon defined
#endif
native BAD_GivePlayerWeapon(playerid, weaponid, ammo) = GivePlayerWeapon;

#if FIX_HydraSniper
	stock FIXES_GivePlayerWeapon(playerid, weaponid, ammo)
	{
		new
			vid = GetPlayerVehicleID(playerid);
		if (vid)
		{
			vid = GetVehicleModel(vid);
			if (weaponid == 34 && (vid == 520 || vid == 425))
			{
				vid = GivePlayerWeapon(playerid, weaponid, ammo),
				SetPlayerArmedWeapon(playerid, 0);
				#if FIX_GetPlayerWeapon
					#if FIXES_Single
						FIXES_gsPlayerWeapon[playerid] = 0;
					#else
						SetPVarInt(playerid, FIXES_pvarPlayerWeapon, 0);
					#endif
				#endif
				return vid;
			}
		}
		return GivePlayerWeapon(playerid, weaponid, ammo);
	}
	
	#define _ALS_GivePlayerWeapon
	#define GivePlayerWeapon FIXES_GivePlayerWeapon
#endif

/* 
 * FIXES_SetPlayerArmedWeapon(playerid, weaponid)
 * 
 * FIXES:
 *     GetPlayerWeapon
 */

#if defined _ALS_SetPlayerArmedWeapon
	#error _ALS_SetPlayerArmedWeapon defined
#endif
native BAD_SetPlayerArmedWeapon(playerid, weaponid) = SetPlayerArmedWeapon;

#if FIX_GetPlayerWeapon
	stock FIXES_SetPlayerArmedWeapon(playerid, weaponid)
	{
		#if FIX_AllowInteriorWeapons
			if (FIXES_gsSettings & e_FIXES_SETTINGS_INTERIOR && GetPlayerInterior(playerid))
			{
				// Not allowed weapons.
				return 0;
			}
		#endif
		new
			vid = GetPlayerVehicleID(playerid);
		if (vid)
		{
			#if FIX_HydraSniper
				vid = GetVehicleModel(vid);
				if (weaponid == 34 && (vid == 520 || vid == 425))
				{
					return 0;
				}
			#endif
			//FIXES_gsPlayerWeapon[playerid] = weaponid;
			#if FIXES_Single
				FIXES_gsPlayerWeapon[playerid] = weaponid;
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerWeapon, weaponid);
			#endif
		}
		return SetPlayerArmedWeapon(playerid, weaponid);
	}
	
	#define _ALS_SetPlayerArmedWeapon
	#define SetPlayerArmedWeapon FIXES_SetPlayerArmedWeapon
#endif

/* 
 * FIXES_GetPlayerWeapon(playerid)
 * 
 * FIXES:
 *     GetPlayerWeapon
 */

#if defined _ALS_GetPlayerWeapon
	#error _ALS_GetPlayerWeapon defined
#endif
native BAD_GetPlayerWeapon(playerid) = GetPlayerWeapon;

#if FIX_GetPlayerWeapon
	stock FIXES_GetPlayerWeapon(playerid)
	{
		#if FIXES_Single
			new
				ret = FIXES_gsPlayerWeapon[playerid];
		#else
			new
				ret = GetPVarInt(playerid, FIXES_pvarPlayerWeapon);
		#endif
		if (ret == -1)
		{
			return GetPlayerWeapon(playerid);
		}
		return ret;
	}
	
	#define _ALS_GetPlayerWeapon
	#define GetPlayerWeapon FIXES_GetPlayerWeapon
#endif

/* 
 * FIXES_PutPlayerInVehicle(playerid, vehicleid, seatid)
 * 
 * FIXES:
 *     PutPlayerInVehicle
 */

#if defined _ALS_PutPlayerInVehicle
	#error _ALS_PutPlayerInVehicle defined
#endif
native BAD_PutPlayerInVehicle(playerid, vehicleid, seatid) = PutPlayerInVehicle;

#if FIX_PutPlayerInVehicle || FIX_TrainExit
	#if FIXES_Single
		stock FIXES_PutPlayerInVehicle(playerid, vehicleid, seatid)
		{
			// ==================
			//  BEGIN: TrainExit
			// ==================
			#if FIX_TrainExit
				FIXES_PRINTF("TrainExit %d", GetVehicleModel(vehicleid));
				switch (GetVehicleModel(vehicleid))
				{
					case 449, 537, 538:
					{
						FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_PUT_IN_TRAIN;
					}
				}
			#endif
			// ==================
			//  END:   TrainExit
			// ==================
			
			// ===========================
			//  BEGIN: PutPlayerInVehicle 
			// ===========================
			#if FIX_PutPlayerInVehicle
				new
					vid = GetPlayerVehicleID(playerid);
				if (vid)
				{
					new
						Float:x,
						Float:y,
						Float:z;
					// Remove them without the animation.
					return
						GetVehiclePos(vid, x, y, z),
						SetPlayerPos(playerid, x, y, z),
						FIXES_gsVehicleSeatData[playerid] = seatid << 24 | vehicleid,
						FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_PUT_IN_VEHICLE,
						1;
				}
			#endif
			// ===========================
			//  END:   PutPlayerInVehicle 
			// ===========================
			return PutPlayerInVehicle(playerid, vehicleid, seatid);
		}
	#else
		forward _FIXES_PutPlayerInVehicle(playerid, vehicleid, seatid, from, data);
		
		stock FIXES_PutPlayerInVehicle(playerid, vehicleid, seatid)
		{
			FIXES_PRINTF("NOT SINGLE");
			// ===========================
			//  BEGIN: PutPlayerInVehicle 
			// ===========================
			#if FIX_PutPlayerInVehicle
				new
					vid = GetPlayerVehicleID(playerid);
				if (vid)
				{
					new
						Float:x,
						Float:y,
						Float:z;
					// Remove them without the animation.
					return
						GetVehiclePos(vid, x, y, z),
						SetPlayerPos(playerid, x, y, z),
						CallRemoteFunction(FIXES_gscPutPlayerInVehicle, FIXES_gscSpec@iiiii, playerid, vehicleid, seatid, 1, vid),
						1;
				}
			#endif
			// ===========================
			//  END:   PutPlayerInVehicle 
			// ===========================
			
			// ==================
			//  BEGIN: TrainExit
			// ==================
			#if FIX_TrainExit
				new
					model = GetVehicleModel(vehicleid);
				switch (model)
				{
					case 449, 537, 538:
					{
						CallRemoteFunction(FIXES_gscPutPlayerInVehicle, FIXES_gscSpec@iiiii, playerid, vehicleid, seatid, 0, model);
						//FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_PUT_IN_TRAIN;
					}
				}
			#endif
			// ==================
			//  END:   TrainExit
			// ==================
			return PutPlayerInVehicle(playerid, vehicleid, seatid);
		}
		
		public _FIXES_PutPlayerInVehicle(playerid, vehicleid, seatid, from, data)
		{
			FIXES_PRINTF("IN CHARGE: %d", (FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE));
			_FIXES_IS_IN_CHARGE()
			{
				// ===========================
				//  BEGIN: PutPlayerInVehicle 
				// ===========================
				#if FIX_PutPlayerInVehicle
					if (from)
					{
						FIXES_gsVehicleSeatData[playerid] = seatid << 24 | vehicleid,
						FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_PUT_IN_VEHICLE;
					}
				#endif
				// ===========================
				//  END:   PutPlayerInVehicle 
				// ===========================
				
				// ==================
				//  BEGIN: TrainExit
				// ==================
				#if FIX_TrainExit
					#if FIX_PutPlayerInVehicle
						else
					#else
						if (!from)
					#endif
						{
							switch (data)
							{
								case 449, 537, 538:
								{
									FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_PUT_IN_TRAIN;
								}
							}
						}
				#endif
				// ==================
				//  END:   TrainExit
				// ==================
			}
			return 1;
		}
	#endif
	
	#define _ALS_PutPlayerInVehicle
	#define PutPlayerInVehicle FIXES_PutPlayerInVehicle
#endif

/* 
 * _FIXES_SetCheckpoint(playerid, Float:x, Float:y, Float:z, Float:size)
 * 
 * FIXES:
 *     SetPlayerCheckpoint
 */

#if FIX_SetPlayerCheckpoint
	forward _FIXES_SetCheckpoint(playerid, Float:x, Float:y, Float:z, Float:size);
	
	public _FIXES_SetCheckpoint(playerid, Float:x, Float:y, Float:z, Float:size)
	{
		#if FIXES_Single
			if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_CP_DELAYED)
		#else
			if (GetPVarInt(playerid, FIXES_pvarPlayerCheckpoint))
		#endif
			{
				return
					#if FIXES_Single
						FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_CP_DELAYED,
					#else
						DeletePVar(playerid, FIXES_pvarPlayerCheckpoint),
					#endif
						SetPlayerCheckpoint(playerid, x, y, z, size);
			}
			return 0;
	}
#endif

/* 
 * FIXES_SetPlayerCheckpoint(playerid, Float:x, Float:y, Float:z, Float:size)
 * 
 * FIXES:
 *     SetPlayerCheckpoint
 */

#if defined _ALS_SetPlayerCheckpoint
	#error _ALS_SetPlayerCheckpoint defined
#endif
native BAD_SetPlayerCheckpoint(playerid, Float:x, Float:y, Float:z, Float:size) = SetPlayerCheckpoint;

#if FIX_SetPlayerCheckpoint
	stock FIXES_SetPlayerCheckpoint(playerid, Float:x, Float:y, Float:z, Float:size)
	{
		return
			#if FIXES_Single
				FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_CP_DELAYED,
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerCheckpoint, 1),
			#endif
				DisablePlayerCheckpoint(playerid),
				SetTimerEx(FIXES_gscSetCheckpoint, 50, 0, FIXES_gscSpec@iffff, playerid, x, y, z, size),
				1;
	}
	
	#define _ALS_SetPlayerCheckpoint
	#define SetPlayerCheckpoint FIXES_SetPlayerCheckpoint
#endif

/* 
 * FIXES_DisablePlayerCheckpoint(playerid)
 * 
 * FIXES:
 *     SetPlayerCheckpoint
 */

#if defined _ALS_DisablePlayerCheckpoint
	#error _ALS_DisablePlayerCheckpoint defined
#endif
native BAD_DisablePlayerCheckpoint(playerid) = DisablePlayerCheckpoint;

#if FIX_SetPlayerCheckpoint
	stock FIXES_DisablePlayerCheckpoint(playerid)
	{
		return
			#if FIXES_Single
				FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_CP_DELAYED,
			#else
				DeletePVar(playerid, FIXES_pvarPlayerCheckpoint),
			#endif
				DisablePlayerCheckpoint(playerid);
	}
	
	#define _ALS_DisablePlayerCheckpoint
	#define DisablePlayerCheckpoint FIXES_DisablePlayerCheckpoint
#endif

/* 
 * _FIXES_SetRaceCheckpoint(playerid, type, Float:x, Float:y, Float:z, Float:nextx, Float:nexty, Float:nextz, Float:size)
 * 
 * FIXES:
 *     SetPlayerRaceCheckpoint
 */

#if FIX_SetPlayerRaceCheckpoint
	forward _FIXES_SetRaceCheckpoint(playerid, type, Float:x, Float:y, Float:z, Float:nextx, Float:nexty, Float:nextz, Float:size);
	
	public _FIXES_SetRaceCheckpoint(playerid, type, Float:x, Float:y, Float:z, Float:nextx, Float:nexty, Float:nextz, Float:size)
	{
		#if FIXES_Single
			if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_RACE_CP_DELAYED)
		#else
			if (GetPVarInt(playerid, FIXES_pvarPlayerRaceCheckpoint))
		#endif
			{
				return
					#if FIXES_Single
						FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_RACE_CP_DELAYED,
					#else
						DeletePVar(playerid, FIXES_pvarPlayerRaceCheckpoint),
					#endif
						SetPlayerRaceCheckpoint(playerid, type, x, y, z, nextx, nexty, nextz, size);
			}
			return 0;
	}
#endif

/* 
 * FIXES_SetPlayerRaceCheckpoint(playerid, type, Float:x, Float:y, Float:z, Float:nextx, Float:nexty, Float:nextz, Float:size)
 * 
 * FIXES:
 *     SetPlayerRaceCheckpoint
 */

#if defined _ALS_SetPlayerRaceCheckpoint
	#error _ALS_SetPlayerRaceCheckpoint defined
#endif
native BAD_SetPlayerRaceCheckpoint(playerid, type, Float:x, Float:y, Float:z, Float:nextx, Float:nexty, Float:nextz, Float:size) = SetPlayerRaceCheckpoint;

#if FIX_SetPlayerRaceCheckpoint
	stock FIXES_SetPlayerRaceCheckpoint(playerid, type, Float:x, Float:y, Float:z, Float:nextx, Float:nexty, Float:nextz, Float:size)
	{
		return
			#if FIXES_Single
				FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_RACE_CP_DELAYED,
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerRaceCheckpoint, 1),
			#endif
				DisablePlayerRaceCheckpoint(playerid),
				SetTimerEx(FIXES_gscSetRaceCheckpoint, 50, 0, FIXES_gscSpec@iifffffff, playerid, type, x, y, z, nextx, nexty, nextz, size),
				1;
	}
	
	#define _ALS_SetPlayerRaceCheckpoint
	#define SetPlayerRaceCheckpoint FIXES_SetPlayerRaceCheckpoint
#endif

/* 
 * FIXES_DisablePlayerRaceCheckpoint(playerid)
 * 
 * FIXES:
 *     SetPlayerRaceCheckpoint
 */

#if defined _ALS_DisablePlayerRaceCP
	#error _ALS_DisablePlayerRaceCP defined
#endif
native BAD_DisablePlayerRaceCheckpoint(playerid) = DisablePlayerRaceCheckpoint;

#if FIX_SetPlayerRaceCheckpoint
	stock FIXES_DisablePlayerRaceCP(playerid)
	{
		return
			#if FIXES_Single
				FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_RACE_CP_DELAYED,
			#else
				DeletePVar(playerid, FIXES_pvarPlayerRaceCheckpoint),
			#endif
				DisablePlayerRaceCheckpoint(playerid);
	}
	
	#define _ALS_DisablePlayerRaceCP
	#define DisablePlayerRaceCheckpoint FIXES_DisablePlayerRaceCP
#endif

/* 
 * _FIXES_HideGameTextForPlayer(playerid, style, parr[])
 * 
 * Hides a GameText style for one or more players, and accounts for the fact
 * that they may have different messages being shown.
 * 
 * FIXES:
 *     GameText
 */

#if FIX_GameText
	static stock _FIXES_HideGameTextForPlayer(playerid, style, parr[])
	{
		switch (playerid)
		{
			case MAX_PLAYERS:
			{
				// Remove the global GameText for everyone that can see it.
				TextDrawHideForAll(FIXES_gsGTStyle[style]);
			}
			case INVALID_PLAYER_ID:
			{
				// Hide any player-local ones.  This is similar to "foreach".
				playerid = parr[MAX_PLAYERS],
				parr[MAX_PLAYERS] = MAX_PLAYERS;
				for (new next; playerid != MAX_PLAYERS; playerid = next)
				{
					PlayerTextDrawHide(playerid, FIXES_gsPGTStyle[playerid][style]),
					next = parr[playerid],
					parr[playerid] = 0;
				}
			}
			default:
			{
				// Remove.
				PlayerTextDrawHide(playerid, FIXES_gsPGTStyle[playerid][style]),
				_FIXES_RemoveInternal(parr, playerid, MAX_PLAYERS);
			}
		}
	}
#endif

/* 
 * _FIXES_HideGameTextTimer(playerid, style)
 * 
 * FIXES:
 *     GameText
 */

#if FIX_GameText
	forward _FIXES_HideGameTextTimer(playerid, style);
	
	public _FIXES_HideGameTextTimer(playerid, style)
	{
		if (GetTickCount() >= FIXES_gsGTTimer[style][playerid])
		{
			// Only hide it if it hasn't been re-shown in the interim.
			_FIXES_HideGameTextForPlayer(playerid, style, FIXES_gsPlayerPGTShown[style]);
		}
	}
#endif

/* 
 * _FIXES_GameTextShow(playerid, string[], time, style)
 * 
 * FIXES:
 *     GameText
 */

#if FIX_GameText
	forward _FIXES_GameTextShow(playerid, string[], time, style);
	
	public _FIXES_GameTextShow(playerid, string[], time, style)
	{
		_FIXES_IS_IN_CHARGE()
		{
			if (playerid == MAX_PLAYERS)
			{
				_FIXES_HideGameTextForPlayer(INVALID_PLAYER_ID, style, FIXES_gsPlayerPGTShown[style]),
				TextDrawSetString(FIXES_gsGTStyle[style], string),
				TextDrawShowForAll(FIXES_gsGTStyle[style]);
			}
			else
			{
				TextDrawHideForPlayer(playerid, FIXES_gsGTStyle[style]),
				PlayerTextDrawSetString(playerid, FIXES_gsPGTStyle[playerid][style], string),
				PlayerTextDrawShow(playerid, FIXES_gsPGTStyle[playerid][style]),
				_FIXES_AddInternal(FIXES_gsPlayerPGTShown[style], playerid, MAX_PLAYERS);
			}
			if (time)
			{
				// Tiny offset because timers are inaccurate.
				FIXES_gsGTTimer[style][playerid] = GetTickCount() + time - 10,
				// Don't need to save the timer's ID.
				SetTimerEx(FIXES_gscHideGameTextTimer, time, false, FIXES_gscSpec@ii, playerid, style);
			}
		}
		return 1;
	}
#endif

/* 
 * FIXES_GameTextForAll(string[], time, style)
 * 
 * FIXES:
 *     GameText
 */

#if defined _ALS_GameTextForAll
	#error _ALS_GameTextForAll defined
#endif
native BAD_GameTextForAll(const string[], time, style) = GameTextForAll;

#if FIX_GameText
	stock FIXES_GameTextForAll(string[], time, style)
	{
		if (_FIXES_IN_RANGE(style, 0, FIXES_GT_STYLE_COUNT))
		{
			if ((string[0] == '\0') || (string[0] == '\1' && string[1] == '\0'))
			{
				#if FIXES_Single
					return _FIXES_GameTextShow(MAX_PLAYERS, FIXES_gscSpace, time, style);
				#else
					return CallRemoteFunction(FIXES_gscGameTextShow, FIXES_gscSpec@isii, MAX_PLAYERS, FIXES_gscSpace, time, style);
				#endif
			}
			else
			{
				#if FIXES_Single
					return _FIXES_GameTextShow(MAX_PLAYERS, string, time, style);
				#else
					return CallRemoteFunction(FIXES_gscGameTextShow, FIXES_gscSpec@isii, MAX_PLAYERS, string, time, style);
				#endif
			}
		}
		return 0;
	}
	
	#define _ALS_GameTextForAll
	
	#define GameTextForAll FIXES_GameTextForAll
#endif

/* 
 * FIXES_GameTextForPlayer(playerid, string[], time, style)
 * 
 * FIXES:
 *     GameText
 */

#if defined _ALS_GameTextForPlayer
	#error _ALS_GameTextForPlayer defined
#endif
native BAD_GameTextForPlayer(playerid, const string[], time, style) = GameTextForPlayer;

#if FIX_GameText
	stock FIXES_GameTextForPlayer(playerid, string[], time, style)
	{
		if (_FIXES_IN_RANGE(style, 0, FIXES_GT_STYLE_COUNT) && _FIXES_IS_PLAYER_CONNECTED(playerid))
		{
			if ((string[0] <= '\0') || (string[0] == '\1' && string[1] == '\0'))
			{
				#if FIXES_Single
					return _FIXES_GameTextShow(playerid, FIXES_gscSpace, time, style);
				#else
					return CallRemoteFunction(FIXES_gscGameTextShow, FIXES_gscSpec@isii, playerid, FIXES_gscSpace, time, style);
				#endif
			}
			else
			{
				#if FIXES_Single
					return _FIXES_GameTextShow(playerid, string, time, style);
				#else
					return CallRemoteFunction(FIXES_gscGameTextShow, FIXES_gscSpec@isii, playerid, string, time, style);
				#endif
			}
		}
		return 0;
	}
	
	#define _ALS_GameTextForPlayer
	
	#define GameTextForPlayer FIXES_GameTextForPlayer
#endif

/* 
 * HideGameTextForAll(style)
 * 
 * FIXES:
 *     HideGameText
 */

#if defined _ALS_HideGameTextForAll
	#error _ALS_HideGameTextForAll defined
#endif
native BAD_HideGameTextForAll(style) = HideGameTextForAll;

#if FIX_HideGameText
	#define _ALS_HideGameTextForAll
	
	#define HideGameTextForAll(%0) GameTextForAll(FIXES_gscSpace, 0, (%0))
#endif

/* 
 * HideGameTextForPlayer(playerid, style)
 * 
 * FIXES:
 *     HideGameText
 */

#if defined _ALS_HideGameTextForPlayer
	#error _ALS_HideGameTextForPlayer defined
#endif
native BAD_HideGameTextForPlayer(playerid, style) = HideGameTextForPlayer;

#if FIX_HideGameText
	#define _ALS_HideGameTextForPlayer
	
	#define HideGameTextForPlayer(%1,%0) GameTextForPlayer((%1), FIXES_gscSpace, 0, (%0))
#endif

/* 
 * PlayerText:FIXES_CreatePlayerTextDraw(playerid, Float:x, Float:y, text[])
 * 
 * FIXES:
 *     CreatePlayerTextDraw
 */

#if defined _ALS_CreatePlayerTextDraw
	#error _ALS_CreatePlayerTextDraw defined
#endif
native Text:BAD_CreatePlayerTextDraw(playerid, Float:x, Float:y, text[]) = CreatePlayerTextDraw;

#if FIX_CreatePlayerTextDraw
	stock PlayerText:FIXES_CreatePlayerTextDraw(playerid, Float:x, Float:y, text[])
	{
		if ((text[0] == '\0') || (text[0] == '\1' && text[1] == '\0'))
		{
			return CreatePlayerTextDraw(playerid, x, y, FIXES_gscSpace);
		}
		else
		{
			return CreatePlayerTextDraw(playerid, x, y, text);
		}
	}
	
	#define _ALS_CreatePlayerTextDraw
	#define CreatePlayerTextDraw FIXES_CreatePlayerTextDraw
#endif

/* 
 * FIXES_PlayerTextDrawSetString(playerid, PlayerText:text, string[])
 * 
 * FIXES:
 *     PlayerTextDrawSetString
 */

#if defined _ALS_PlayerTextDrawSetString
	#error _ALS_PlayerTextDrawSetString defined
#endif
native BAD_PlayerTextDrawSetString(playerid, PlayerText:text, string[]) = PlayerTextDrawSetString;

#if FIX_PlayerTextDrawSetString
	stock FIXES_PlayerTextDrawSetString(playerid, PlayerText:text, string[])
	{
		if ((string[0] == '\0') || (string[0] == '\1' && string[1] == '\0'))
		{
			return PlayerTextDrawSetString(playerid, text, FIXES_gscSpace);
		}
		else
		{
			return PlayerTextDrawSetString(playerid, text, string);
		}
	}
	
	#define _ALS_PlayerTextDrawSetString
	#define PlayerTextDrawSetString FIXES_PlayerTextDrawSetString
#endif

/* 
 * Text:FIXES_TextDrawCreate(Float:x, Float:y, text[])
 * 
 * FIXES:
 *     TextDrawCreate
 */

	#if defined _ALS_TextDrawCreate
		#error _ALS_TextDrawCreate defined
native Text:BAD_TextDrawCreate(Float:x, Float:y, text[]) = TextDrawCreate;

#if FIX_TextDrawCreate
	stock Text:FIXES_TextDrawCreate(Float:x, Float:y, text[])
	{
		if ((text[0] == '\0') || (text[0] == '\1' && text[1] == '\0'))
		{
			return TextDrawCreate(x, y, FIXES_gscSpace);
		}
		else
		{
			return TextDrawCreate(x, y, text);
		}
	}
	
	#endif
	#define _ALS_TextDrawCreate
	#define TextDrawCreate FIXES_TextDrawCreate
#endif

/* 
 * FIXES_TextDrawSetString(Text:text, string[])
 * 
 * FIXES:
 *     TextDrawSetString
 */

#if defined _ALS_TextDrawSetString
	#error _ALS_TextDrawSetString defined
#endif
native BAD_TextDrawSetString(Text:text, string[]) = TextDrawSetString;

#if FIX_TextDrawSetString
	stock FIXES_TextDrawSetString(Text:text, string[])
	{
		if ((string[0] == '\0') || (string[0] == '\1' && string[1] == '\0'))
		{
			return TextDrawSetString(text, FIXES_gscSpace);
		}
		else
		{
			return TextDrawSetString(text, string);
		}
	}
	
	#define _ALS_TextDrawSetString
	#define TextDrawSetString FIXES_TextDrawSetString
#endif

/* 
 * FIXES_AllowInteriorWeapons(allow)
 * 
 * FIXES:
 *     AllowInteriorWeapons
 */

#if defined _ALS_AllowInteriorWeapons
	#error _ALS_AllowInteriorWeapons defined
#endif
native BAD_AllowInteriorWeapons(allow) = AllowInteriorWeapons;

#if FIX_AllowInteriorWeapons
	#if FIXES_Single
		stock FIXES_AllowInteriorWeapons(allow)
		{
			if (allow)
			{
				FIXES_gsSettings &= ~e_FIXES_SETTINGS_INTERIOR;
				_FIXES_FOREACH(FIXES_gsPlayersIterator, i)
				{
					FIXES_gsPlayerBools[i] &= ~e_FIXES_BOOLS_INTERIOR;
				}
			}
			else
			{
				FIXES_gsSettings |= e_FIXES_SETTINGS_INTERIOR;
				_FIXES_FOREACH(FIXES_gsPlayersIterator, i)
				{
					if (GetPlayerInterior(i))
					{
						FIXES_gsPlayerBools[i] |= e_FIXES_BOOLS_INTERIOR;
					}
				}
			}
			return allow;
		}
	#else
		forward _FIXES_AllowInteriorWeapons(allow);
		
		stock FIXES_AllowInteriorWeapons(allow)
		{
			CallRemoteFunction(FIXES_gscAllowInteriorWeapons, FIXES_gscSpec@i, allow);
			return allow;
		}
		
		public _FIXES_AllowInteriorWeapons(allow)
		{
			if (allow)
			{
				FIXES_gsSettings &= ~e_FIXES_SETTINGS_INTERIOR;
				_FIXES_FOREACH(FIXES_gsPlayersIterator, i)
				{
					FIXES_gsPlayerBools[i] &= ~e_FIXES_BOOLS_INTERIOR;
				}
			}
			else
			{
				FIXES_gsSettings |= e_FIXES_SETTINGS_INTERIOR;
				_FIXES_FOREACH(FIXES_gsPlayersIterator, i)
				{
					if (GetPlayerInterior(i))
					{
						FIXES_gsPlayerBools[i] |= e_FIXES_BOOLS_INTERIOR;
					}
				}
			}
			return allow;
		}
	#endif
	
	#define _ALS_AllowInteriorWeapons
	#define AllowInteriorWeapons FIXES_AllowInteriorWeapons
#endif

/* 
 * FIXES_GetPlayerInterior(playerid)
 * 
 * FIXES:
 *     GetPlayerInterior
 */

#if defined _ALS_GetPlayerInterior
	#error _ALS_GetPlayerInterior defined
#endif
native BAD_GetPlayerInterior(playerid) = GetPlayerInterior;

#if FIX_GetPlayerInterior
	stock FIXES_GetPlayerInterior(playerid)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			#if FIXES_Single
				return FIXES_gsInterior[playerid];
			#else
				return GetPVarInt(playerid, FIXES_pvarPlayerInterior);
			#endif
		}
		return 0;
	}
	
	#define _ALS_GetPlayerInterior
	#define GetPlayerInterior FIXES_GetPlayerInterior
#endif

/* 
 * FIXES_SetPlayerInterior(playerid, interiorid)
 * 
 * FIXES:
 *     GetPlayerInterior
 */

#if defined _ALS_SetPlayerInterior
	#error _ALS_SetPlayerInterior defined
#endif
native BAD_SetPlayerInterior(playerid, interiorid) = SetPlayerInterior;

#if FIX_GetPlayerInterior
	stock FIXES_SetPlayerInterior(playerid, interiorid)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			#if FIXES_Single
				FIXES_gsInterior[playerid] = interiorid;
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerInterior, interiorid);
			#endif
			return SetPlayerInterior(playerid, interiorid);
		}
		return 0;
	}
	
	#define _ALS_SetPlayerInterior
	#define SetPlayerInterior FIXES_SetPlayerInterior
#endif

/* 
 * OnPlayerClickMap(playerid, Float:fX, Float:fY, Float:fZ)
 * 
 * FIXES:
 *     AllowTeleport
 */

#if FIX_AllowTeleport && !defined FILTERSCRIPT
	forward _FIXES_AllowTeleport(playerid, allow);
	
	public OnPlayerClickMap(playerid, Float:fX, Float:fY, Float:fZ)
	{
		// ======================
		//  BEGIN: AllowTeleport 
		// ======================
		#if FIX_AllowTeleport
			if (!_FIXES_gIsFilterscript)
			{
				// Unusually, the call order here is always Game Mode first -
				// most callbacks are Filter Script first.
				if ((FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_TELEPORT) || ((FIXES_gsSettings & e_FIXES_SETTINGS_ADMIN_TELEPORT) && (IsPlayerAdmin(playerid))))
				{
					SetPlayerPosFindZ(playerid, fX, fY, fZ);
				}
			}
		#endif
		// ======================
		//  END:   AllowTeleport 
		// ======================
		return FIXES_OnPlayerClickMap(playerid, fX, fY, fZ);
	}
	
	#if defined _ALS_OnPlayerClickMap
		#error _ALS_OnPlayerClickMap defined
	#endif
	#define _ALS_OnPlayerClickMap
	#define OnPlayerClickMap(%0) FIXES_OnPlayerClickMap(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnPlayerClickMap(playerid, Float:fX, Float:fY, Float:fZ);
#endif

/* 
 * FIXES_AllowPlayerTeleport(playerid, allow)
 * 
 * FIXES:
 *     AllowTeleport
 */

#if defined _ALS_AllowPlayerTeleport
	#error _ALS_AllowPlayerTeleport defined
#endif
native BAD_AllowPlayerTeleport(playerid, allow) = AllowPlayerTeleport;

#if FIX_AllowTeleport
	#if FIXES_Single
		stock FIXES_AllowPlayerTeleport(playerid, allow)
		{
			if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
			{
				if (allow)
				{
					FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_TELEPORT;
				}
				else
				{
					FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_TELEPORT;
				}
			}
			return 0;
		}
	#else
		stock FIXES_AllowPlayerTeleport(playerid, allow)
		{
			if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
			{
				CallRemoteFunction(FIXES_gscAllowTeleport, FIXES_gscSpec@ii, playerid, allow);
			}
			return 0;
		}
	#endif
	
	#define _ALS_AllowPlayerTeleport
	#define AllowPlayerTeleport FIXES_AllowPlayerTeleport
#endif

/* 
 * FIXES_AllowAdminTeleport(allow)
 * 
 * FIXES:
 *     AllowTeleport
 */

#if defined _ALS_AllowAdminTeleport
	#error _ALS_AllowAdminTeleport defined
#endif
native BAD_AllowAdminTeleport(allow) = AllowAdminTeleport;

#if FIX_AllowTeleport
	#if FIXES_Single
		stock FIXES_AllowAdminTeleport(allow)
		{
			if (allow)
			{
				FIXES_gsSettings |= e_FIXES_SETTINGS_ADMIN_TELEPORT;
			}
			else
			{
				FIXES_gsSettings &= ~e_FIXES_SETTINGS_ADMIN_TELEPORT;
			}
			return allow;
		}
	#else
		stock FIXES_AllowAdminTeleport(allow)
		{
			return
				CallRemoteFunction(FIXES_gscAllowTeleport, FIXES_gscSpec@ii, INVALID_PLAYER_ID, allow),
				allow;
		}
	#endif
	
	#define _ALS_AllowAdminTeleport
	#define AllowAdminTeleport FIXES_AllowAdminTeleport
#endif

/* 
 * FIXES_AllowAdminTeleport(allow)
 * 
 * FIXES:
 *     AllowTeleport
 */

#if FIX_AllowTeleport && !defined FILTERSCRIPT && !FIXES_Single
	public _FIXES_AllowTeleport(playerid, allow)
	{
		if (!_FIXES_gIsFilterscript)
		{
			if (playerid == INVALID_PLAYER_ID)
			{
				if (allow)
				{
					FIXES_gsSettings |= e_FIXES_SETTINGS_ADMIN_TELEPORT;
				}
				else
				{
					FIXES_gsSettings &= ~e_FIXES_SETTINGS_ADMIN_TELEPORT;
				}
			}
			else
			{
				if (allow)
				{
					FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_TELEPORT;
				}
				else
				{
					FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_TELEPORT;
				}
			}
		}
	}
#endif

/* 
 * FIXES_SetPlayerSpecialAction(playerid, actionid)
 * 
 * FIXES:
 *     SetPlayerSpecialAction
 */

#if defined _ALS_SetPlayerSpecialAction
	#error _ALS_SetPlayerSpecialAction defined
#endif
native BAD_SetPlayerSpecialAction(playerid, actionid) = SetPlayerSpecialAction;

#if FIX_SetPlayerSpecialAction
	stock FIXES_SetPlayerSpecialAction(playerid, actionid)
	{
		if (GetPlayerSpecialAction(playerid) == SPECIAL_ACTION_USEJETPACK)
		{
			ClearAnimations(playerid);
		}
		SetPlayerSpecialAction(playerid, actionid);
		return 1;
	}
	
	#define _ALS_SetPlayerSpecialAction
	#define SetPlayerSpecialAction FIXES_SetPlayerSpecialAction
#endif

/* 
 * FIXES_ClearAnimations(playerid, forcesync = 0)
 * 
 * FIXES:
 *     ClearAnimations
 */

#if defined _ALS_ClearAnimations
	#error _ALS_ClearAnimations defined
#endif
native BAD_ClearAnimations(playerid, forcesync = 0) = ClearAnimations;

#if FIX_ClearAnimations || FIX_ClearAnimations_2
	stock FIXES_ClearAnimations(playerid, forcesync = 0)
	{
		#if FIX_ClearAnimations || FIX_ClearAnimations_2
			if (IsPlayerInAnyVehicle(playerid))
		#endif
		#if FIX_ClearAnimations
			{
				return ApplyAnimation(playerid, "PED", "CAR_SIT", 4.0, 0, 0, 0, 0, 1, forcesync);
			}
		#endif
		#if FIX_ClearAnimations + FIX_ClearAnimations_2 == 1 // XOR
			{
				return ClearAnimations(playerid, forcesync);
			}
		#endif
		#if FIX_ClearAnimations_2
			new
				ret = ClearAnimations(playerid, forcesync);
			ApplyAnimation(playerid, "PED", "IDLE_STANCE", 4.0, 0, 0, 0, 0, 1, forcesync);
			ApplyAnimation(playerid, "PED", "IDLE_CHAT", 4.0, 0, 0, 0, 0, 1, forcesync);
			ApplyAnimation(playerid, "PED", "WALK_PLAYER", 4.0, 0, 0, 0, 0, 1, forcesync);
			return ret;
		#endif
	}
	
	#define _ALS_ClearAnimations
	#define ClearAnimations FIXES_ClearAnimations
#endif

/* 
 * FIXES_GangZoneCreate(Float:minx, Float:miny, Float:maxx, Float:maxy)
 * 
 * FIXES:
 *     GangZoneCreate
 */

#if defined _ALS_GangZoneCreate
	#error _ALS_GangZoneCreate defined
#endif
native BAD_GangZoneCreate(Float:minx, Float:miny, Float:maxx, Float:maxy) = GangZoneCreate;

#if FIX_GangZoneCreate
	stock FIXES_GangZoneCreate(Float:minx, Float:miny, Float:maxx, Float:maxy)
	{
		return GangZoneCreate(floatsub(minx, floatfract(minx)), floatsub(miny, floatfract(miny)), floatsub(maxx, floatfract(maxx)), floatsub(maxy, floatfract(maxy)));
	}
	
	#define _ALS_GangZoneCreate
	#define GangZoneCreate FIXES_GangZoneCreate
#endif

/* 
 * FIXES_ShowPlayerDialog(playerid, dialog, style, title[], caption[], button1[], button2[])
 * 
 * FIXES:
 *     OnDialogResponse
 *     GetPlayerDialog
 */

#if defined _ALS_ShowPlayerDialog
	#error _ALS_ShowPlayerDialog defined
#endif
native BAD_ShowPlayerDialog(playerid, dialog, style, title[], caption[], button1[], button2[]) = ShowPlayerDialog;

#if FIX_OnDialogResponse || FIX_GetPlayerDialog
	stock FIXES_ShowPlayerDialog(playerid, dialog, style, title[], caption[], button1[], button2[])
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			#if FIXES_Single
				FIXES_gsDialogID[playerid] = dialog;
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerDialog, dialog);
			#endif
			return ShowPlayerDialog(playerid, dialog, style, title, caption, button1, button2);
		}
		return 0;
	}
	
	#define _ALS_ShowPlayerDialog
	#define ShowPlayerDialog FIXES_ShowPlayerDialog
#endif

/* 
 * OnDialogResponse(playerid, dialogid, response, listitem, inputtext[])
 * 
 * FIXES:
 *     OnDialogResponse
 *     GetPlayerDialog
 */

#if FIX_OnDialogResponse || FIX_GetPlayerDialog
	public OnDialogResponse(playerid, dialogid, response, listitem, inputtext[])
	{
		// =========================
		//  BEGIN: OnDialogResponse 
		// =========================
		#if FIX_OnDialogResponse || FIX_GetPlayerDialog
			#if FIXES_Single
				#if FIX_OnDialogResponse
					dialogid = FIXES_gsDialogID[playerid];
				#endif
				FIXES_gsDialogID[playerid] = INVALID_DIALOG_ID;
			#elseif FIX_OnDialogResponse
				_FIXES_IS_IN_CHARGE()
				{
					// If there are multiple scripts, we can't just wipe the
					// current dialog data or subsequent scripts will display
					// the wrong value.
					dialogid = GetPVarInt(playerid, FIXES_pvarPlayerDialog),
					SetPVarInt(playerid, FIXES_pvarCurrentDialog, dialogid),
					SetPVarInt(playerid, FIXES_pvarPlayerDialog, INVALID_DIALOG_ID);
				}
				else
				{
					dialogid = GetPVarInt(playerid, FIXES_pvarCurrentDialog);
				}
			#else
				SetPVarInt(playerid, FIXES_pvarPlayerDialog, INVALID_DIALOG_ID);
			#endif
			if (inputtext[0])
			{
				return FIXES_OnDialogResponse(playerid, dialogid, response, listitem, inputtext);
			}
			else
			{
				return FIXES_OnDialogResponse(playerid, dialogid, response, listitem, FIXES_gscNULL);
			}
		#endif
		// =========================
		//  END:   OnDialogResponse 
		// =========================
	}
	
	#if defined _ALS_OnDialogResponse
		#error _ALS_OnDialogResponse defined
	#endif
	#define _ALS_OnDialogResponse
	#define OnDialogResponse(%0) FIXES_OnDialogResponse(%0) <_ALS : _ALS_go>
	
	_FIXES_FORWARD FIXES_OnDialogResponse(playerid, dialogid, response, listitem, inputtext[]);
#endif

/* 
 * FIXES_GetPlayerDialog(playerid)
 * 
 * FIXES:
 *     GetPlayerDialog
 */

#if defined _ALS_GetPlayerDialog
	#error _ALS_GetPlayerDialog defined
#endif
native BAD_GetPlayerDialog(playerid) = GetPlayerDialog;

#if FIX_GetPlayerDialog
	stock FIXES_GetPlayerDialog(playerid)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			#if FIXES_Single
				return FIXES_gsDialogID[playerid];
			#else
				return GetPVarInt(playerid, FIXES_pvarPlayerDialog);
			#endif
		}
		return -1;
	}
	
	#define _ALS_GetPlayerDialog
	#define GetPlayerDialog FIXES_GetPlayerDialog
#endif

/* 
 * FIXES_valstr(dest[], value, bool:pack = false)
 * 
 * FIXES:
 *     valstr
 */

#if defined _ALS_valstr
	#error _ALS_valstr defined
#endif
native BAD_valstr(dest[], value, bool:pack = false) = valstr;

#if FIX_valstr
	stock FIXES_valstr(dest[], value, bool:pack = false)
	{
		// "format" can't handle cellmin properly.
		static const
			sc_szCellmin[] = !"-2147483648";
		if (value == cellmin)
		{
			pack && strpack(dest, sc_szCellmin, 12) || strunpack(dest, sc_szCellmin, 12);
		}
		else
		{
			format(dest, 12, "%d", value),
			pack && strpack(dest, dest, 12);
		}
		return 0;
	}
	
	#define _ALS_valstr
	#define valstr FIXES_valstr
#endif

/* 
 * FIXES_fclose(File:handle)
 * 
 * FIXES:
 *     fclose
 */

#if defined _ALS_fclose
	#error _ALS_fclose defined
#endif
native BAD_fclose(File:handle) = fclose;

#if FIX_fclose
	stock bool:FIXES_fclose(File:handle)
	{
		return handle && fclose(handle);
	}
	
	#define _ALS_fclose
	#define fclose FIXES_fclose
#endif

/* 
 * FIXES_fwrite(File:handle, const string[])
 * 
 * FIXES:
 *     fwrite
 */

#if defined _ALS_fwrite
	#error _ALS_fwrite defined
#endif
native BAD_fwrite(File:handle, const string[]) = fwrite;

#if FIX_fwrite
	stock FIXES_fwrite(File:handle, const string[])
	{
		return handle && fwrite(handle, string);
	}
	
	#define _ALS_fwrite
	#define fwrite FIXES_fwrite
#endif

/* 
 * FIXES_fread(File:handle, string[], size = sizeof (string), bool:pack = false)
 * 
 * FIXES:
 *     fread
 */

#if defined _ALS_fread
	#error _ALS_fread defined
#endif
native BAD_fread(File:handle, string[], size = sizeof (string), bool:pack = false) = fread;

#if FIX_fread
	stock FIXES_fread(File:handle, string[], size = sizeof (string), bool:pack = false)
	{
		return handle && fread(handle, string, size, pack);
	}
	
	#define _ALS_fread
	#define fread FIXES_fread
#endif

/* 
 * FIXES_fputchar(File:handle, value, bool:utf8 = true)
 * 
 * FIXES:
 *     fputchar
 */

#if defined _ALS_fputchar
	#error _ALS_fputchar defined
#endif
native BAD_fputchar(File:handle, value, bool:utf8 = true) = fputchar;

#if FIX_fputchar
	stock bool:FIXES_fputchar(File:handle, value, bool:utf8 = true)
	{
		return handle && fputchar(handle, value, utf8);
	}
	
	#define _ALS_fputchar
	#define fputchar FIXES_fputchar
#endif

/* 
 * FIXES_fgetchar(File:handle, value, bool:utf8 = true)
 * 
 * FIXES:
 *     fgetchar
 */

#if defined _ALS_fgetchar
	#error _ALS_fgetchar defined
#endif
native BAD_fgetchar(File:handle, value, bool:utf8 = true) = fgetchar;

#if FIX_fgetchar
	stock FIXES_fgetchar(File:handle, value, bool:utf8 = true)
	{
		return handle && fgetchar(handle, value, utf8);
	}
	
	#define _ALS_fgetchar
	#define fgetchar FIXES_fgetchar
#endif

/* 
 * FIXES_fblockwrite(File:handle, const buffer[], size = sizeof (buffer))
 * 
 * FIXES:
 *     fblockwrite
 */

#if defined _ALS_fblockwrite
	#error _ALS_fblockwrite defined
#endif
native BAD_fblockwrite(File:handle, const buffer[], size = sizeof (buffer)) = fblockwrite;

#if FIX_fblockwrite
	stock FIXES_fblockwrite(File:handle, const buffer[], size = sizeof (buffer))
	{
		return handle && fblockwrite(handle, buffer, size);
	}
	
	#define _ALS_fblockwrite
	#define fblockwrite FIXES_fblockwrite
#endif

/* 
 * FIXES_fblockread(File:handle, buffer[], size = sizeof (buffer))
 * 
 * FIXES:
 *     fblockread
 */

#if defined _ALS_fblockread
	#error _ALS_fblockread defined
#endif
native BAD_fblockread(File:handle, buffer[], size = sizeof (buffer)) = fblockread;

#if FIX_fblockread
	stock FIXES_fblockread(File:handle, buffer[], size = sizeof (buffer))
	{
		return handle && fblockread(handle, buffer, size);
	}
	
	#define _ALS_fblockread
	#define fblockread FIXES_fblockread
#endif

/* 
 * FIXES_fseek(File:handle, position = 0, seek_whence:whence = seek_start)
 * 
 * FIXES:
 *     fseek
 */

#if defined _ALS_fseek
	#error _ALS_fseek defined
#endif
native BAD_fseek(File:handle, position = 0, seek_whence:whence = seek_start) = fseek;

#if FIX_fseek
	stock FIXES_fseek(File:handle, position = 0, seek_whence:whence = seek_start)
	{
		return handle && fseek(handle, position, whence);
	}
	
	#define _ALS_fseek
	#define fseek FIXES_fseek
#endif

/* 
 * FIXES_flength(File:handle)
 * 
 * FIXES:
 *     flength
 */

#if defined _ALS_flength
	#error _ALS_flength defined
#endif
native BAD_flength(File:handle) = flength;

#if FIX_flength
	stock FIXES_flength(File:handle)
	{
		return handle && flength(handle);
	}
	
	#define _ALS_flength
	#define flength FIXES_flength
#endif

/* 
 * FIXES_Kick(playerid)
 * 
 * FIXES:
 *     Kick
 */

#if defined _ALS_Kick
	#error _ALS_Kick defined
#endif
native BAD_Kick(playerid) = Kick;

#if FIX_Kick
	forward _FIXES_Kick(playerid);
	
	public _FIXES_Kick(playerid)
	{
		FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_KICKED,
		Kick(playerid);
	}
	
	#if !FIXES_Single
		forward @FIXES_Kick(playerid);
		
		public @FIXES_Kick(playerid)
		{
			FIXES_BlockUpdate(playerid, true, 0);
		}
	#endif
	
	stock FIXES_Kick(playerid)
	{
		if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_ON_PLAYER_CONNECT)
		{
			// Disable as much of the server as possible quickly.
			return
				#if FIXES_Single
					FIXES_BlockUpdate(playerid, true, 0),
				#else
					CallRemoteFunction("@FIXES_Kick", "i", playerid),
				#endif
					TogglePlayerSpectating(playerid, true),
					TogglePlayerControllable(playerid, false),
					SetPVarInt(playerid, FIXES_pvarKick, SetTimerEx(FIXES_gscKick, 1000, 0, FIXES_gscSpec@i, playerid)),
					FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_KICKED,
					1;
		}
		else
		{
			return Kick(playerid);
		}
	}
	
	#define _ALS_Kick
	#define Kick FIXES_Kick
#endif

/* 
 * FIXES_IsPAttachedObjectSlotUsed(playerid, index)
 * 
 * FIXES:
 *     IsPlayerAttachedObjectSlotUsed
 */

#if defined _ALS_IsPAttachedObjSlotUsed
	#error _ALS_IsPAttachedObjSlotUsed defined
#endif
native BAD_IsPlayerAttachedObjSlotUsed(playerid, index) = IsPlayerAttachedObjectSlotUsed;

#if FIX_IsPlayerAttachedObjSlotUsed
	stock FIXES_IsPAttachedObjectSlotUsed(playerid, index)
	{
		if (_FIXES_IS_PLAYER_CONNECTED(playerid))
		{
			return FIXES_gsObjectSlots[playerid / _FIXES_ATTACHMENTS] & 1 << playerid % _FIXES_ATTACHMENTS * MAX_PLAYER_ATTACHED_OBJECTS + index;
		}
		return 0;
	}
	
	#define _ALS_IsPAttachedObjSlotUsed
	#define IsPlayerAttachedObjectSlotUsed FIXES_IsPAttachedObjectSlotUsed
#endif

/* 
 * _FIXES_SetPlayerAttachedObject(slot, to)
 * 
 * FIXES:
 *     IsPlayerAttachedObjectSlotUsed
 */

#if (FIX_IsPlayerAttachedObjSlotUsed || FIX_SetPlayerAttachedObject) && !FIXES_Single
	forward _FIXES_SetPlayerAttachedObject(slot, to);
	
	public _FIXES_SetPlayerAttachedObject(slot, to)
	{
		FIXES_gsObjectSlots[slot] = to;
	}
#endif

/* 
 * FIXES_SetPlayerAttachedObject(playerid, index, modelid, bone, Float:fOffsetX = 0.0, Float:fOffsetY = 0.0, Float:fOffsetZ = 0.0, Float:fRotX = 0.0, Float:fRotY = 0.0, Float:fRotZ = 0.0, Float:fScaleX = 1.0, Float:fScaleY = 1.0, Float:fScaleZ = 1.0)
 * 
 * FIXES:
 *     IsPlayerAttachedObjectSlotUsed
 *     SetPlayerAttachedObject
 */

#if defined _ALS_SetPlayerAttachedObject
	#error _ALS_SetPlayerAttachedObject defined
#endif
native BAD_SetPlayerAttachedObject(playerid, index, modelid, bone, Float:fOffsetX = 0.0, Float:fOffsetY = 0.0, Float:fOffsetZ = 0.0, Float:fRotX = 0.0, Float:fRotY = 0.0, Float:fRotZ = 0.0, Float:fScaleX = 1.0, Float:fScaleY = 1.0, Float:fScaleZ = 1.0, materialcolor1 = 0, materialcolor2 = 0) = SetPlayerAttachedObject;

#if FIX_IsPlayerAttachedObjSlotUsed || FIX_SetPlayerAttachedObject
	stock FIXES_SetPlayerAttachedObject(playerid, index, modelid, bone, Float:fOffsetX = 0.0, Float:fOffsetY = 0.0, Float:fOffsetZ = 0.0, Float:fRotX = 0.0, Float:fRotY = 0.0, Float:fRotZ = 0.0, Float:fScaleX = 1.0, Float:fScaleY = 1.0, Float:fScaleZ = 1.0, materialcolor1 = 0, materialcolor2 = 0)
	{
		if (SetPlayerAttachedObject(playerid, index, modelid, bone, fOffsetX, fOffsetY, fOffsetZ, fRotX, fRotY, fRotZ, fScaleX, fScaleY, fScaleZ, materialcolor1, materialcolor2))
		{
			#if FIXES_Single
				FIXES_gsObjectSlots[playerid / _FIXES_ATTACHMENTS] |= (1 << playerid % _FIXES_ATTACHMENTS * MAX_PLAYER_ATTACHED_OBJECTS + index);
			#else
				new
					slot = playerid / _FIXES_ATTACHMENTS;
				CallRemoteFunction(FIXES_gscSetPlayerAttachedObj, FIXES_gscSpec@ii, slot, FIXES_gsObjectSlots[slot] | (1 << playerid % _FIXES_ATTACHMENTS * MAX_PLAYER_ATTACHED_OBJECTS + index));
			#endif
			return 1;
		}
		return 0;
	}
	
	#define _ALS_SetPlayerAttachedObject
	#define SetPlayerAttachedObject FIXES_SetPlayerAttachedObject
#endif

/* 
 * FIXES_RemovePlayerAttachedObj(playerid, index)
 * 
 * FIXES:
 *     IsPlayerAttachedObjectSlotUsed
 */

#if defined _ALS_RemovePlayerAttachedObject
	#error _ALS_RemovePlayerAttachedObject defined
#endif
native BAD_RemovePlayerAttachedObject(playerid, index) = RemovePlayerAttachedObject;

#if FIX_IsPlayerAttachedObjSlotUsed || FIX_SetPlayerAttachedObject
	stock FIXES_RemovePlayerAttachedObj(playerid, index)
	{
		if (RemovePlayerAttachedObject(playerid, index))
		{
			#if FIXES_Single
				FIXES_gsObjectSlots[playerid / _FIXES_ATTACHMENTS] &= ~(1 << playerid % _FIXES_ATTACHMENTS * MAX_PLAYER_ATTACHED_OBJECTS + index);
			#else
				new
					slot = playerid / _FIXES_ATTACHMENTS;
				CallRemoteFunction(FIXES_gscSetPlayerAttachedObj, FIXES_gscSpec@ii, slot, FIXES_gsObjectSlots[slot] & ~(1 << playerid % _FIXES_ATTACHMENTS * MAX_PLAYER_ATTACHED_OBJECTS + index));
			#endif
			return 1;
		}
		return 0;
	}
	
	#define _ALS_RemovePlayerAttachedObject
	#define RemovePlayerAttachedObject FIXES_RemovePlayerAttachedObj
#endif

/* 
 * _FIXES_DetermineOrder()
 * 
 * Figure out which the first filterscript to be called is so that it can do the
 * majority of the work.  More detail:  Several pieces of code for some fixes
 * rely on things like blocking "OnPlayerUpdate".  This should be done in the
 * first script in which "OnPlayerUpdate" is called only, so we need to find out
 * in advance which script will be called first (at least out of all the fixed
 * scripts).
 */

#if !FIXES_Single
	public _FIXES_DetermineOrder()
	{
		// TODO: Graceful handoff.  I thought I was past all this rubbish!
		if (!_FIXES_gIsFilterscript)
		{
			if (existproperty(5, FIXES_gscNoGMProperty))
			{
				return 0;
			}
			// Make sure certain data is synced with what the GM thinks it is.
			// =============================
			//  BEGIN: AllowInteriorWeapons 
			// =============================
			#if FIX_AllowInteriorWeapons
				CallRemoteFunction(FIXES_gscAllowInteriorWeapons, FIXES_gscSpec@i, !(FIXES_gsSettings & e_FIXES_SETTINGS_INTERIOR));
			#endif
			// =============================
			//  END: AllowInteriorWeapons 
			// =============================
			
			// ================================
			//  BEGIN: SetPlayerAttachedObject 
			// ================================
			#if FIX_IsPlayerAttachedObjSlotUsed || FIX_SetPlayerAttachedObject
				for (new i = 0; i != _FIXES_ATTACHMENTS; ++i)
				{
					CallRemoteFunction(FIXES_gscSetPlayerAttachedObj, FIXES_gscSpec@ii, i, FIXES_gsObjectSlots[i]);
				}
			#endif
			// ==============================
			//  END: SetPlayerAttachedObject 
			// ==============================
		}
		if (existproperty(5, FIXES_gscOrderProperty))
		{
			// Either this is a Filter Script (1) and we don't want gamemodes
			// (1) or this isn't a filterscript (0) and we do want gamemodes
			// (0).  All other cases return here due to a logic mismatch.  This
			// is to rectify the difference in call orders between
			// "CallRemoteFunction" and normal callbacks.  Now DOESN'T end on
			// the second time round for filterscripts to correctly pass on the
			// data to GameModes.
			if (!existproperty(5, FIXES_gscNoGMProperty) && _FIXES_gIsFilterscript == bool:getproperty(5, FIXES_gscOrderProperty))
			{
				// The game mode is not in charge, which means we are checking
				// the filterscripts a second time and shouldn't be.
				return 0;
			}
			// If this script was previously the owner, and we are not ending
			// the game mode (in which case discard all data), and if we are
			// only dealing with filterscripts or the gamemode is now the
			// master.
			if (FIXES_gsSettings & (e_FIXES_SETTINGS_IN_CHARGE | e_FIXES_SETTINGS_DROP_ALL_DATA) == e_FIXES_SETTINGS_IN_CHARGE)
			{
				FIXES_PRINTF("_FIXES_DetermineOrder: Was in charge");
				// This script is currently in charge, but a new script has
				// usurped it!  Pass all relevant data over to the new script.
				// Also, this wasn't triggered by a game mode change (where we
				// just want to dump and reset all data).  There is no need to
				// reset the admin teleport data as that is always handled by
				// the Game Mode, so it will either be correct, or it will be
				// blank.
				FIXES_gsSettings &= ~e_FIXES_SETTINGS_IN_CHARGE;
				#if FIX_SetPlayerWorldBounds || FIX_TogglePlayerControllable
					_FIXES_FOREACH(FIXES_gsPlayersIterator, i)
					{
						// =================
						//  BEGIN: GameText 
						// =================
						#if FIX_GameText
							for (new j = 0; j != FIXES_GT_STYLE_COUNT; ++j)
							{
								PlayerTextDrawDestroy(i, FIXES_gsPGTStyle[i][j]);
							}
						#endif
						// =================
						//  END:   GameText 
						// =================
						
						// =============================
						//  BEGIN: SetPlayerWorldBounds 
						// =============================
						#if FIX_SetPlayerWorldBounds
							// I realised that you could have the GM in charge,
							// set their bounds, load an FS, remove their bounds
							// then unload the FS again and it would result in
							// the old GM setting of them HAVING bounds being
							// re-applied, so the check is removed.
							CallRemoteFunction(FIXES_gscSetPlayerWorldBounds, FIXES_gscSpec@iffff, i, FIXES_gsWorldbounds[i][E_FIXES_WORLDBOUND_DATA_UX], FIXES_gsWorldbounds[i][E_FIXES_WORLDBOUND_DATA_LX], FIXES_gsWorldbounds[i][E_FIXES_WORLDBOUND_DATA_UY], FIXES_gsWorldbounds[i][E_FIXES_WORLDBOUND_DATA_LY]);
						#endif
						// =============================
						//  END:   SetPlayerWorldBounds 
						// =============================
						
						// =================================
						//  BEGIN: TogglePlayerControllable 
						// =================================
						#if FIX_TogglePlayerControllable
							CallRemoteFunction(FIXES_gscTogglePlayerControl, FIXES_gscSpec@ii, i, !(FIXES_gsPlayerBools[i] & e_FIXES_BOOLS_UNCONTROLLABLE));
						#endif
						// =================================
						//  END:   TogglePlayerControllable 
						// =================================
						
						// ===========================
						//  BEGIN: PutPlayerInVehicle 
						// ===========================
						#if FIX_PutPlayerInVehicle
							if (FIXES_gsPlayerBools[i] & e_FIXES_BOOLS_PUT_IN_VEHICLE)
							{
								CallRemoteFunction(FIXES_gscPutPlayerInVehicle, FIXES_gscSpec@iii, i, FIXES_gsVehicleSeatData[i] & 0x00FFFFFF, FIXES_gsVehicleSeatData[i] >>> 24);
							}
						#endif
						// ===========================
						//  END:   PutPlayerInVehicle 
						// ===========================
					}
				#endif
				
				// =================
				//  BEGIN: GameText 
				// =================
				#if FIX_GameText
					#if FIX_GameTextStyles
						TextDrawDestroy(FIXES_gsGTStyle[13]),
						TextDrawDestroy(FIXES_gsGTStyle[12]),
						TextDrawDestroy(FIXES_gsGTStyle[11]),
						TextDrawDestroy(FIXES_gsGTStyle[10]),
						TextDrawDestroy(FIXES_gsGTStyle[9]),
						TextDrawDestroy(FIXES_gsGTStyle[8]),
						TextDrawDestroy(FIXES_gsGTStyle[7]),
					#endif
						TextDrawDestroy(FIXES_gsGTStyle[6]),
						TextDrawDestroy(FIXES_gsGTStyle[5]),
						TextDrawDestroy(FIXES_gsGTStyle[4]),
						TextDrawDestroy(FIXES_gsGTStyle[3]),
						TextDrawDestroy(FIXES_gsGTStyle[2]),
						TextDrawDestroy(FIXES_gsGTStyle[1]),
						TextDrawDestroy(FIXES_gsGTStyle[0]);
				#endif
				// =================
				//  END:   GameText 
				// =================
				
				FIXES_PRINTF("_FIXES_DetermineOrder: Not in charge");
				return 0;
			}
			else
			{
				FIXES_gsSettings &= ~e_FIXES_SETTINGS_IN_CHARGE;
				FIXES_PRINTF("_FIXES_DetermineOrder: Not in charge");
				return 0;
			}
		}
		else if (!(FIXES_gsSettings & e_FIXES_SETTINGS_ENDING))
		{
			setproperty(5, FIXES_gscOrderProperty, _FIXES_gIsFilterscript);
			#if FIXES_Debug
				if (FIXES_gsSettings & e_FIXES_SETTINGS_IN_CHARGE)
				{
					FIXES_PRINTF("_FIXES_DetermineOrder: Already in charge");
				}
			#endif
			FIXES_gsSettings |= e_FIXES_SETTINGS_IN_CHARGE;
			FIXES_PRINTF("_FIXES_DetermineOrder: Now in charge");
			
			// =================
			//  BEGIN: GameText 
			// =================
			#if FIX_GameText
				// Create all the relevant TextDraws.
				_FIXES_FOREACH(FIXES_gsPlayersIterator, playerid)
				{
					_FIXES_CreateGameTextDraws(playerid);
				}
				_FIXES_CreateGameTextDraws(INVALID_PLAYER_ID);
			#endif
			// =================
			//  END:   GameText 
			// =================
			return 1;
		}
		else if (!(FIXES_gsSettings & e_FIXES_SETTINGS_ENDED))
		{
			// Only called when a filterscript that is currently master (and
			// thus called first by "CallRemoteFunction") ends.  This doesn't
			// get triggered when a gamemode is master, but that doesn't matter
			// because in that case all the textdraws are destroyed anyway.
			
			// =================
			//  BEGIN: GameText 
			// =================
			#if FIX_GameText
					// Destroy all the relevant TextDraws.
					_FIXES_FOREACH(FIXES_gsPlayersIterator, playerid)
					{
						#if FIX_GameTextStyles
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][13]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][12]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][11]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][10]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][9]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][8]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][7]),
						#endif
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][6]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][5]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][4]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][3]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][2]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][1]),
							PlayerTextDrawDestroy(playerid, FIXES_gsPGTStyle[playerid][0]);
					}
				#if FIX_GameTextStyles
					TextDrawDestroy(FIXES_gsGTStyle[13]),
					TextDrawDestroy(FIXES_gsGTStyle[12]),
					TextDrawDestroy(FIXES_gsGTStyle[11]),
					TextDrawDestroy(FIXES_gsGTStyle[10]),
					TextDrawDestroy(FIXES_gsGTStyle[9]),
					TextDrawDestroy(FIXES_gsGTStyle[8]),
					TextDrawDestroy(FIXES_gsGTStyle[7]),
				#endif
					TextDrawDestroy(FIXES_gsGTStyle[6]),
					TextDrawDestroy(FIXES_gsGTStyle[5]),
					TextDrawDestroy(FIXES_gsGTStyle[4]),
					TextDrawDestroy(FIXES_gsGTStyle[3]),
					TextDrawDestroy(FIXES_gsGTStyle[2]),
					TextDrawDestroy(FIXES_gsGTStyle[1]),
					TextDrawDestroy(FIXES_gsGTStyle[0]);
			#endif
			// =================
			//  END:   GameText 
			// =================
			
			#if FIXES_Debug
				FIXES_PRINTF("_FIXES_DetermineOrder: Ending");
			#endif
			
			FIXES_gsSettings |= e_FIXES_SETTINGS_ENDED;
		}
		return 0;
	}
#endif

/* 
 * FIXES_strins(string[], const substr[], pos, maxlength = sizeof string)
 * 
 * FIXES:
 *     strins
 */

#if defined _ALS_strins
	#error _ALS_strins defined
#endif
native BAD_strins(string[], const substr[], pos, maxlength = sizeof string) = strins;

#if FIX_strins
	stock FIXES_strins(string[], const substr[], pos, maxlength = sizeof string)
	{
		if (string[0] > 255)
		{
			new
				strlength = strlen(string),
				sublength = strlen(substr),
				m4 = maxlength * 4;
			// Packed - format doesn't like these strings.
			if (strlength + sublength >= m4)
			{
				if (pos + sublength >= m4)
				{
					return
						string{pos} = '\0',
						// Hopefully this doesn't ignore maxlength and does packed.
						strcat(string, substr, maxlength);
				}
				else
				{
					// pos + sublength is less than maxlength, so this sum MUST
					// be positive and gte than "pos", so there's no need for
					// additional checks.
					string{maxlength - sublength - 1} = '\0';
				}
			}
			return strins(string, substr, pos, maxlength);
		}
		else if (substr[0] > 255)
		{
			new
				strlength = strlen(string),
				sublength = strlen(substr);
			// Packed - format doesn't like these strings.
			if (strlength + sublength >= maxlength)
			{
				if (pos + sublength >= maxlength)
				{
					return
						string[pos] = '\0',
						// Hopefully this doesn't ignore maxlength and does packed.
						strcat(string, substr, maxlength);
				}
				else
				{
					// pos + sublength is less than maxlength, so this sum MUST
					// be positive and gte than "pos", so there's no need for
					// additional checks.
					string[maxlength - sublength - 1] = '\0';
				}
			}
			return strins(string, substr, pos, maxlength);
		}
		else
		{
			return format(string, maxlength, "%.*s%s%s", pos, string, substr, string[pos]);
		}
	}
	
	#define _ALS_strins
	#define strins FIXES_strins
#endif

/* 
 * _FIXES_ApplyAnimation(playerid, animlib, Float:fDelta, loop, lockx, locky, freeze, time, forcesync)
 * 
 * FIXES:
 *     ApplyAnimation_2
 */

#if FIX_ApplyAnimation_2
	forward _FIXES_ApplyAnimation(playerid, animlib, Float:fDelta, loop, lockx, locky, freeze, time, forcesync);
	
	public _FIXES_ApplyAnimation(playerid, animlib, Float:fDelta, loop, lockx, locky, freeze, time, forcesync)
	{
		ApplyAnimation(playerid, FIXES_gscAnimLib[animlib], FIXES_gsClassAnimName[playerid], fDelta, loop, lockx, locky, freeze, time, forcesync),
		FIXES_gsAnimTimer[playerid] = 0;
	}
#endif

/* 
 * FIXES_ApplyAnimation(playerid, animlib[], animname[], Float:fDelta, loop, lockx, locky, freeze, time, forcesync = 0)
 * 
 * FIXES:
 *     ApplyAnimation
 *     ApplyAnimation_2
 */

#if defined _ALS_ApplyAnimation
	#error _ALS_ApplyAnimation defined
#endif
native BAD_ApplyAnimation(playerid, animlib[], animname[], Float:fDelta, loop, lockx, locky, freeze, time, forcesync = 0) = ApplyAnimation;

#if FIX_ApplyAnimation || FIX_ApplyAnimation_2
	stock FIXES_ApplyAnimation(playerid, animlib[], animname[], Float:fDelta, loop, lockx, locky, freeze, time, forcesync = 0)
	{
		new
			diff,
			idx = animlib[0] & ~0x20;
		// Uses a sort of optimised binary search.  The code first identifies the area in the array
		// in which libraries with this first letter are, then does a binary search using only that
		// subset of the array.  This used to use an N-ary search that just went linearly through
		// the identified subset of the array, and that was 5x faster than a simple linear loop over
		// the whole array.  This new version is 50% faster than even that was.  "E" has no
		// libraries, but we don't check for that explicitly as it would slow down the more common
		// code path - and it ends fairly quickly anyway as "upper == lower".
		if (_FIXES_IN_RANGE(idx, 'A', 'W' + 1))
		{
			new
				upper = FIXES_gscAnimIndexes[idx - ('A' - 1)],
				lower = FIXES_gscAnimIndexes[idx - 'A'];
			while (upper != lower)
			{
				idx = (upper - lower) / 2 + lower;
				if ((diff = strcmp(FIXES_gscAnimLib[idx], animlib, true)))
				{
					if (diff > 0) upper = idx;
					else lower = idx + 1;
				}
				else
				{
					// If we hit the "else" branch, we have found the correct
					// animation library from the n-ary search.
					#if FIX_ApplyAnimation_2
						if (FIXES_gsAnimTimer[playerid])
						{
							KillTimer(FIXES_gsAnimTimer[playerid]),
							FIXES_gsAnimTimer[playerid] = 0;
						}
						if (FIXES_gsPlayerAnimLibs[playerid][idx >>> 5] & (1 << (idx & 0x1F)))
						{
							FIXES_gsPlayerAnimLibs[playerid][idx >>> 5] &= ~(1 << (idx & 0x1F)),
							FIXES_gsClassAnimName[playerid][0] = '\0',
							strcat(FIXES_gsClassAnimName[playerid], animname),
							FIXES_gsAnimTimer[playerid] = SetTimerEx("_FIXES_ApplyAnimation", 350, false, "ddfdddddd", playerid, idx, fDelta, loop, lockx, locky, freeze, time, forcesync);
						}
					#endif
						return ApplyAnimation(playerid, animlib, animname, fDelta, loop, lockx, locky, freeze, time, forcesync);
				}
			}
		}
		return 0;
	}
	
	#define _ALS_ApplyAnimation
	#define ApplyAnimation FIXES_ApplyAnimation
#endif

/* 
 * FIXES_random(max)
 * 
 * FIXES:
 *     random
 */

#if defined _ALS_random
	#error _ALS_random defined
#endif
native BAD_random(max) = random;

#if FIX_random
	stock FIXES_random(max)
	{
		if (max < 0)
		{
			return -random(-max);
		}
		else
		{
			return random(max);
		}
	}
	
	#define _ALS_random
	#define random FIXES_random
#endif

/* 
 * _FIXES_SetCamera
 * 
 * FIXES:
 *     SetPlayerCamera
 */

#if FIX_SetPlayerCamera
	forward _FIXES_SetCamera(playerid, type, Float:x, Float:y, Float:z);

	public _FIXES_SetCamera(playerid, type, Float:x, Float:y, Float:z)
	{
		if (type)
		{
			SetPlayerCameraPos(playerid, x, y, z);
		}
		else
		{
			SetPlayerCameraLookAt(playerid, x, y, z);
		}
		return 0;
	}
#endif

/* 
 * FIXES_SetPlayerCameraLookAt(playerid, Float:x, Float:y, Float:z)
 * 
 * FIXES:
 *     SetPlayerCamera
 */

#if defined _ALS_SetPlayerCameraPos
	#error _ALS_SetPlayerCameraPos defined
#endif
native BAD_SetPlayerCameraPos(playerid, Float:x, Float:y, Float:z) = SetPlayerCameraPos;

#if FIX_SetPlayerCamera
	stock FIXES_SetPlayerCameraPos(playerid, Float:x, Float:y, Float:z)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			#if FIXES_Single
				if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_SPECTATING || GetPlayerState(playerid) == PLAYER_STATE_SPECTATING)
			#else
				if (GetPVarInt(playerid, FIXES_pvarPlayerSpectate) || GetPlayerState(playerid) == PLAYER_STATE_SPECTATING)
			#endif
				{
					SetTimerEx(FIXES_gscSetCamera, 300, 0, FIXES_gscSpec@iifff, playerid, 1, x, y, z);
				}
				else
				{
					SetPlayerCameraPos(playerid, x, y, z);
				}
		}
		return 0;
	}
	
	#define _ALS_SetPlayerCameraPos
	#define SetPlayerCameraPos FIXES_SetPlayerCameraPos
#endif

/* 
 * FIXES_SetPlayerCameraLookAt(playerid, Float:x, Float:y, Float:z)
 * 
 * FIXES:
 *     SetPlayerCamera
 */

#if defined _ALS_SetPlayerCameraLookAt
	#error _ALS_SetPlayerCameraLookAt defined
#endif
native BAD_SetPlayerCameraLookAt(playerid, Float:x, Float:y, Float:z) = SetPlayerCameraLookAt;

#if FIX_SetPlayerCamera
	stock FIXES_SetPlayerCameraLookAt(playerid, Float:x, Float:y, Float:z)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			#if FIXES_Single
				if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_SPECTATING || GetPlayerState(playerid) == PLAYER_STATE_SPECTATING)
			#else
				if (GetPVarInt(playerid, FIXES_pvarPlayerSpectate) || GetPlayerState(playerid) == PLAYER_STATE_SPECTATING)
			#endif
				{
					SetTimerEx(FIXES_gscSetCamera, 300, 0, FIXES_gscSpec@iifff, playerid, 0, x, y, z);
				}
				else
				{
					SetPlayerCameraLookAt(playerid, x, y, z);
				}
		}
		return 0;
	}
	
	#define _ALS_SetPlayerCameraLookAt
	#define SetPlayerCameraLookAt FIXES_SetPlayerCameraLookAt
#endif

/* 
 * FIXES_TogglePlayerSpectating(playerid, toggle)
 * 
 * FIXES:
 *     SetPlayerCamera
 */

#if defined _ALS_TogglePlayerSpectating
	#error _ALS_TogglePlayerSpectating defined
#endif
native BAD_TogglePlayerSpectating(playerid, toggle) = TogglePlayerSpectating;

#if FIX_SetPlayerCamera
	stock FIXES_TogglePlayerSpectating(playerid, toggle)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			if (toggle)
			{
				#if FIXES_Single
					FIXES_gsPlayerBools[playerid] |= e_FIXES_BOOLS_SPECTATING;
				#else
					SetPVarInt(playerid, FIXES_pvarPlayerSpectate, 1);
				#endif
			}
			else
			{
				#if FIXES_Single
					FIXES_gsPlayerBools[playerid] &= ~e_FIXES_BOOLS_SPECTATING;
				#else
					DeletePVar(playerid, FIXES_pvarPlayerSpectate);
				#endif
			}
			return TogglePlayerSpectating(playerid, toggle);
		}
		return 0;
	}
	
	#define _ALS_TogglePlayerSpectating
	#define TogglePlayerSpectating FIXES_TogglePlayerSpectating
#endif

/* 
 * FIXES_SetPlayerTime(playerid, hour, minute)
 * 
 * FIXES:
 *     SetPlayerTime
 */

#if defined _ALS_SetPlayerTime
	#error _ALS_SetPlayerTime defined
#endif
native BAD_SetPlayerTime(playerid, hour, minute) = SetPlayerTime;

#if FIX_SetPlayerTime
	forward _FIXES_SetTime(playerid, hour, minute);
	
	public _FIXES_SetTime(playerid, hour, minute)
	{
		if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_ON_PLAYER_CONNECT)
		{
			return SetPlayerTime(playerid, hour, minute);
		}
		return 0;
	}
	
	stock FIXES_SetPlayerTime(playerid, hour, minute)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_ON_PLAYER_CONNECT)
			{
				return SetTimerEx(FIXES_gscSetTime, 250, 0, FIXES_gscSpec@iii, playerid, hour, minute);
			}
			return SetPlayerTime(playerid, hour, minute);
		}
		return 0;
	}
	
	#define _ALS_SetPlayerTime
	#define SetPlayerTime FIXES_SetPlayerTime
#endif

/* 
 * FIXES_SetPlayerColor(playerid, color)
 * 
 * FIXES:
 *     SetPlayerColor
 */

#if defined _ALS_SetPlayerColor
	#error _ALS_SetPlayerColor defined
#endif
native BAD_SetPlayerColor(playerid, color) = SetPlayerColor;

#if FIX_SetPlayerColour
	forward _FIXES_SetColor(playerid, color);
	
	public _FIXES_SetColor(playerid, color)
	{
		if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_ON_PLAYER_CONNECT)
		{
			return SetPlayerColor(playerid, color);
		}
		return 0;
	}
	
	stock FIXES_SetPlayerColor(playerid, color)
	{
		if (_FIXES_IN_RANGE(playerid, 0, MAX_PLAYERS))
		{
			if (FIXES_gsPlayerBools[playerid] & e_FIXES_BOOLS_ON_PLAYER_CONNECT)
			{
				return SetTimerEx(FIXES_gscSetColor, 300, 0, FIXES_gscSpec@ii, playerid, color);
			}
			return SetPlayerColor(playerid, color);
		}
		return 0;
	}
	
	#define _ALS_SetPlayerColor
	#define SetPlayerColor FIXES_SetPlayerColor
#endif

/* 
 * FIXES_GetPlayerWeaponData(playerid, slot, &weapons, &ammo)
 * 
 * FIXES:
 *     GetPlayerWeaponData
 */

#if defined _ALS_GetPlayerWeaponData
	#error _ALS_GetPlayerWeaponData defined
#endif
native BAD_GetPlayerWeaponData(playerid, slot, &weapons, &ammo) = GetPlayerWeaponData;

#if FIX_GetPlayerWeaponData
	stock FIXES_GetPlayerWeaponData(playerid, slot, &weapons, &ammo)
	{
		// This reuses the "slot" variable so we don't have to declare a new one
		// and can put all the code in a single statement.
		return
			slot = GetPlayerWeaponData(playerid, slot, weapons, ammo),
			weapons = ammo ? weapons : 0,
			slot;
	}
	
	#define _ALS_GetPlayerWeaponData
	#define GetPlayerWeaponData FIXES_GetPlayerWeaponData
#endif

/* 
 * FIXES_sleep(const time)
 * 
 * FIXES:
 *     sleep
 */

// Uses a little trick to consume part of the line and thus not match
// our hooked version.
#if defined _ALS_sleep
	#error _ALS_sleep defined
#endif
#define BAD_sleep%0\n%9 sleep%0

#if FIX_sleep
	stock FIXES_sleep(ms)
	{
		// Call a native function that does very little, but saves the current
		// heap pointer.  Then return to save the accurate stack pointer.
		return heapspace(), ms;
	}
	
	#define _ALS_sleep
	
	#define sleep%0\n%9 sleep FIXES_sleep(%0)
	// This fixes another BIZZARE bug.  Just doing:
	//
	//  #define FIXES_sleep(%0;) FIXES_sleep(%0)
	//
	// Results in:
	//
	//  FIXES_sleep(n));
	//
	// Which clearly it shouldn't.  I've stepped through the compilation and that
	// extra bracket comes from nowhere!
	#define FIXES_sleep(%0;) FIXES_sleep _FIXES_SLEEP_BRACKET %0);
	#define _FIXES_SLEEP_BRACKET (
#endif

/* 
 * _FIXES_AddInternal(array[], value, size)
 * 
 * Add something to an internal linked list.
 */

static stock _FIXES_AddInternal(array[], value, size)
{
	if (array[value] <= value)
	{
		new
			last = size,
			next = array[last];
		while (next < value)
		{
			last = next,
			next = array[last];
		}
		array[next - 1] = value + 1,
		array[(value - 1) % (size + 1)] = (last + 1) % (size + 1),
		array[last] = value,
		array[value] = next;
	}
}

/* 
 * _FIXES_RemoveInternal(array[], value, size)
 * 
 * Remove something from an internal linked list.
 */

static stock _FIXES_RemoveInternal(array[], value, size)
{
	if (array[value] > value)
	{
		static
			last;
		// Adjustment for easier "mod"ing.
		++size,
		last = (array[(value - 1) % size] - 1) % size,
		// Store the reverse value here as well as in the previous slot.
		array[value] =
			// Copy the next value to the last value.
			array[(array[last] = array[value]) - 1] =
				// Set the reverse iterator value.
				(last + 1) % size;
	}
}

#undef _FIXES_CEILDIV
#undef _FIXES_INFINITY
#undef _FIXES_N_INFINITY
#undef _FIXES_ATTACHMENTS
#undef _FIXES_FOREACH
#undef _FIXES_IS_UNSET
#undef _FIXES_IS_IN_CHARGE
#undef _FIXES_IN_RANGE
#undef _FIXES_NO_RANGE
#undef _FIXES_FORWARD
#undef _FIXES_IS_PLAYER_CONNECTED
#undef FIXES_PRINTF

#endinput

// Fix inclusion template.

#if !defined FIX_NameOfFixHere
	#define FIX_NameOfFixHere            (1)
#elseif _FIXES_IS_UNSET(FIX_NameOfFixHere)
	#undef FIX_NameOfFixHere
	#define FIX_NameOfFixHere            (2)
#endif

// Fix function template.

/* 
 * FIXES_NameOfFixHere
 * 
 * FIXES:
 *     NameOfFixHere
 */

#if defined _ALS_NameOfFixHere
	#error _ALS_NameOfFixHere defined
#endif
native BAD_NameOfFixHere(params) = NameOfFixHere;

#if FIX_NameOfFixHere
	stock FIXES_NameOfFixHere(params)
	{
		return 0;
	}
	
	#define _ALS_NameOfFixHere
	#define NameOfFixHere FIXES_NameOfFixHere
#endif

