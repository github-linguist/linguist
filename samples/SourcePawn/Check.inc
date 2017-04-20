// 	Checkpoint Manager
// 	(c) João Pedro Lopes, All right's reserved
// 	
// 	Feel free to change any line above, as long as you post the 'fix' / 'patch' at the forum
// 	and send a copy to jplopes@live.com.pt

/* natives
	native CreateCheckpoint(ownerid, chpid, Float:posX, Float:posY, Float:posZ, Float:size); // Creates a checkpoint
	native SetCheckpointInterior(chpid, interiorid); // Changes the checkpoint interior
	native SetCheckpointVirtualWorld(chpid, VirtualWorldID); // Changes the Checkpoint vWorld
	native ToggleCheckpointActive(chpid, bool:active); // Deactivates / Activates the checkpoint
	native ChangeCheckpointOwner(chpid, owner);	// Change the checkpoint owner
	native RemoveCheckpoint(chpid); // Removes the specified checkpoint
	native StartCheckpointSeeking(); // Starts seeking for each individual
	native StopCheckpointSeeking(); // Stops the system
	native VerifyCheckpoint(playerid); // Place this at OnPlayerEnterCheckpoint
*/

// Function Forwards
forward public OnCheckpointEnter(playerid, checkpointid);

#if defined _CHECKPOINT_MANAGER_INCLUDED
	#endinput
#endif

#define _CHECKPOINT_MANAGER_INCLUDED
#pragma library CheckpointManager

#include <a_samp>

#define MAX_CHECKPOINTS 200
#define CHECKPOINT_SEEKER_DELAY 300

#define GLOBAL_OWNER_ID -1


// CHECKPOINT ENUMERATION
enum _checkpointEnum{
	_chp_populated,			// Is this slot of the memory populated?
	
	_chp_id,				// The ID of the checkpoint
	_chp_owner,				// The ID of the player who this checkpoint is visible too
	
	Float:_chp_posX,		// The X position of this checkpoint
	Float:_chp_posY,		// The Y position of this checkpoint
	Float:_chp_posZ,		// The Z position of this checkpoint
	Float:_chp_size,		// The checkpoint size
	Float:_chp_viewDistance,	// The checkpoint view distance
	
	bool:_chp_active,		// Is this checkpoint active?
	
	_chp_interior_id,		// The interior id of this checkpoint
	_chp_world_id			// The world id of this checkpoint
};

// DATA ARRAYS
new _checkpoints[MAX_CHECKPOINTS][_checkpointEnum];
new _p_VisibleCheckpoint[MAX_PLAYERS];
new _chp_manager_timer_id;

// DATA VARIABLES
new _totalCheckpoints;

// --------------------------------------------------------------------------------------------------------
// Creates a new checkpoint with some initial data
stock CreateCheckpoint(__ownerid, __chpid, Float:__posX, Float:__posY, Float:__posZ, Float:__size){
	// Max checkpoint reached?
	if(_totalCheckpoints == MAX_CHECKPOINTS) return 0;
	
	// First checkpoint? Setting everything to unpopulated
	if(!_totalCheckpoints){
		for(new i; i < MAX_PLAYERS; i++) _p_VisibleCheckpoint[i] = -1;
		for(new i; i < MAX_CHECKPOINTS; i++){
			_checkpoints[i][_chp_populated] = false;
		}
		
		// Sending the Initialization Info
		printf("[Checkpoint Manager : Version 0.1.1b] System Initialized...", __chpid);
	}
	
	// Getting the first open slot
	new _slot;
	for(new i = 0; i < MAX_CHECKPOINTS; i++){
		if(!_checkpoints[i][_chp_populated]){
			_slot = i;
			break;
		}
	}
	
	// Adding the new checkpoint
	_checkpoints[_slot][_chp_populated] = true;
	_checkpoints[_slot][_chp_id] = __chpid;
	_checkpoints[_slot][_chp_owner] = __ownerid;
	_checkpoints[_slot][_chp_posX] = __posX;
	_checkpoints[_slot][_chp_posY] = __posY;
	_checkpoints[_slot][_chp_posZ] = __posZ;
	_checkpoints[_slot][_chp_size] = __size;
	_checkpoints[_slot][_chp_viewDistance] = 50.0;
	_checkpoints[_slot][_chp_active] = true;
	_checkpoints[_slot][_chp_interior_id] = 0;
	_checkpoints[_slot][_chp_world_id] = 0;
	
	printf("[Checkpoint Manager] Checkpoint created (%d) at slot %d", __chpid, _slot);
	printf("Checkpoint Position: { %f, %f, %f }", _checkpoints[_slot][_chp_posX], _checkpoints[_slot][_chp_posY], _checkpoints[_slot][_chp_posZ]);
	
	_totalCheckpoints++;
	return 1;
}

//---------------------------------------------------------------------------------------------
stock SetCheckpointInterior(__chpid, __interiorid){
	new _slot = __ChpSlotByID(__chpid);
	if(_slot > -1){
		// Valid slot?
		_checkpoints[_slot][_chp_interior_id] = __interiorid;
		return 1;
	}
	return 0;
}

//---------------------------------------------------------------------------------------------
stock SetCheckpointVirtualWorld(__chpid, __virtual_world_id){
	new _slot = __ChpSlotByID(__chpid);
	if(_slot > -1){
		_checkpoints[_slot][_chp_world_id] = __virtual_world_id;
		return 1;
	}
	return 0;
}

stock ToggleCheckpointActive(__chpid, bool:__active){
	new _slot = __ChpSlotByID(__chpid);
	if(_slot > -1){
		_checkpoints[_slot][_chp_active] = __active;
		return 1;
	}
	return 0;
}

stock ChangeCheckpointOwner(__chpid, __owner){
	new _slot = __ChpSlotByID(__chpid);
	if(_slot > -1){
		_checkpoints[_slot][_chp_owner] = __owner;
		return 1;
	}
	return 0;
}

stock RemoveCheckpoint(__chpid){
	new _slot = __ChpSlotByID(__chpid);
	if(_slot > -1){
		// Deleting the checkpoint
		_checkpoints[_slot][_chp_populated] = false;
		_checkpoints[_slot][_chp_id] = -1;
		_checkpoints[_slot][_chp_owner] = 255;
		_checkpoints[_slot][_chp_posX] = -1;
		_checkpoints[_slot][_chp_posY] = -1;
		_checkpoints[_slot][_chp_posZ] = -1;
		_checkpoints[_slot][_chp_size] = -1;
		_checkpoints[_slot][_chp_viewDistance] = -1;
		_checkpoints[_slot][_chp_active] = false;
		_checkpoints[_slot][_chp_interior_id] = -1;
		_checkpoints[_slot][_chp_world_id] = -1;
		_totalCheckpoints--;
		printf("\n[Checkpoint Manager] Checkpoint removed (ID: %d)", __chpid);
		return 1;
	}
	return 0;
}

//---------------------------------------------------------------------------------------------
// Gets the checkpoint slot by id
stock __ChpSlotByID(__chpid){
	for(new i; i < MAX_CHECKPOINTS; i++){
		if(_checkpoints[i][_chp_id] == __chpid) return i;
	}
	return -1;
}


forward CheckpointSeeker();
stock StartCheckpointSeeking(){
	_chp_manager_timer_id = SetTimer("CheckpointSeeker", CHECKPOINT_SEEKER_DELAY, 1);
	return 1;
}

stock StopCheckpointSeeking(){
	KillTimer(_chp_manager_timer_id);
	return 1;
}

public CheckpointSeeker(){
	new Float:__posX, Float:__posY, Float:__posZ;
	new __interior;
	new __virtualWorld;
	for(new i; i < MAX_PLAYERS; i++)
	{
		if(!IsPlayerConnected(i)) continue;
		
		GetPlayerPos(i, Float:__posX, Float:__posY, Float:__posZ);
		// Is the player near a checkpoint?
		if(_p_VisibleCheckpoint[i] > -1)
		{
			// If the player is no longer near that point
			if(__posX < (_checkpoints[_p_VisibleCheckpoint[i]][_chp_posX] - _checkpoints[_p_VisibleCheckpoint[i]][_chp_viewDistance])
			|| __posX > (_checkpoints[_p_VisibleCheckpoint[i]][_chp_posX] + _checkpoints[_p_VisibleCheckpoint[i]][_chp_viewDistance])
			|| __posY < (_checkpoints[_p_VisibleCheckpoint[i]][_chp_posY] - _checkpoints[_p_VisibleCheckpoint[i]][_chp_viewDistance])
			|| __posY > (_checkpoints[_p_VisibleCheckpoint[i]][_chp_posY] + _checkpoints[_p_VisibleCheckpoint[i]][_chp_viewDistance])){
				DisablePlayerCheckpoint(i);
				_p_VisibleCheckpoint[i] = -1;
			}
		}
		else
		{
			// Getting the player Interior and virtual world
			__interior = GetPlayerInterior(i);
			__virtualWorld = GetPlayerVirtualWorld(i);
			
			// Looking for a new checkpoint
			for(new j = 0; j < MAX_CHECKPOINTS; j++){
				if(!_checkpoints[j][_chp_populated]) continue;
				if((_checkpoints[j][_chp_owner] != i) && (_checkpoints[j][_chp_owner] != -1)) continue;
				if(_checkpoints[j][_chp_interior_id] != __interior) continue;
				if(_checkpoints[j][_chp_world_id] != __virtualWorld) continue;
				
				if(__posX > (_checkpoints[j][_chp_posX] - _checkpoints[j][_chp_viewDistance])
				&& __posX < (_checkpoints[j][_chp_posX] + _checkpoints[j][_chp_viewDistance])
				&& __posY > (_checkpoints[j][_chp_posY] - _checkpoints[j][_chp_viewDistance])
				&& __posY < (_checkpoints[j][_chp_posY] + _checkpoints[j][_chp_viewDistance])){
					SetPlayerCheckpoint(i, _checkpoints[j][_chp_posX], _checkpoints[j][_chp_posY], _checkpoints[j][_chp_posZ], _checkpoints[j][_chp_size]);
					_p_VisibleCheckpoint[i] = j;
					break;
				}
			}
		}
	}
	return 1;
}

stock VerifyCheckpoint(__playerid){
	if(_p_VisibleCheckpoint[__playerid] >= 0){
		OnCheckpointEnter(__playerid, _checkpoints[_p_VisibleCheckpoint[__playerid]][_chp_id]);
		return 1;
	}
	return 0;
}