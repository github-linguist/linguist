/*
 * Author: commy2
 *
 * Execute a function on a remote machine in mp.
 * 
 * Argument:
 * 0: Function arguments (Array)
 * 1: Function to execute, has to be defined on the remote machine first (String)
 * 2: The function will be executed where this unit is local OR the mode were this function should be executed. (Object OR Number, optional default: 2)
 * 		Mode 0: execute on this machine only
 * 		Mode 1: execute on server
 * 		Mode 2: execute on all clients + server
 * 		Mode 3: execute on dedicated only
 * 
 * Return value:
 * Nothing
 */

private ["_arguments", "_function", "_unit", "_id"];

AGM_Core_remoteFnc = _this;

_arguments = _this select 0;
_function = call compile (_this select 1);
_unit = _this select 2;

if (isNil "_unit") then {
	_unit = 2;
};

if (typeName _unit == "SCALAR") exitWith {
	switch (_unit) do {
		case 0 : {
			_arguments call _function;
		};
		case 1 : {
			if (isServer) then {
				_arguments call _function;
			} else {
				publicVariableServer "AGM_Core_remoteFnc";
			};
		};
		case 2 : {
			_arguments call _function;

			AGM_Core_remoteFnc set [2, 0];
			publicVariable "AGM_Core_remoteFnc";
		};
		case 3 : {
			if (isDedicated) then {
				_arguments call _function;
			} else {
				if (!isServer) then {publicVariableServer "AGM_Core_remoteFnc"};
			};
		};
	};
};

if (local _unit) then {
	_arguments call _function;
} else {
	if (isServer) then {
		_id = owner _unit;
		_id publicVariableClient "AGM_Core_remoteFnc";
	} else {
		publicVariableServer "AGM_Core_remoteFnc";
	};
};