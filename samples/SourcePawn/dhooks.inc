#if defined _dhooks_included
#endinput
#endif
#define _dhooks_included

// Needed for the SDKFuncConfSource enum.
#include <sdktools>

#define INVALID_HOOK_ID 0

enum ObjectValueType
{
	ObjectValueType_Int = 0,
	ObjectValueType_Bool,
	ObjectValueType_Ehandle,
	ObjectValueType_Float,
	ObjectValueType_CBaseEntityPtr,
	ObjectValueType_IntPtr,
	ObjectValueType_BoolPtr,
	ObjectValueType_EhandlePtr,
	ObjectValueType_FloatPtr,
	ObjectValueType_Vector,
	ObjectValueType_VectorPtr,
	ObjectValueType_CharPtr,
	ObjectValueType_String
};

enum ListenType
{
	ListenType_Created,
	ListenType_Deleted
};

enum ReturnType
{
	ReturnType_Unknown,
	ReturnType_Void,
	ReturnType_Int,
	ReturnType_Bool,
	ReturnType_Float,
	ReturnType_String, //Note this is a string_t
	ReturnType_StringPtr, //Note this is a string_t *
	ReturnType_CharPtr,
	ReturnType_Vector,
	ReturnType_VectorPtr,
	ReturnType_CBaseEntity,
	ReturnType_Edict
};

enum HookParamType
{
	HookParamType_Unknown,
	HookParamType_Int,
	HookParamType_Bool,
	HookParamType_Float,
	HookParamType_String, //Note this is a string_t
	HookParamType_StringPtr, //Note this is a string_t *
	HookParamType_CharPtr,
	HookParamType_VectorPtr,
	HookParamType_CBaseEntity,
	HookParamType_ObjectPtr,
	HookParamType_Edict,
	HookParamType_Object
};

enum ThisPointerType
{
	ThisPointer_Ignore,
	ThisPointer_CBaseEntity,
	ThisPointer_Address
};

enum HookType
{
	HookType_Entity,
	HookType_GameRules,
	HookType_Raw
};

enum CallingConvention
{
	CallConv_CDECL,
	CallConv_THISCALL,
	CallConv_STDCALL,
	CallConv_FASTCALL,
};

enum HookMode
{
	Hook_Pre,                   // Callback will be executed BEFORE the original function.
	Hook_Post                   // Callback will be executed AFTER the original function.
};

enum MRESReturn
{
	MRES_ChangedHandled = -2,	// Use changed values and return MRES_Handled
	MRES_ChangedOverride,		// Use changed values and return MRES_Override
	MRES_Ignored,				// plugin didn't take any action
	MRES_Handled,				// plugin did something, but real function should still be called
	MRES_Override,				// call real function, but use my return value
	MRES_Supercede				// skip real function; use my return value
};

enum DHookPassFlag
{
	DHookPass_ByVal = 		(1<<0),		/**< Passing by value */
	DHookPass_ByRef = 		(1<<1),		/**< Passing by reference */
	DHookPass_ODTOR =		(1<<2),		/**< Object has a destructor */
	DHookPass_OCTOR =		(1<<3),		/**< Object has a constructor */
	DHookPass_OASSIGNOP	=	(1<<4),		/**< Object has an assignment operator */
};

enum DHookRegister
{
	// Don't change the register and use the default for the calling convention.
	DHookRegister_Default,
	
	// 8-bit general purpose registers
	DHookRegister_AL,
	DHookRegister_CL,
	DHookRegister_DL,
	DHookRegister_BL,
	DHookRegister_AH,
	DHookRegister_CH,
	DHookRegister_DH,
	DHookRegister_BH,
	
	// 32-bit general purpose registers
	DHookRegister_EAX,
	DHookRegister_ECX,
	DHookRegister_EDX,
	DHookRegister_EBX,
	DHookRegister_ESP,
	DHookRegister_EBP,
	DHookRegister_ESI,
	DHookRegister_EDI,
	
	// 128-bit XMM registers
	DHookRegister_XMM0,
	DHookRegister_XMM1,
	DHookRegister_XMM2,
	DHookRegister_XMM3,
	DHookRegister_XMM4,
	DHookRegister_XMM5,
	DHookRegister_XMM6,
	DHookRegister_XMM7,
	
	// 80-bit FPU registers
	DHookRegister_ST0
};

typeset ListenCB
{
	//Deleted
	function void (int entity);
	
	//Created
	function void (int entity, const char[] classname);
};

typeset DHookRemovalCB
{
	function void (int hookid);
};
typeset DHookCallback
{
	//Function Example: void Ham::Test() with this pointer ignore
	function MRESReturn ();
	
	//Function Example: void Ham::Test() with this pointer passed
	function MRESReturn (int pThis);
	
	//Function Example: void Ham::Test(int cake) with this pointer ignore
	function MRESReturn (DHookParam hParams);
	
	//Function Example: void Ham::Test(int cake) with this pointer passed
	function MRESReturn (int pThis, DHookParam hParams);
	
	//Function Example: int Ham::Test() with this pointer ignore
	function MRESReturn (DHookReturn hReturn);
	
	//Function Example: int Ham::Test() with this pointer passed
	function MRESReturn (int pThis, DHookReturn hReturn);
	
	//Function Example: int Ham::Test(int cake) with this pointer ignore
	function MRESReturn (DHookReturn hReturn, DHookParam hParams);
	
	//Function Example: int Ham::Test(int cake) with this pointer passed
	function MRESReturn (int pThis, DHookReturn hReturn, DHookParam hParams);
	
	//Address NOW
	
	//Function Example: void Ham::Test() with this pointer passed
	function MRESReturn (Address pThis);
	
	//Function Example: void Ham::Test(int cake) with this pointer passed
	function MRESReturn (Address pThis, DHookParam hParams);
	
	//Function Example: int Ham::Test() with this pointer passed
	function MRESReturn (Address pThis, DHookReturn hReturn);
	
	//Function Example: int Ham::Test(int cake) with this pointer passed
	function MRESReturn (Address pThis, DHookReturn hReturn, DHookParam hParams);
	
};

// Represents the parameters of the hooked function.
methodmap DHookParam < Handle
{
	// Get the value of a parameter.
	// Use only for: int, entity, edict, bool or float parameter types.
	//
	// @param num           Parameter number to get, starting at 1. Parameter number 0 returns
	//                      the number of parameters.
	//
	// @return              Value if num greater than 0. If 0 returns parameter count.
	//                      If CBaseEntity returns entity index.
	// @error               Invalid handle, invalid param number or invalid param type.
	public native any Get(int num);

	// Get the value of a vector parameter.
	// Use only for: vector or vectorptr parameter types.
	//
	// @param num           Parameter number to get, starting at 1.
	// @param vec           Vector buffer to store result.
	//
	// @error               Invalid handle, invalid param number or invalid param type.
	public native void GetVector(int num, float vec[3]);

	// Get the value of a string parameter.
	// Use only for: string, stringptr or charptr parameter types.
	//
	// @param num           Parameter number to get, starting at 1.
	// @param buffer        String buffer to store result.
	// @param size          Buffer size.
	// 
	// @error               Invalid handle, invalid param number or invalid param type.
	public native void GetString(int num, char[] buffer, int size);

	// Set the value of a parameter.
	// Use only for: int, entity, edict, bool or float parameter types.
	//
	// The changes are only applied when MRES_ChangedHandled or MRES_ChangedOverride
	// is returned in the callback.
	//
	// @param num           Parameter number to set starting at 1.
	// @param value         Value to set it as (only pass int, bool, float or entity index).
	//
	// @error               Invalid handle, invalid param number or invalid param type.
	public native void Set(int num, any value);

	// Set the value of a vector parameter.
	// Use only for: vector or vectorptr parameter types.
	//
	// The changes are only applied when MRES_ChangedHandled or MRES_ChangedOverride
	// is returned in the callback.
	//
	// @param num           Parameter number to set, starting at 1.
	// @param vec           Value to set vector as.
	//
	// @error               Invalid handle, invalid param number or invalid param type.
	public native void SetVector(int num, const float vec[3]);

	// Set the value of a string parameter.
	// Use only for: string, stringptr or charptr parameter types.
	//
	// The changes are only applied when MRES_ChangedHandled or MRES_ChangedOverride
	// is returned in the callback.
	//
	// @param num           Parameter number to set, starting at 1.
	// @param value         Value to set string as.
	//
	// @error               Invalid handle, invalid param number or invalid param type.
	public native void SetString(int num, const char[] value);

	// Gets an object's variable value.
	//
	// @param num           Parameter number to get, starting at 1.
	// @param offset        Byte offset within the object to the var to get.
	// @param type          Type of var it is.
	//
	// @return              Value of the objects var. If EHANDLE type or entity returns entity index.
	// @error               Invalid handle, invalid param number, invalid param type or invalid Object type.
	public native any GetObjectVar(int num, int offset, ObjectValueType type);

	// Gets an object's vector variable value.
	//
	// @param num           Parameter number to get, starting at 1.
	// @param offset        Byte offset within the object to the var to get.
	// @param type          Type of var it is.
	// @param vec           Buffer to store the result vector.
	//
	// @error               Invalid handle, invalid param number, invalid param type or invalid Object type.
	public native void GetObjectVarVector(int num, int offset, ObjectValueType type, float vec[3]);

	// Gets an object's string variable value.
	//
	// @param num           Parameter number to get, starting at 1.
	// @param offset        Byte offset within the object to the var to get.
	// @param type          Type of var it is.
	// @param buffer        Buffer to store the result string.
	// @param size          Size of the buffer.
	//
	// @error               Invalid handle, invalid param number, invalid param type or invalid Object type.
	public native void GetObjectVarString(int num, int offset, ObjectValueType type, char[] buffer, int size);

	// Sets an object's variable value.
	//
	// The changes are only applied when MRES_ChangedHandled or MRES_ChangedOverride
	// is returned in the callback.
	//
	// @param num           Parameter number to set, starting at 1.
	// @param offset        Byte offset within the object to the var to set.
	// @param type          Type of var it is.
	// @param value         The value to set the var to.
	//
	// @error               Invalid handle, invalid param number, invalid param type or invalid Object type.
	public native void SetObjectVar(int num, int offset, ObjectValueType type, any value);

	// Sets an object's vector variable value.
	//
	// The changes are only applied when MRES_ChangedHandled or MRES_ChangedOverride
	// is returned in the callback.
	//
	// @param num           Parameter number to set, starting at 1.
	// @param offset        Byte offset within the object to the var to set.
	// @param type          Type of var it is.
	// @param vec           The value to set the vector var to.
	//
	// @error               Invalid handle, invalid param number, invalid param type or invalid Object type.
	public native void SetObjectVarVector(int num, int offset, ObjectValueType type, const float vec[3]);

	// No setter for object strings yet. Open an issue if you really need it.

	// Checks if a pointer parameter is null.
	//
	// @param num           Parameter number to check, starting at 1.
	//
	// @return              True if null, false otherwise.
	// @error               Non-pointer parameter.
	public native bool IsNull(int num);

	// Get param address (Use only for ptr param types)
	// 
	// @param num			Param number to get. (Example if the function has 2 params and you need the value of the first param num would be 1.)
	// 
	// @error Invalid handle. Invalid param number. Invalid param type.
	// @return address of the parameter.
	public native Address GetAddress(int num);
};


// Represents the return value of the hooked function.
methodmap DHookReturn < Handle
{
 	// Retrieves or sets the return value.
 	// Use only for: int, entity, edict, bool or float return types.
 	//
 	// The return value is only readable in a post hook.
 	// The value is only applied when MRES_Override or MRES_Supercede is returned
 	// in the callback.
	property any Value {
		public native get();
		public native set(any value);
	}

	// Get return vector value.
	// Use only for: vector or vectorptr return types.
	//
	// Only useful in post hooks.
	//
	// @param vec           Vector buffer to store result in.
	//
	// @error               Invalid Handle or invalid type.
	public native void GetVector(float vec[3]);

	// Get return string value.
	// Use only for: string, stringptr or charptr return types.
	//
	// Only useful in post hooks.
	//
	// @param buffer        String buffer to store result in.
	// @param size          String buffer size.
	//
	// @error               Invalid Handle or invalid type.
	public native void GetString(char[] buffer, int size);

	// Set return vector value.
	// Use only for: vector or vectorptr return types.
	//
	// The value is only applied when MRES_Override or MRES_Supercede is returned
 	// in the callback.
	//
	// @param vec           Value to set return vector to.
	//
	// @error               Invalid Handle or invalid type.
	public native void SetVector(const float vec[3]);

	// Set return string value.
	// Use only for: string, stringptr or charptr return types.
	//
	// The value is only applied when MRES_Override or MRES_Supercede is returned
 	// in the callback.
	//
	// @param buffer        Value to set return string to.
	//
	// @error               Invalid Handle or invalid type.
	public native void SetString(const char[] buffer);
};

// Base method map for common functions between virtual hooks and detours.
methodmap DHookSetup < Handle
{
	// Load address or offset for a vtable hook or detour from a gamedata file.
	//
	// @param gameconf      GameData handle.
	// @param source        Whether to look in Offsets, Signatures, or Addresses.
	// @param name          Name of the property to find.
	//
	// @return              True on success, false if nothing was found.
	// @error               Invalid setup or gamedata handle.
	public native bool SetFromConf(Handle gameconf, SDKFuncConfSource source, const char[] name);

	// Adds a parameter to a hook setup.
	//
	// @param type              Parameter type.
	// @param size              Used for Objects (not Object ptr) to define the size of the object.
	// @param flag              Used to change the pass type (ignored by detours).
	// @param custom_register   The register this argument is passed in instead of the stack (ignored by vhooks).
	// 
	// @error                   Invalid setup handle or too many params added (request upping the max in thread).
	public native void AddParam(HookParamType type, int size=-1, DHookPassFlag flag=DHookPass_ByVal, DHookRegister custom_register=DHookRegister_Default);
};

// A DynamicHook allows to hook a virtual function on any C++ object.
// Currently CBaseEntity and CGameRules have a convenience API for easy entity hooking,
// but it's possible to provide a raw this-pointer to hook any object in memory too.
// 
// Internally this intercepts function calls by replacing the function pointer
// in the virtual table of the object with our own function.
methodmap DynamicHook < DHookSetup
{
	// Creates a vtable hook.
 	//
 	// @param offset        Virtual table offset of function to hook.
 	// @param hooktype      Type of hook.
	// @param returntype    Type of return value.
	// @param thistype      Type of this pointer or ignore (ignore can be used if not needed).
	// 
	// @error               Failed to create hook setup handle or invalid callback function.
	public native DynamicHook(int offset, HookType hooktype, ReturnType returntype, ThisPointerType thistype);

	// Setup a vtable hook for a function as described in a "Functions" section in gamedata.
	// The "Functions" section is parsed once the gamedata file is loaded and cached globally.
	//
	// @param gameconf      GameData handle to use for address lookup.
	//                      Doesn't have to be the same as the one with the "Functions" section.
	// @param name          Name of the function in a "Functions" section to load.
	//
	// @return              Setup handle for the detour or null if offset wasn't found.
	// @error               Failed to create detour setup handle, invalid gamedata handle,
	//                      invalid callback function or failed to find function in cached "Functions" sections.
	public static native DynamicHook FromConf(Handle gameconf, const char[] name);

	// Hook an entity.
	//
	// Entity hooks are auto-removed when the entity is destroyed.
	// If you need to read the return value of the function, choose a post hook.
	//
	// @param mode          The desired hook mode - pre or post.
	//                      A pre hook calls your callback BEFORE the original function is called. 
	//                        You can access the parameters, set the return value, and skip the original function.
	//                      A post hook calls your callback AFTER the original function executed.
	//                        You can access the parameters and get/set the return value.
	// @param entity        Entity index to hook on.
	// @param callback      Callback function.
	// @param removalcb     Optional callback for when the hook is removed.
	// 
	// @return              A hookid on success, INVALID_HOOK_ID otherwise.
	// @error               Invalid setup handle, invalid address, invalid hook type or invalid callback.
	public native int HookEntity(HookMode mode, int entity, DHookCallback callback, DHookRemovalCB removalcb=INVALID_FUNCTION);

	// Hook gamerules object.
	//
	// Game rules hooks are auto-removed on map end.
	// If you need to read the return value of the function, choose a post hook.
	//
	// @param mode          The desired hook mode - pre or post.
	//                      A pre hook calls your callback BEFORE the original function is called. 
	//                        You can access the parameters, set the return value, and skip the original function.
	//                      A post hook calls your callback AFTER the original function executed.
	//                        You can access the parameters and get/set the return value.
	// @param callback      Callback function.
	// @param removalcb     Optional callback for when the hook is removed.
	// 
	// @return              A hookid on success, INVALID_HOOK_ID otherwise.
	// @error               Invalid setup handle, invalid address, invalid hook type or invalid callback.
	public native int HookGamerules(HookMode mode, DHookCallback callback, DHookRemovalCB removalcb=INVALID_FUNCTION);

	// Hook a raw this-pointer.
	// If you need to read the return value of the function, choose a post hook.
	//
	// @param mode          The desired hook mode - pre or post.
	//                      A pre hook calls your callback BEFORE the original function is called. 
	//                        You can access the parameters, set the return value, and skip the original function.
	//                      A post hook calls your callback AFTER the original function executed.
	//                        You can access the parameters and get/set the return value.
	// @param addr          This pointer address.
	// @param callback      Callback function.
	// 
	// @return              A hookid on success, INVALID_HOOK_ID otherwise.
	// @error               Invalid setup handle, invalid address, invalid hook type or invalid callback.
	public native int HookRaw(HookMode mode, Address addr, DHookCallback callback);

	// Remove hook by hook id:
	// This will NOT fire the removal callback!
	// 
	// @param hookid        Hook id to remove.
	// 
	// @return              True on success, false otherwise
	public static native bool RemoveHook(int hookid);
};

// A DynamicDetour is a way to hook and block any function in memory.
// Given the address of a function, it can call a callback in your script whenever
// the function gets called. The callback has access to all parameters of the function
// as well as the return value.
//
// Internally this works by replacing the first instructions of the function
// with a jump to our own code. This means that the signature used to find
// the function address in the first place might not match anymore after a detour.
// If you need to detour the same function in different plugins make sure to 
// wildcard \x2a the first 6 bytes of the signature to accommodate for the patched
// jump introduced by the detour.
methodmap DynamicDetour < DHookSetup
{
	// Creates a detour.
	//
	// @param funcaddr      The address of the function to detour.
	//                      Can be Address_Null if you want to load the address from gamedata using DHookSetFromConf.
	// @param callConv      Calling convention of the function.
	// @param returnType    Type of the return value.
	// @param thisType      Type of this pointer or ignore (ignore can be used if not needed).
	//                      Only used for thiscall detours.
	//
	// @error               Failed to create detour setup handle.
	public native DynamicDetour(Address funcaddr, CallingConvention callConv, ReturnType returntype, ThisPointerType thisType=ThisPointer_Ignore);

	// Setup a detour for a function as described in a "Functions" section in gamedata.
	// The "Functions" section is parsed once the gamedata file is loaded and cached globally.
	//
	// @param gameconf      GameData handle to use for address lookup.
	//                      Doesn't have to be the same as the one with the "Functions" section.
	// @param name          Name of the function in a "Functions" section to load.
	//
	// @return              Setup handle for the detour or null if offset wasn't found.
	// @error               Failed to create detour setup handle, invalid gamedata handle,
	//                      invalid callback function or failed to find function in cached "Functions" sections.
	public static native DynamicDetour FromConf(Handle gameconf, const char[] name);

	// Enable the detour of the function described in this detour setup.
	// If you need to read the return value of the function, choose a post hook.
	//
	// @param mode          The desired hook mode - pre or post.
	//                      A pre hook calls your callback BEFORE the original function is called. 
	//                        You can access the parameters, set the return value, and skip the original function.
	//                      A post hook calls your callback AFTER the original function executed.
	//                        You can access the parameters and get/set the return value.
	// @param callback      Callback function.
	//
	// @return              True if detour was enabled, false otherwise.
	// @error               Hook handle is not setup for a detour.
	public native bool Enable(HookMode mode, DHookCallback callback);

	// Disable the detour of the function described in this detour setup.
	//
	// @param mode          The hook mode to disable - pre or post.
	// @param callback      Callback function.
	//
	// @return              True if detour was disabled, false otherwise.
	// @error               Hook handle is not setup for a detour or function is not detoured.
	public native bool Disable(HookMode mode, DHookCallback callback);
};

/* Adds an entity listener hook
 *
 * @param type			Type of listener to add
 * @param callback		Callback to use
 *
 * @noreturn
*/
native void DHookAddEntityListener(ListenType type, ListenCB callback);

/* Removes an entity listener hook
 *
 * @param type			Type of listener to remove
 * @param callback		Callback this listener was using
 *
 * @return True if one was removed false otherwise.
*/
native bool DHookRemoveEntityListener(ListenType type, ListenCB callback);

/* Creates a hook
 *
 * @param offset		vtable offset of function to hook
 * @param hooktype		Type of hook
 * @param returntype	Type of return value
 * @param thistype		Type of this pointer or ignore (ignore can be used if not needed)
 * @param callback		Optional callback function, if not set here must be set when hooking.
 * 
 * @return Returns setup handle for the hook.
 * @error Failed to create hook setup handle or invalid callback function.
*/
native DynamicHook DHookCreate(int offset, HookType hooktype, ReturnType returntype, ThisPointerType thistype, DHookCallback callback=INVALID_FUNCTION);

/**
 * Creates a detour
 *
 * @param funcaddr		The address of the function to detour.
 *						Can be Address_Null if you want to load the address from gamedata using DHookSetFromConf.
 * @param callConv		Calling convention of the function.
 * @param returnType	Type of the return value.
 * @param thisType		Type of this pointer or ignore (ignore can be used if not needed)
 *
 * @return				Setup handle for the detour.
 * @error				Failed to create detour setup handle.
 */	
native DynamicDetour DHookCreateDetour(Address funcaddr, CallingConvention callConv, ReturnType returntype, ThisPointerType thisType);

/**
 * Setup a detour or hook for a function as described in a "Functions" section in gamedata.
 *
 * @param gameconf		GameConfig handle
 * @param name			Name of the function in the gamedata to load.
 *
 * @return				Setup handle for the detour or INVALID_HANDLE if offset/signature/address wasn't found.
 * @error				Failed to create detour setup handle, invalid gamedata handle, invalid callback function or failed to find function in gamedata.
 */
native DHookSetup DHookCreateFromConf(Handle gameconf, const char[] name);

/**
 * Load details for a vhook or detour from a gamedata file.
 *
 * @param setup			Hook setup handle to set the offset or address on.
 * @param gameconf		GameConfig handle
 * @param source		Whether to look in Offsets or Signatures.
 * @param name			Name of the property to find.
 *
 * @return				True on success, false if nothing was found.
 * @error				Invalid setup or gamedata handle.
 */
native bool DHookSetFromConf(Handle setup, Handle gameconf, SDKFuncConfSource source, const char[] name);

/**
 * Enable the detour of the function described in the hook setup handle.
 *
 * @param setup			Hook setup handle
 * @param post			True to make the hook a post hook. (If you need to change the retunr value or need the return value use a post hook! If you need to change params and return use a pre and post hook!)
 * @param callback		Callback function
 *
 * @return				True if detour was enabled, false otherwise.
 * @error				Hook handle is not setup for a detour.
 */
native bool DHookEnableDetour(Handle setup, bool post, DHookCallback callback);

/**
 * Disable the detour of the function described in the hook setup handle.
 *
 * @param setup			Hook setup handle
 * @param post			True to disable a post hook.
 * @param callback		Callback function
 *
 * @return				True if detour was disabled, false otherwise.
 * @error				Hook handle is not setup for a detour or function is not detoured.
 */
native bool DHookDisableDetour(Handle setup, bool post, DHookCallback callback);

/* Adds param to a hook setup
 *
 * @param setup				Setup handle to add the param to.
 * @param type				Param type
 * @param size				Used for Objects (not Object ptr) to define the size of the object.
 * @param flag				Used to change the pass type.
 * @param custom_register	The register this argument is passed in instead of the stack.
 * 
 * @error	Invalid setup handle or too many params added (request upping the max in thread)
 * @noreturn
*/
native void DHookAddParam(Handle setup, HookParamType type, int size=-1, DHookPassFlag flag=DHookPass_ByVal, DHookRegister custom_register=DHookRegister_Default);

/* Hook entity
 * 
 * @param setup			Setup handle to use to add the hook.
 * @param post			True to make the hook a post hook. (If you need to change the return value or need the return value use a post hook! If you need to change params and return use a pre and post hook!)
 * @param entity		Entity index to hook on.
 * @param removalcb		Callback for when the hook is removed (Entity hooks are auto-removed on entity destroyed and will call this callback)
 * @param callback		Optional callback function, if not set here must be set when creating the hook.
 * 
 * @error Invalid setup handle, invalid address, invalid hook type or invalid callback.
 * @return INVALID_HOOK_ID on fail a hookid on success
*/
native int DHookEntity(Handle setup, bool post, int entity, DHookRemovalCB removalcb=INVALID_FUNCTION, DHookCallback callback=INVALID_FUNCTION);

/* Hook gamerules
 * 
 * @param setup			Setup handle to use to add the hook.
 * @param post			True to make the hook a post hook. (If you need to change the return value or need the return value use a post hook! If you need to change params and return use a pre and post hook!)
 * @param removalcb		Callback for when the hook is removed (Game rules hooks are auto-removed on map end and will call this callback)
 * @param callback		Optional callback function, if not set here must be set when creating the hook.
 * 
 * @error Invalid setup handle, invalid address, invalid hook type or invalid callback.
 * @return INVALID_HOOK_ID on fail a hookid on success
*/
native int DHookGamerules(Handle setup, bool post, DHookRemovalCB removalcb=INVALID_FUNCTION, DHookCallback callback=INVALID_FUNCTION);

/* Hook a raw pointer
 * 
 * @param setup			Setup handle to use to add the hook.
 * @param post			True to make the hook a post hook. (If you need to change the return value or need the return value use a post hook! If you need to change params and return use a pre and post hook!)
 * @param addr			This pointer address.
 * @param removalcb		Callback for when the hook is removed (Entity hooks are auto-removed on entity destroyed and will call this callback)
 * @param callback		Optional callback function, if not set here must be set when creating the hook.
 * 
 * @error Invalid setup handle, invalid address, invalid hook type or invalid callback.
 * @return INVALID_HOOK_ID on fail a hookid on success
*/
native int DHookRaw(Handle setup, bool post, Address addr, DHookRemovalCB removalcb=INVALID_FUNCTION, DHookCallback callback=INVALID_FUNCTION);

/* Remove hook by hook id
 * 
 * @param hookid		Hook id to remove
 * 
 * @return true on success false otherwise
 * @note This will not fire the removal callback!
*/
native bool DHookRemoveHookID(int hookid);

/* Get param value (Use only for: int, entity, bool or float param types)
 * 
 * @param hParams		Handle to params structure
 * @param num			Param number to get. (Example if the function has 2 params and you need the value of the first param num would be 1. 0 Will return the number of params stored)
 * 
 * @error Invalid handle. Invalid param number. Invalid param type.
 * @return value if num greater than 0. If 0 returns paramcount.
*/
native any DHookGetParam(Handle hParams, int num);

/* Get vector param value
 * 
 * @param hParams		Handle to params structure
 * @param num			Param number to get. (Example if the function has 2 params and you need the value of the first param num would be 1.)
 * @param vec			Vector buffer to store result.
 * 
 * @error Invalid handle. Invalid param number. Invalid param type.
 * @noreturn
*/
native void DHookGetParamVector(Handle hParams, int num, float vec[3]);

/* Get string param value
 * 
 * @param hParams		Handle to params structure
 * @param num			Param number to get. (Example if the function has 2 params and you need the value of the first param num would be 1.)
 * @param buffer		String buffer to store result
 * @param size			Buffer size
 * 
 * @error Invalid handle. Invalid param number. Invalid param type.
 * @noreturn
*/
native void DHookGetParamString(Handle hParams, int num, char[] buffer, int size);

/* Set param value (Use only for: int, entity, bool or float param types)
 * 
 * @param hParams		Handle to params structure
 * @param num			Param number to set (Example if the function has 2 params and you need to set the value of the first param num would be 1.)
 * @param value			Value to set it as (only pass int, bool, float or entity index)
 * 
 * @error Invalid handle. Invalid param number. Invalid param type.
 * @noreturn
*/
native void DHookSetParam(Handle hParams, int num, any value);

/* Set vector param value
 * 
 * @param hParams		Handle to params structure
 * @param num			Param number to set (Example if the function has 2 params and you need to set the value of the first param num would be 1.)
 * @param vec			Value to set vector as.
 * 
 * @error Invalid handle. Invalid param number. Invalid param type.
 * @noreturn
*/
native void DHookSetParamVector(Handle hParams, int num, float vec[3]);

/* Set string param value
 * 
 * @param hParams		Handle to params structure
 * @param num			Param number to set (Example if the function has 2 params and you need to set the value of the first param num would be 1.)
 * @param value			Value to set string as.
 * 
 * @error Invalid handle. Invalid param number. Invalid param type.
 * @noreturn
*/
native void DHookSetParamString(Handle hParams, int num, char[] value);

/* Get return value (Use only for: int, entity, bool or float return types)
 * 
 * @param hReturn		Handle to return structure
 * 
 * @error Invalid Handle, invalid type.
 * @return Returns default value if prehook returns actual value if post hook.
*/
native any DHookGetReturn(Handle hReturn);

/* Get return vector value
 * 
 * @param hReturn		Handle to return structure
 * @param vec			Vector buffer to store result in. (In pre hooks will be default value (0.0,0.0,0.0))
 * 
 * @error Invalid Handle, invalid type.
 * @noreturn
*/
native void DHookGetReturnVector(Handle hReturn, float vec[3]);

/* Get return string value
 * 
 * @param hReturn		Handle to return structure
 * @param buffer		String buffer to store result in. (In pre hooks will be default value "")
 * @param size			String buffer size
 * 
 * @error Invalid Handle, invalid type.
 * @noreturn
*/
native void DHookGetReturnString(Handle hReturn, char[] buffer, int size);

/* Set return value (Use only for: int, entity, bool or float return types)
 * 
 * @param hReturn		Handle to return structure
 * @param value			Value to set return as
 * 
 * @error Invalid Handle, invalid type.
 * @noreturn
*/
native void DHookSetReturn(Handle hReturn, any value);

/* Set return vector value
 * 
 * @param hReturn		Handle to return structure
 * @param vec			Value to set return vector as
 * 
 * @error Invalid Handle, invalid type.
 * @noreturn
*/
native void DHookSetReturnVector(Handle hReturn, float vec[3]);

/* Set return string value
 * 
 * @param hReturn		Handle to return structure
 * @param value			Value to set return string as
 * 
 * @error Invalid Handle, invalid type.
 * @noreturn
*/
native void DHookSetReturnString(Handle hReturn, char[] value);

//WE SHOULD WRAP THESE AROUND STOCKS FOR NON PTR AS WE SUPPORT BOTH WITH THESE NATIVE'S

/* Gets an objects variable value
 *
 * @param hParams		Handle to params structure
 * @param num			Param number to get.
 * @param offset		Offset within the object to the var to get.
 * @param type			Type of var it is
 *
 * @error Invalid handle. Invalid param number. Invalid param type. Invalid Object type.
 * @return Value of the objects var. If EHANDLE type or entity returns entity index.
*/
native any DHookGetParamObjectPtrVar(Handle hParams, int num, int offset, ObjectValueType type);

/* Sets an objects variable value
 *
 * @param hParams		Handle to params structure
 * @param num			Param number to set.
 * @param offset		Offset within the object to the var to set.
 * @param type			Type of var it is
 * @param value			The value to set the var to.
 *
 * @error Invalid handle. Invalid param number. Invalid param type. Invalid Object type.
 * @noreturn
*/
native void DHookSetParamObjectPtrVar(Handle hParams, int num, int offset, ObjectValueType type, any value);

/* Gets an objects vector variable value
 *
 * @param hParams		Handle to params structure
 * @param num			Param number to get.
 * @param offset		Offset within the object to the var to get.
 * @param type			Type of var it is
 * @param buffer		Buffer to store the result vector
 *
 * @error Invalid handle. Invalid param number. Invalid param type. Invalid Object type.
 * @noreturn
*/
native void DHookGetParamObjectPtrVarVector(Handle hParams, int num, int offset, ObjectValueType type, float buffer[3]);

/* Sets an objects vector variable value
 *
 * @param hParams		Handle to params structure
 * @param num			Param number to set.
 * @param offset		Offset within the object to the var to set.
 * @param type			Type of var it is
 * @param value			The value to set the vector var to.
 *
 * @error Invalid handle. Invalid param number. Invalid param type. Invalid Object type.
 * @noreturn
*/
native void DHookSetParamObjectPtrVarVector(Handle hParams, int num, int offset, ObjectValueType type, float value[3]);

/* Gets an objects string variable value
 *
 * @param hParams		Handle to params structure
 * @param num			Param number to get.
 * @param offset		Offset within the object to the var to get.
 * @param type			Type of var it is
 * @param buffer		Buffer to store the result vector
 * @param size			Size of the buffer
 *
 * @error Invalid handle. Invalid param number. Invalid param type. Invalid Object type.
 * @noreturn
*/
native void DHookGetParamObjectPtrString(Handle hParams, int num, int offset, ObjectValueType type, char[] buffer, int size);

/* Checks if a pointer param is null
 *
 * @param hParams		Handle to params structure
 * @param num			Param number to check.
 *
 * @error Non pointer param
 * @return True if null false otherwise.
*/
native bool DHookIsNullParam(Handle hParams, int num);

/* Get param address (Use only for ptr param types)
 * 
 * @param hParams		Handle to params structure
 * @param num			Param number to get. (Example if the function has 2 params and you need the value of the first param num would be 1.)
 * 
 * @error Invalid handle. Invalid param number. Invalid param type.
 * @return address of the parameter.
*/
native Address DHookGetParamAddress(Handle hParams, int num);

public Extension __ext_dhooks =
{
	name = "dhooks",
	file = "dhooks.ext",
#if defined AUTOLOAD_EXTENSIONS
	autoload = 1,
#else
	autoload = 0,
#endif
#if defined REQUIRE_EXTENSIONS
	required = 1,
#else
	required = 0,
#endif
};

#if !defined REQUIRE_EXTENSIONS
public __ext_dhooks_SetNTVOptional()
{
	MarkNativeAsOptional("DHookAddEntityListener");
	MarkNativeAsOptional("DHookRemoveEntityListener");
	MarkNativeAsOptional("DHookCreate");
	MarkNativeAsOptional("DHookCreateDetour");
	MarkNativeAsOptional("DHookCreateFromConf");
	MarkNativeAsOptional("DHookSetFromConf");
	MarkNativeAsOptional("DHookEnableDetour");
	MarkNativeAsOptional("DHookDisableDetour");
	MarkNativeAsOptional("DHookAddParam");
	MarkNativeAsOptional("DHookEntity");
	MarkNativeAsOptional("DHookGamerules");
	MarkNativeAsOptional("DHookRaw");
	MarkNativeAsOptional("DHookRemoveHookID");
	MarkNativeAsOptional("DHookGetParam");
	MarkNativeAsOptional("DHookGetParamVector");
	MarkNativeAsOptional("DHookGetParamString");
	MarkNativeAsOptional("DHookSetParam");
	MarkNativeAsOptional("DHookSetParamVector");
	MarkNativeAsOptional("DHookSetParamString");
	MarkNativeAsOptional("DHookGetReturn");
	MarkNativeAsOptional("DHookGetReturnVector");
	MarkNativeAsOptional("DHookGetReturnString");
	MarkNativeAsOptional("DHookSetReturn");
	MarkNativeAsOptional("DHookSetReturnVector");
	MarkNativeAsOptional("DHookSetReturnString");
	MarkNativeAsOptional("DHookGetParamObjectPtrVar");
	MarkNativeAsOptional("DHookSetParamObjectPtrVar");
	MarkNativeAsOptional("DHookGetParamObjectPtrVarVector");
	MarkNativeAsOptional("DHookSetParamObjectPtrVarVector");
	MarkNativeAsOptional("DHookIsNullParam");
	MarkNativeAsOptional("DHookGetParamObjectPtrString");
	MarkNativeAsOptional("DHookGetParamAddress");

	MarkNativeAsOptional("DHookParam.IsNull");
	MarkNativeAsOptional("DHookParam.Get");
	MarkNativeAsOptional("DHookParam.GetVector");
	MarkNativeAsOptional("DHookParam.GetString");
	MarkNativeAsOptional("DHookParam.Set");
	MarkNativeAsOptional("DHookParam.SetVector");
	MarkNativeAsOptional("DHookParam.SetString");
	MarkNativeAsOptional("DHookParam.GetObjectVar");
	MarkNativeAsOptional("DHookParam.GetObjectVarVector");
	MarkNativeAsOptional("DHookParam.GetObjectVarString");
	MarkNativeAsOptional("DHookParam.SetObjectVar");
	MarkNativeAsOptional("DHookParam.SetObjectVarVector");
	MarkNativeAsOptional("DHookParam.GetAddress");
	MarkNativeAsOptional("DHookReturn.Value.get");
	MarkNativeAsOptional("DHookReturn.Value.set");
	MarkNativeAsOptional("DHookReturn.GetVector");
	MarkNativeAsOptional("DHookReturn.GetString");
	MarkNativeAsOptional("DHookReturn.SetVector");
	MarkNativeAsOptional("DHookReturn.SetString");
	MarkNativeAsOptional("DHookSetup.SetFromConf");
	MarkNativeAsOptional("DHookSetup.AddParam");
	MarkNativeAsOptional("DynamicHook.DynamicHook");
	MarkNativeAsOptional("DynamicHook.FromConf");
	MarkNativeAsOptional("DynamicHook.HookEntity");
	MarkNativeAsOptional("DynamicHook.HookGamerules");
	MarkNativeAsOptional("DynamicHook.HookRaw");
	MarkNativeAsOptional("DynamicHook.RemoveHook");
	MarkNativeAsOptional("DynamicDetour.DynamicDetour");
	MarkNativeAsOptional("DynamicDetour.FromConf");
	MarkNativeAsOptional("DynamicDetour.Enable");
	MarkNativeAsOptional("DynamicDetour.Disable");
}
#endif
