#if defined _legacy_bridge_included
    #endinput
#endif
#define _legacy_bridge_included

native CallLocalFunction(const String:name[], const String:format[], any:...);

public SharedPlugin:__pl_legacy_bridge =
{
    name = "legacy_bridge",
    file = "legacy_bridge.smx",
    required = 1
};

stock bool:NotifyLocalClient(client)
{
    CallLocalFunction("OnClientReady", "i", client);
    return true;
}
