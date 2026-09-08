#if defined _slot_helpers_included
    #endinput
#endif
#define _slot_helpers_included

#pragma rational Float
#define NEXT_SLOT(%1) (%1 + 1)

public Action:OnSlotUpdated(client)
{
    return Plugin_Continue;
}

stock Float:GetSlotProgress(completed, total)
{
    new Float:progress = float(completed) / float(total);
    return progress;
}
