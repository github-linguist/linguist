//::///////////////////////////////////////////////
//:: Filename act_unlockkobold (OnUnlocked Placable Script)
//:: act_unlockkobold.nss (PC unlocks the Kobold's treasure chest)
//:: Scripted With: Script Editor (Written Manually)
//:: Created On: 7/07/2004 5:58:52 PM, by John Gardner
//:://////////////////////////////////////////////
/*[OnUnlock] Should the tribe notice anybody who tries unlocking their treasure
chest, they'll promptly attack in rage. Award a minor XP gain, and set a
local variable on this chest that ensures this script only awards XP once.

UPDATE 2/09/2004 5:19:11 PM- Added functions that determine if the last person to
unlock the chest was a player-character or not. While this sounds like a pretty useless
addition, a player's "Knock" spell isn't considered a player-controlled character's action
for the purposes of an OnUnlock script... and the kobolds aren't open-minded enough
to realize the faint clicking sounds from their strongbox were the player's magic secretely
unlocking it. They *will* however attack anybody who tries pilfering their belongings.*/
#include "nw_o2_coninclude"
#include "nw_i0_generic"
void main()
{//Declare major variables
 object oShaman = GetObjectByTag("q9k_shamancrragtail");
 object oPC = GetLastUnlocked();
 object oChest = OBJECT_SELF;
 //Only award XP if whoever unlocked chest was a PC. This prevents giving XP from casting Knock spells
 if (GetIsPC(oPC) == TRUE && GetLocalInt(OBJECT_SELF, "HasBeenUnlocked") == 0)
 {GiveXPToCreature(oPC, 10);
  SetLocalInt(OBJECT_SELF, "HasBeenUnlocked", 1);}

 if (GetIsPC(oPC) == TRUE && (GetLocalInt(oShaman, "Charmed") != 1 || GetLocalInt(oShaman, "Scared") != 1))
 {ShoutDisturbed();
 AdjustReputation(GetLastUnlocked(), OBJECT_SELF, -100);
 object oTribeFaction = GetObjectByTag("q9k_tribehealer");
 object oKobold = GetFirstFactionMember(oTribeFaction, FALSE);
 object oPC = GetNearestCreature(CREATURE_TYPE_PLAYER_CHAR, PLAYER_CHAR_IS_PC, oKobold, 1);
 AssignCommand(oShaman, ClearAllActions(FALSE)); //Kick Crragtail off his throne before fighting
 while (GetIsObjectValid(oKobold) == TRUE)
 {AssignCommand(oKobold, ActionAttack(oPC, FALSE));
 oKobold = GetNextFactionMember(oKobold, FALSE);}
 AssignCommand(oShaman, RespondToShout(oChest, CLEAR_X0_INC_HENAI_RespondToShout1, oPC));
 }
}
