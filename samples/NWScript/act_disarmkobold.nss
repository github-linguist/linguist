//::///////////////////////////////////////////////
//:: Filename act_disarmkobold (OnDisarm Trigger script)
//:: act_disarmkobold.nss (PC disarms the kobold's tangle trap)
//:: Scripted With: Script Editor (Written Manually)
//:: Created On: 7/07/2004 5:55:24 PM, by John Gardner
//:://////////////////////////////////////////////
/*[OnDisarm] Any character seen disarming the trap rigged on the tribe's chest
arouse the kobold's anger, who attack the supposed thief breaking into their
belongings. Award a 5 XP gain for disarming a very easy trap, and adjust
reputation by -100%

UPDATE July 13th- Kobolds won't attack player if Shaman's been charmed or
subject to a "Scare" spell during dialogue.
Update 29/07/2004 5:46:41 PM- Fixed an unusual loophole where kobolds attack PC
if trap's disarmed at a distance (via "Find Traps"). Kobolds will only attack the
player if they see him/her within a certain distance of their belongings (as they
won't realize the figure in the distance was the one who caused their trap to
be disarmed).*/
#include "nw_o2_coninclude"
#include "nw_i0_generic"
void main()
{object oShaman = GetObjectByTag("q9k_shamancrragtail");
 object oTrappedChest = GetObjectByTag("q9a_strongboxdirty");
 if (GetDistanceBetween(oTrappedChest, GetLastDisarmed()) < 10.0)
 {GiveXPToCreature(GetLastDisarmed(), 5);
 //If Shaman's charmed, he won't be angered by having the PC disarm his trapped chest
 if (GetLocalInt(oShaman, "Charmed") == 1 || GetLocalInt(oShaman, "Scared") != 1
 && GetLocalInt(oShaman, "CrragtailSpokenCharmedLine") != 1)
 {string szCharmLine = "Yip! You take Crragtail clan's belongings... You be trustworthy friend, yip!";
 AssignCommand(oShaman, SpeakString(szCharmLine, TALKVOLUME_TALK));
 SetLocalInt(oShaman, "CrragtailSpokenCharmedLine", 1);}
 //Similarly, if he's been terrified into submission, he's not going to dare attack the PC
 else if (GetLocalInt(oShaman, "Charmed") != 1 || GetLocalInt(oShaman, "Scared") == 1
 && GetLocalInt(oShaman, "CrragtailSpokenScaredLine") != 1)
 {string szScareLine = "*whimper* P-please, take shinies, j-just not hurts poor Crragtail... *snivvel*";
 AssignCommand(oShaman, SpeakString(szScareLine, TALKVOLUME_TALK));
 SetLocalInt(oShaman, "CrragtailSpokenScaredLine", 1);}
 //Otherwise, the kobolds aren't going to take kindly to having somebody dismantle their trapwork...
 else if (GetLocalInt(oShaman, "Charmed") != 1 && GetLocalInt(oShaman, "Scared") != 1)
 {ShoutDisturbed();
 AdjustReputation(GetLastDisarmed(), oTrappedChest, -100);
 object oTribeFaction = GetObjectByTag("q9k_tribehealer");
 object oKobold = GetFirstFactionMember(oTribeFaction, FALSE);
 object oPC = GetNearestCreature(CREATURE_TYPE_PLAYER_CHAR, PLAYER_CHAR_IS_PC, oKobold, 1);
 while (GetIsObjectValid(oKobold) == TRUE)
 {AssignCommand(oKobold, ActionAttack(oPC, FALSE));
 oKobold = GetNextFactionMember(oKobold, FALSE);}}
}}
