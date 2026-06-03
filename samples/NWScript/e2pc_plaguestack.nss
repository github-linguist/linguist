//::///////////////////////////////////////////////
//:: Filename e2pc_plaguestack (OnEnter Encounter Script)
//:: e2pc_plaguestack.nss (PC encounters the stacked piles of bodies)
//:: Scripted with: Script Editor (Written Manually)
//:: Created On: 6/07/2004 2:40:13 PM, by John Gardner
//::///////////////////////////////////////////////
/*[OnEnter Script]: Upon entering the encounter area, send a floating text
message to the entering character describing what they see, and set a local
variable specifying that the PC's seen the stacked, plague-withered bodies
(used in Animal Empathy dialogue with rats/bats).*/
void main()
{object oPC = GetEnteringObject();
if (GetLocalInt(oPC, "HasPCSeenPlaguebodies") != 1)
{FloatingTextStringOnCreature("Piles of withered corpses lay sprawled about, each having atrophied from some mysterious epidemic", oPC, FALSE);
SetLocalInt(oPC, "HasPCSeenPlaguebodies", 1);}}
