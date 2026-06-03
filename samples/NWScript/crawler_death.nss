//::///////////////////////////////////////////////
//:: Filename crawler_death (OnDeath Creature Script)
//:: crawler_death.nss (Apply "Freeze Animation" VFX; add custom tokens)
//:: Created On: 5/02/2004 4:38:37 PM, by John Gardner
//:: Scripted With: Script Editor (Written Manually)
//:://////////////////////////////////////////////
/*Because the carrion crawler model included in Chaeolwyn's HAK lacked
a proper death animation, this script was designed to rectify that
problem. The carrion crawler would continue to spasm inexorably,
until its corpse was searched and the blood taken. As it seems, I lack
any sort of modelling/animation skill, so actually *editing* the MDL file
was futile for me, let alone fixing the animation bug.

UPDATE 5/07/2004 7:36:23 PM- Added a function to set a variable upon
Quallo for purpose of conversation, so the line "Move aside so I can kill
this crawler, etc..." only shows up if the player *hasn't* killed the
carrion crawler.

UPDATE 3/09/2004 4:24:11 PM- After having come a milestone in my understanding of
gmax, I've *just* learned enough to know how to configure animations; so the crawler's
dying/dead animations have been fixed to be more appropriate. That means the "Frozen
animation" effect is no longer needed (Don't get me wrong, though. I STILL suck bad
at modelling... I'm wondering myself how I managed it successfully ^-~;)*/
void main()
{effect eVFXAnimFreeze = EffectVisualEffect(VFX_DUR_FREEZE_ANIMATION);
effect eVFXGreenSlime = EffectVisualEffect(VFX_COM_BLOOD_LRG_GREEN, FALSE);
ApplyEffectToObject(DURATION_TYPE_INSTANT, eVFXGreenSlime, OBJECT_SELF);
AssignCommand(OBJECT_SELF, PlaySound("c_spider_dead"));
object oQuallo = GetObjectByTag("x7_quallo");
SetLocalInt(oQuallo, "HasPCKilledCrawler", 1);
SetCustomToken(398, "knew");
SetCustomToken(399, "(he glances remorsefully at the carrion crawler's corpse spawled on the floor).");}
