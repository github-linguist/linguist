//::///////////////////////////////////////////////
//:: Filename illusion_spawn (OnSpawn Creature Script)
//:: illusion_spawn.nss (Default spawn script for Phantasmal Image)
//:: Initially Scripted With: Lilac Soul's NWN Script Generator v1.6
//:: Created On: 4/02/2004 09:15:19 AM, by John Gardner
//:://///////////////////////////////////////////
/*[OnSpawn Script] When spawning in during the cutscene, the Phantasmal
Image will appear "kneeling down", so that it appears as if Lilarcor's
rising slowly from the sewer's waters. After the PC takes the blade,
have the Phantasm removed from the module.*/
void main()
{//Spawn in using a CutsceneInvisibility effect, removed when Lilarcor reaches the pre-defined waypoint
ApplyEffectToObject(DURATION_TYPE_PERMANENT, EffectVisualEffect(VFX_DUR_CUTSCENE_INVISIBILITY), OBJECT_SELF);
AssignCommand(OBJECT_SELF, ActionPlayAnimation(ANIMATION_LOOPING_SIT_CROSS, 1.0, 2.0));
DelayCommand(3.8, DestroyObject(OBJECT_SELF));}

