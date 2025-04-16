//::///////////////////////////////////////////////
//:: Filename e3pc_spidersnest (OnEnter Encounter Script)
//:: e3pc_spidersnest.nss (PC disturbs Spider's Nest)
//:: Initially scripted with: Lilac Soul's NWN Script Generator v1.6
//:: Created on 26/3/2004 6:14:33 PM, by John Gardner
//::///////////////////////////////////////////////
/*When the player disturbs the spider's nest, a brood of three spiders
will drop to the floor, accompanied by a non-magical Web effect (as the
spiders drop their tangled webbing to immobolise their quarry). Directly
before the encounter begins, a brief text message will appear, describing
the PC noticing faint movement overhead.

Note: As the Web-like effect is non-magical, recipients are not permitted
Spell resistance checks (although states such as "Freedom of Movement" and
Reflex saves apply normally)*/
void main()
{object oPC = GetEnteringObject();
int nOneShot = GetLocalInt(OBJECT_SELF, "q9x_spidernest_active");
if (GetLocalInt(OBJECT_SELF, "q9x_spidernest_active") == 1 && GetIsPC(oPC) == TRUE)
{object oActionSubject = GetObjectByTag("q9s_spiderweb_trig");
effect eAOEWeb = EffectAreaOfEffect(AOE_PER_WEB);
effect eSpiderweb = ExtraordinaryEffect(eAOEWeb);
location lLocation = GetLocation(GetObjectByTag("q9a_spiderweb_trigger"));
SendMessageToPC(oPC, "You briefly notice a vague movement writhing amidst the shadows overhead... before a brood of spiders drop from the rafters!");
ApplyEffectAtLocation(DURATION_TYPE_TEMPORARY, eSpiderweb, lLocation, 24.0);
SetLocalInt(OBJECT_SELF, "q9x_spidernest_active", 0);}}
