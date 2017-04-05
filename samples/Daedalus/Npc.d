instance BAU_2200_Dervin (Npc_Default)
{
	name 		= "Dervin";
	guild 		= GIL_BAU;
	id 			= 2200;
	voice 		= 14;
	flags       = 0;																					
	npctype		= NPCTYPE_MAIN;
	
	B_SetAttributesToChapter (self, 2);																	

	fight_tactic		= FAI_HUMAN_COWARD;
	
	B_CreateAmbientInv 	(self);
	CreateInvItems		(self, ItMi_Hammer, 1);
	CreateInvItems		(self, ItMi_Saw, 1);
	CreateInvItems		(self, ItFo_Cheese, 1);
	CreateInvItems		(self, ItFo_Water, 1);
	CreateInvItems		(self, ItFo_Bread, 1);
																		
	B_SetNpcVisual 		(self, MALE, "Hum_Head_Bald", Face_N_ImportantGrey, BodyTex_N, ITAR_Bau_L);		
	Mdl_SetModelFatness	(self, 1);
	Mdl_ApplyOverlayMds	(self, "Humans_Relaxed.mds");

	B_GiveNpcTalents (self);
																			
	B_SetFightSkills (self, 20);
	
	daily_routine = Rtn_PreStart_2200;
};

FUNC VOID Rtn_PreStart_2200 ()
{
	TA_Stand_ArmsCrossed	(07,00,01,00,"WS_FARM_03");
	TA_Sleep				(01,00,07,00,"WS_HOUSE1_SLEEP2");
};

FUNC VOID Rtn_Start_2200 ()
{	
	TA_Sleep		(01,00,07,00,"WS_HOUSE1_SLEEP2");
    TA_Rake_FP		(07,00,10,00,"WS_FARM_05");
	TA_Sit_Chair	(10,00,12,00,"WS_HOUSE1_03");
	TA_Saw			(12,00,18,00,"WS_FARM_DERVIN_SAW_03");
	TA_Sit_Throne	(18,00,19,00,"WS_HOUSE1_THRONE");
	TA_Sit_Bench	(19,00,01,00,"WS_FARM_04");
};
