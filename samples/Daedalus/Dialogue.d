// ************************************************************
// 			  				   EXIT 
// ************************************************************

INSTANCE DIA_Dervin_EXIT(C_INFO)
{
	npc			= BAU_2200_Dervin;
	nr			= 999;
	condition	= DIA_Dervin_EXIT_Condition;
	information	= DIA_Dervin_EXIT_Info;
	permanent	= TRUE;
	description = DIALOG_ENDE;
};                       

FUNC INT DIA_Dervin_EXIT_Condition()
{
	return TRUE;
};

FUNC VOID DIA_Dervin_EXIT_Info()
{
	AI_StopProcessInfos	(self);
};

// ************************************************************
// 			  				Hello
// ************************************************************

INSTANCE DIA_Dervin_Hello (C_INFO)
{
	npc			= BAU_2200_Dervin;
	nr			= 1;
	condition	= DIA_Dervin_Hello_Condition;
	information	= DIA_Dervin_Hello_Info;
	permanent	= FALSE;
	important 	= TRUE;
};                       

FUNC INT DIA_Dervin_Hello_Condition()
{
	return TRUE;
};

func void DIA_Dervin_Hello_Info()
{
	AI_Output (self, other,"DIA_Dervin_Hello_13_00");
	AI_Output (other, self,"DIA_Dervin_Hello_13_01");
	AI_Output (self, other,"DIA_Dervin_Hello_13_02");
	AI_Output (self, other,"DIA_Dervin_Hello_13_03");
	AI_Output (other, self,"DIA_Dervin_Hello_13_04");
	AI_Output (self, other,"DIA_Dervin_Hello_13_05");
	AI_Output (self, other,"DIA_Dervin_Hello_13_06");
	
	Npc_ExchangeRoutine	(BAU_2201_Malvina, "Talk");
	
	AI_StopProcessInfos	(self);
};
