// ********************************************************************************
// B_AssessEnemy
// -------------
// Wenn NSC einen aNSC (oder Monster) sieht(!), zu dem er GILDEN(!)-Att_Hostile hat
// Hier wird davon ausgegangen,dass other auf JEDEN FALL ein Enemy ist!
// NSC geht in ZS_Attack (dort wird auf evtl. Flucht gecheckt)
// Reichweite: PERC_DIST_ACTIVE_MAX
// AssessEnemy wird NACH AssessPlayer gecheckt
// ********************************************************************************

func int B_AssessEnemy()
{
	// EXIT IF...
		
	// ------- TESTMODE: Levelinspektor wird ignoriert ------
	var C_NPC PCL; PCL = Hlp_GetNpc(PC_Levelinspektor);
	if (Hlp_GetInstanceID(other) == Hlp_GetInstanceID(PCL))
	{
		return FALSE;
	};

	//ADDON>
	// ------- NoFightParker NPC kloppt sich nur mit Monstern oder dem Player, wird auch nie angegriffen ------
	if ((Hlp_GetInstanceID(other) != Hlp_GetInstanceID(hero))
		&& (other.guild < GIL_SEPERATOR_HUM)
		&& (	(self.aivar[AIV_NoFightParker] == TRUE)
			|| 	(other.aivar[AIV_NoFightParker] == TRUE)))
		|| ((other.guild > GIL_SEPERATOR_HUM) && (other.aivar[AIV_NoFightParker] == TRUE))//Monster mit AIV_NoFightParker = TRUE werden auch ignoriert
	{
		return FALSE;
	};	
	//ADDON<
	
	// ------ Gegner im Wasser ------
	if (C_BodyStateContains(other,BS_SWIM) || C_BodyStateContains(other,BS_DIVE))
	&& (self.aivar[AIV_MM_FollowInWater] == FALSE)
	{
		return FALSE;
	};
	
	// ------- Burgzinnenwachen sollen sich nicht zu Tode stürzen, etc. ------
	if (Npc_GetHeightToNpc(self, other) > PERC_DIST_HEIGHT)
	{
		return FALSE;
	};
	
	// ------- für Party-NSCs -------
	if (self.aivar[AIV_PARTYMEMBER] == TRUE)
	{
		if (Npc_GetDistToNpc (self, other) > 1500) //Sollen nicht so weit weg angreifen
		{
			return FALSE;
		};
		
		if (!Npc_CanSeeNpc (self, other))
		{	
			return FALSE;
		};
	};
	
	// ------- Verkleidete Banditen -----------
	if (C_PlayerisFakeBandit(self,other))
	&& (self.guild == GIL_BDT)
	{
		return FALSE;
	};
	
	// ------- Mag-Golem-Lares-Hack ------------
	var C_NPC MGO; MGO = Hlp_GetNpc(MagicGolem);
	var C_NPC LAR; LAR = Hlp_GetNpc(VLK_449_Lares);
	
	if  (Hlp_GetInstanceID(self) == Hlp_GetInstanceID(LAR))
	&&  (Hlp_GetInstanceID(other) == Hlp_GetInstanceID(MGO))
	{
		return FALSE;
	};
	
	// ------- OVERRIDE für Enemies, die noch vorher quatschen sollen ------
	if ( (self.aivar[AIV_EnemyOverride] == TRUE) || (other.aivar[AIV_EnemyOverride] == TRUE) )
	&& (other.guild < GIL_SEPERATOR_HUM)
	{
		return FALSE;
	};
	
	// ------- Sicherheitsabfrage (B_AssessEnemy wird auch von anderen Funktionen aufgerufen, nicht nur über Wahrnehmung) ------
	if (Npc_GetAttitude (self, other) != ATT_HOSTILE)
	{
		return FALSE;
	};
	
	if (C_NpcIsGateGuard(self))
	{
		return FALSE;
	};
	
	// ------ Npctype_Friend ignoriert den Spieler-Enemy ------
	if ( Npc_IsPlayer(other) && (self.npctype == NPCTYPE_FRIEND) ) 
	{
		return FALSE;
	};
	
	// ------ ich bin nicht GILDEN-Hostile zu other ------
	if (Wld_GetGuildAttitude(self.guild, other.guild) != ATT_HOSTILE)
	{
		// ------ ABER ich bin temp_hostile (d.h. hab gerade gegen den Spieler gekämpft) ------
		if (Npc_GetAttitude (self, other) == ATT_HOSTILE)
		// -------------------------
		// && Npc_IsPlayer(other) - auskommentiert, weil ich NUR zum Player Temp_Att haben kann ------
		// ------ NICHT direkt nach ZS-Attack-Abbruch reagieren ------
		&& ( (Npc_GetStateTime(self) > 2) || Npc_IsInState(self, ZS_ObservePlayer) )
		// ------ Kampf nur fortsetzen, wenn Spieler näher als 10m ------
		&& (Npc_GetDistToNpc(self,other) <= PERC_DIST_INTERMEDIAT)
		{
			B_Attack (self, other, self.aivar[AIV_LastPlayerAR], 0);
			return TRUE;
		}
		else
		{
			return FALSE;
		};
	};

	
	// FUNC
		
	B_Attack (self, other, AR_GuildEnemy, 0);
	return TRUE;
};


	
