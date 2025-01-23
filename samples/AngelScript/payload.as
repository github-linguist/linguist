// Sample script.
// Source: https://github.com/codecat/ssbd-payload

array<WorldScript::PayloadBeginTrigger@> g_payloadBeginTriggers;
array<WorldScript::PayloadTeamForcefield@> g_teamForceFields;

[GameMode]
class Payload : TeamVersusGameMode
{
	[Editable]
	UnitFeed PayloadUnit;

	[Editable]
	UnitFeed FirstNode;

	[Editable default=10]
	int PrepareTime;

	[Editable default=300]
	int TimeLimit;

	[Editable default=90]
	int TimeAddCheckpoint;

	[Editable default=2]
	float TimeOvertime;

	[Editable default=1000]
	int TimePayloadHeal;

	[Editable default=1]
	int PayloadHeal;

	PayloadBehavior@ m_payload;

	int m_tmStarting;
	int m_tmStarted;
	int m_tmLimitCustom;
	int m_tmOvertime;
	int m_tmInOvertime;

	PayloadHUD@ m_payloadHUD;
	PayloadClassSwitchWindow@ m_switchClass;

	array<SValue@>@ m_switchedSidesData;

	Payload(Scene@ scene)
	{
		super(scene);

		m_tmRespawnCountdown = 5000;

		@m_payloadHUD = PayloadHUD(m_guiBuilder);
		@m_switchTeam = PayloadTeamSwitchWindow(m_guiBuilder);
		@m_switchClass = PayloadClassSwitchWindow(m_guiBuilder);
	}

	void UpdateFrame(int ms, GameInput& gameInput, MenuInput& menuInput) override
	{
		TeamVersusGameMode::UpdateFrame(ms, gameInput, menuInput);

		m_payloadHUD.Update(ms);

		if (Network::IsServer())
		{
			uint64 tmNow = CurrPlaytimeLevel();

			if (m_tmStarting == 0)
			{
				if (GetPlayersInTeam(0) > 0 && GetPlayersInTeam(1) > 0)
				{
					m_tmStarting = tmNow;
					(Network::Message("GameStarting") << m_tmStarting).SendToAll();
				}
			}

			if (m_tmStarting > 0 && m_tmStarted == 0 && tmNow - m_tmStarting > PrepareTime * 1000)
			{
				m_tmStarted = tmNow;
				(Network::Message("GameStarted") << m_tmStarted).SendToAll();

				for (uint i = 0; i < g_payloadBeginTriggers.length(); i++)
				{
					WorldScript@ ws = WorldScript::GetWorldScript(g_scene, g_payloadBeginTriggers[i]);
					ws.Execute();
				}
			}
		}

		if (!m_ended && m_tmStarted > 0)
			CheckTimeReached(ms);
	}

	string NameForTeam(int index) override
	{
		if (index == 0)
			return "Defenders";
		else if (index == 1)
			return "Attackers";

		return "Unknown";
	}

	void CheckTimeReached(int dt)
	{
		// Check if time limit is not reached yet
		if (m_tmLimitCustom - (CurrPlaytimeLevel() - m_tmStarted) > 0)
		{
			// Don't need to continue checking
			m_tmOvertime = 0;
			m_tmInOvertime = 0;
			return;
		}

		// Count how long we're in overtime for later time limit fixing when we reach a checkpoint
		if (m_tmOvertime > 0)
			m_tmInOvertime += dt;

		// Check if there are any attackers still inside
		if (m_payload.AttackersInside() > 0)
		{
			// We have overtime
			m_tmOvertime = int(TimeOvertime * 1000);
			return;
		}

		// If we have overtime
		if (m_tmOvertime > 0)
		{
			// Decrease timer
			m_tmOvertime -= dt;
			if (m_tmOvertime <= 0)
			{
				// Overtime countdown reached, time limit reached
				TimeReached();
			}
		}
		else
		{
			// No overtime, so time limit is reached
			TimeReached();
		}
	}

	void TimeReached()
	{
		if (!Network::IsServer())
			return;

		(Network::Message("TimeReached")).SendToAll();
		SetWinner(false);
	}

	bool ShouldFreezeControls() override
	{
		return m_switchClass.m_visible
		    || TeamVersusGameMode::ShouldFreezeControls();
	}

	bool ShouldDisplayCursor() override
	{
		return m_switchClass.m_visible
		    || TeamVersusGameMode::ShouldDisplayCursor();
	}

	bool CanSwitchTeams() override
	{
		return m_tmStarted == 0;
	}

	PlayerRecord@ CreatePlayerRecord() override
	{
		return PayloadPlayerRecord();
	}

	int GetPlayerClassCount(PlayerClass playerClass, TeamVersusScore@ team)
	{
		if (team is null)
			return 0;

		int ret = 0;
		for (uint i = 0; i < team.m_players.length(); i++)
		{
			if (team.m_players[i].peer == 255)
				continue;
			auto record = cast<PayloadPlayerRecord>(team.m_players[i]);
			if (record.playerClass == playerClass)
				ret++;
		}
		return ret;
	}

	void PlayerClassesUpdated()
	{
		m_switchClass.PlayerClassesUpdated();
	}

	void SetWinner(bool attackers)
	{
		if (attackers)
			print("Attackers win!");
		else
			print("Defenders win!");

		m_payloadHUD.Winner(attackers);
		EndMatch();
	}

	void DisplayPlayerName(int idt, SpriteBatch& sb, PlayerRecord@ record, PlayerHusk@ plr, vec2 pos) override
	{
		TeamVersusGameMode::DisplayPlayerName(idt, sb, record, plr, pos);

		m_payloadHUD.DisplayPlayerName(idt, sb, cast<PayloadPlayerRecord>(record), plr, pos);
	}

	void RenderFrame(int idt, SpriteBatch& sb) override
	{
		Player@ player = GetLocalPlayer();
		if (player !is null)
		{
			PlayerHealgun@ healgun = cast<PlayerHealgun>(player.m_currWeapon);
			if (healgun !is null)
				healgun.RenderMarkers(idt, sb);
		}

		TeamVersusGameMode::RenderFrame(idt, sb);
	}

	void RenderWidgets(PlayerRecord@ player, int idt, SpriteBatch& sb) override
	{
		m_payloadHUD.Draw(sb, idt);

		TeamVersusGameMode::RenderWidgets(player, idt, sb);

		m_switchClass.Draw(sb, idt);
	}

	void GoNextMap() override
	{
		if (m_switchedSidesData !is null)
		{
			TeamVersusGameMode::GoNextMap();
			return;
		}

		ChangeLevel(GetCurrentLevelFilename());
	}

	void SpawnPlayers() override
	{
		if (m_switchedSidesData is null)
		{
			TeamVersusGameMode::SpawnPlayers();
			return;
		}

		if (Network::IsServer())
		{
			for (uint i = 0; i < m_switchedSidesData.length(); i += 2)
			{
				uint peer = uint(m_switchedSidesData[i].GetInteger());
				uint team = uint(m_switchedSidesData[i + 1].GetInteger());

				TeamVersusScore@ joinScore = FindTeamScore(team);
				if (joinScore is m_teamScores[0])
					@joinScore = m_teamScores[1];
				else
					@joinScore = m_teamScores[0];

				for (uint j = 0; j < g_players.length(); j++)
				{
					if (g_players[j].peer != peer)
						continue;
					SpawnPlayer(j, vec2(), 0, joinScore.m_team);
					break;
				}
			}
		}
	}

	void Save(SValueBuilder& builder) override
	{
		if (m_switchedSidesData is null)
		{
			builder.PushArray("teams");
			for (uint i = 0; i < g_players.length(); i++)
			{
				if (g_players[i].peer == 255)
					continue;
				builder.PushInteger(g_players[i].peer);
				builder.PushInteger(g_players[i].team);
			}
			builder.PopArray();
		}

		TeamVersusGameMode::Save(builder);
	}

	void Start(uint8 peer, SValue@ save, StartMode sMode) override
	{
		if (save !is null)
			@m_switchedSidesData = GetParamArray(UnitPtr(), save, "teams", false);

		TeamVersusGameMode::Start(peer, save, sMode);

		m_tmLimit = 0; // infinite time limit as far as VersusGameMode is concerned
		m_tmLimitCustom = TimeLimit * 1000; // 5 minutes by default

		@m_payload = cast<PayloadBehavior>(PayloadUnit.FetchFirst().GetScriptBehavior());

		if (m_payload is null)
			PrintError("PayloadUnit is not a PayloadBehavior!");

		UnitPtr unitFirstNode = FirstNode.FetchFirst();
		if (unitFirstNode.IsValid())
		{
			auto node = cast<WorldScript::PayloadNode>(unitFirstNode.GetScriptBehavior());
			if (node !is null)
				@m_payload.m_targetNode = node;
			else
				PrintError("First target node is not a PayloadNode script!");
		}
		else
			PrintError("First target node was not set!");

		WorldScript::PayloadNode@ prevNode;

		float totalDistance = 0.0f;

		UnitPtr unitNode = unitFirstNode;
		while (unitNode.IsValid())
		{
			auto node = cast<WorldScript::PayloadNode>(unitNode.GetScriptBehavior());
			if (node is null)
				break;

			unitNode = node.NextNode.FetchFirst();

			@node.m_prevNode = prevNode;
			@node.m_nextNode = cast<WorldScript::PayloadNode>(unitNode.GetScriptBehavior());

			if (prevNode !is null)
				totalDistance += dist(prevNode.Position, node.Position);

			@prevNode = node;
		}

		float currDistance = 0.0f;

		auto distNode = cast<WorldScript::PayloadNode>(unitFirstNode.GetScriptBehavior());
		while (distNode !is null)
		{
			if (distNode.m_prevNode is null)
				distNode.m_locationFactor = 0.0f;
			else
			{
				currDistance += dist(distNode.m_prevNode.Position, distNode.Position);
				distNode.m_locationFactor = currDistance / totalDistance;
			}

			@distNode = distNode.m_nextNode;
		}

		m_payloadHUD.AddCheckpoints();
	}

	void SpawnPlayer(int i, vec2 pos = vec2(), int unitId = 0, uint team = 0) override
	{
		TeamVersusGameMode::SpawnPlayer(i, pos, unitId, team);

		PayloadPlayerRecord@ record = cast<PayloadPlayerRecord>(g_players[i]);
		record.HandlePlayerClass();

		if (g_players[i].local)
		{
			//TODO: This doesn't work well
			bool localAttackers = (team == HashString("player_1"));
			for (uint j = 0; j < g_teamForceFields.length(); j++)
			{
				bool hasCollision = (localAttackers != g_teamForceFields[j].Attackers);

				auto units = g_teamForceFields[j].Units.FetchAll();
				for (uint k = 0; k < units.length(); k++)
				{
					PhysicsBody@ body = units[k].GetPhysicsBody();
					if (body is null)
					{
						PrintError("PhysicsBody for unit " + units[k].GetDebugName() + "is null");
						continue;
					}
					body.SetActive(hasCollision);
				}
			}
		}
	}
}
