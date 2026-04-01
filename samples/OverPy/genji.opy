#!mainFile "../framework.opy"

# Source: https://github.com/tylovejoy/genji-framework/blob/master/FwHero/Genji.opy

################################################################################
/* Rename Global Variables */
#!define BanAutoCreate CM
################################################################################
/* Include Shared Skill Macros*/
#!include "all.opy"
#!include "climbers.opy"
################################################################################

macro Fw_Parkour_Setup_Hero():
    UltCheckpoints = [i for i in UltCheckpoints if i + false >= 0] if len(UltCheckpoints) else []
    Ability1Checkpoints = [i for i in Ability1Checkpoints if i + false >= 0] if len(Ability1Checkpoints) else []
    # clean out -1's after the ban has loaded
    #BanTriple = [i for i in BanTriple if i + false >= 0] if len(BanTriple) else [] # legacy code, now auto sets it to null to save space
    BanCreate = [i for i in BanCreate if i + false >= 0] if len(BanCreate) else []
    BanAutoCreate = BanAutoCreate if len(BanAutoCreate) else []
    BanStand = [i for i in BanStand if i + false >= 0] if len(BanStand) else []
    BanMulti = [i for i in BanMulti if i + false >= 0] if len(BanMulti) else []
    BanDead = [i for i in BanDead if i + false >= 0] if len(BanDead) else []
    BanEmote = [i for i in BanEmote if i + false >= 0] if len(BanEmote) else []
    BanSaveDouble = BanSaveDouble if len(BanSaveDouble) else []
    BanClimb = [i for i in BanClimb if i + false >= 0] if len(BanClimb) else []
    BanBhop = [i for i in BanBhop if i + false >= 0] if len(BanBhop) else []
    BanDjump = BanDjump if len(BanDjump) else []

macro Fw_Parkour_UpdateCache_Hero():
    if eventPlayer.checkpoint_notLast:
        eventPlayer.cache_startUlt = eventPlayer.checkpoint_current in UltCheckpoints
        if eventPlayer.cache_startUlt:
            smallMessage(eventPlayer, f"   {abilityIconString(Hero.GENJI, Button.ULTIMATE)} {t'Ultimate Is Ready'}")
        eventPlayer.cache_startAbility1 = eventPlayer.checkpoint_current in Ability1Checkpoints
        if eventPlayer.cache_startAbility1:
            smallMessage(eventPlayer, f"   {abilityIconString(Hero.GENJI, Button.ABILITY_1)} {t'Dash Is Ready'}")
        eventPlayer.ban_create = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_CreateBhop, false, 1) or eventPlayer.checkpoint_current in BanCreate
        if eventPlayer.ban_create:
            eventPlayer.banString = f"♂ {eventPlayer.banString}"
        eventPlayer.ban_autocreate = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_AutoCreate, false, 2) or eventPlayer.checkpoint_current in BanAutoCreate
        if eventPlayer.ban_autocreate:
            eventPlayer.banString = f"∀ {eventPlayer.banString}"
        eventPlayer.ban_standcreate = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_StandCreate, false, 3) or eventPlayer.checkpoint_current in BanStand
        if eventPlayer.ban_standcreate:
            eventPlayer.banString = f"♠ {eventPlayer.banString}" # ≥  √ ▼ ↓
        eventPlayer.ban_multi = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_MultiClimb, false, 4) or eventPlayer.checkpoint_current in BanMulti
        if eventPlayer.ban_multi:
            eventPlayer.banString = f"∞ {eventPlayer.banString}"
        eventPlayer.ban_dead = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_DeathHop, false, 5) or eventPlayer.checkpoint_current in BanDead
        if eventPlayer.ban_dead:
            eventPlayer.banString = f"X {eventPlayer.banString}"
        eventPlayer.ban_emote = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_Emote, false, 6) or eventPlayer.checkpoint_current in BanEmote
        if eventPlayer.ban_emote:
            eventPlayer.banString = f"♥ {eventPlayer.banString}"
        eventPlayer.ban_savedouble = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_SaveDouble, false, 7) or eventPlayer.checkpoint_current in BanSaveDouble
        if eventPlayer.ban_savedouble:
            eventPlayer.banString = f"△ {eventPlayer.banString}"
        eventPlayer.ban_climb = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_WallClimb, false, 8) or eventPlayer.checkpoint_current in BanClimb
        if eventPlayer.ban_climb:
            eventPlayer.banString = f"↑ {eventPlayer.banString}"
        eventPlayer.ban_bhop = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_RequireBhop, false, 9) or eventPlayer.checkpoint_current in BanBhop
        if eventPlayer.ban_bhop:
            eventPlayer.banString = f"≥ {eventPlayer.banString}" # ≥  √ ▼ ↓
        eventPlayer.ban_djump = createWorkshopSettingBool(SettingsText.BanCategory, SettingsText.Ban_RequireDjump, false, 10) or eventPlayer.checkpoint_current in BanDjump
        if eventPlayer.ban_djump:
            eventPlayer.banString = f"» {eventPlayer.banString}" # ≥  √ ▼ ↓ ︽
    else:
        eventPlayer.cache_startUlt = true
        eventPlayer.cache_startAbility1 = true
        eventPlayer.ban_create = false
        eventPlayer.ban_autocreate = false
        eventPlayer.ban_standcreate = false
        eventPlayer.ban_multi = false
        eventPlayer.ban_dead = false
        eventPlayer.ban_emote = false
        eventPlayer.ban_savedouble = false
        eventPlayer.ban_climb = false
        eventPlayer.ban_bhop = false
        eventPlayer.ban_djump = false

################################################################################

macro Fw_Mechanics_GroundReset_Hero():
    eventPlayer.skill_usedDouble = null

macro Fw_Mechanics_Hero():
    Fw_Mechanics_All(Fw_Mechanics_GroundReset_Climbers(Fw_Mechanics_GroundReset_Hero()))
    Fw_Mechanics_Climber()

    def ResetAbilities():
        @Name "Mechanic | Genji | SUB Reset Abilities"
        if eventPlayer.toggle_invincible or (eventPlayer == hostPlayer and EditorOn):
            eventPlayer.setUltEnabled(true)
            eventPlayer.setAbility1Enabled(true)
        else:
            eventPlayer.setUltEnabled(eventPlayer.cache_startUlt)
            eventPlayer.setAbility1Enabled(eventPlayer.cache_startAbility1)
        eventPlayer.setUltCharge(100)

    rule "Mechanic | Genji | Dash":
        @Event eachPlayer
        @Hero genji
        @Condition eventPlayer.isUsingAbility1()
        if eventPlayer.toggle_invincible or (EditorOn and eventPlayer == hostPlayer) or not eventPlayer.checkpoint_notLast: return
        eventPlayer.setAbility1Enabled(false)
        waitUntil(not eventPlayer.isUsingAbility1(), true)
        if distance(eventPlayer, CheckpointPositions[eventPlayer.checkpoint_current].last()) <= cpCircleRadius:
            ResetAbilities()

    rule "Mechanic | Genji | Ultimate":
        @Event eachPlayer
        @Hero genji
        # Total Duration: 218 ticks, 3.488s
        # Casting: 76 ticks, 1.216s
        # Blade: 125 ticks, 2s
        # Recovery: 17 ticks, 0.272s
        # Swing Duration: 38 ticks, 0.608s
        @Condition eventPlayer.isUsingUltimate() 
        if eventPlayer.toggle_invincible or (EditorOn and eventPlayer == hostPlayer) or not eventPlayer.checkpoint_notLast:
            eventPlayer.setUltEnabled(true)
            eventPlayer.setUltCharge(100)
            wait(3.488, Wait.ABORT_WHEN_FALSE)
            if ruleCondition: loop()
        else:
            waitUntil(not eventPlayer.isUsingUltimate(), (3 * 38) * 0.016)
            eventPlayer.disallowButton(Button.PRIMARY_FIRE) # Disallow primary fire because of slash exploit
            waitUntil(not eventPlayer.isUsingUltimate(), (218 - (3 * 38)) * 0.016)
            eventPlayer.allowButton(Button.PRIMARY_FIRE)
            if distance(eventPlayer, CheckpointPositions[eventPlayer.checkpoint_current].last()) <= cpCircleRadius:
                ResetAbilities()

    rule "Mechanic | Genji | Double Jump":
        @Event eachPlayer
        @Hero genji
        @Condition eventPlayer.isAlive()
        @Condition eventPlayer.isInAir()
        @Condition eventPlayer.ban_djump or eventPlayer.ban_savedouble or eventPlayer.addon_enableDoubleChecks
        #Save drop
        waitUntil(eventPlayer.isOnGround() or eventPlayer.isJumping() or eventPlayer.isHoldingButton(Button.JUMP), 0.096)
        if not RULE_CONDITION: return
        while true:
            #Released Jump
            waitUntil(eventPlayer.isOnGround() or not eventPlayer.isHoldingButton(Button.JUMP), Math.INFINITY)
            if not RULE_CONDITION: return
            #Double Jumped
            waitUntil(eventPlayer.isOnGround() or eventPlayer.isHoldingButton(Button.JUMP), Math.INFINITY)
            if not RULE_CONDITION: return
            eventPlayer.skill_usedDouble = true
            #Reset
            waitUntil(eventPlayer.isOnGround() or not eventPlayer.skill_usedDouble, Math.INFINITY)
            if not RULE_CONDITION: return

    rule "Mechanic | Genji | Ban Save Double - 封禁二段跳":
        @Event eachPlayer
        @Hero genji
        @Condition eventPlayer.ban_savedouble
        @Condition eventPlayer.toggle_invincible == false
        @Condition eventPlayer.isInAir()
        @Condition eventPlayer.skill_usedDouble == false
        waitUntil(eventPlayer.getThrottle().z > null or not eventPlayer.isInAir() or eventPlayer.skill_usedDouble, Math.INFINITY)
        if not RULE_CONDITION: return
        waitUntil(eventPlayer.getThrottle().z <= null or not eventPlayer.isInAir() or eventPlayer.skill_usedDouble, Math.INFINITY)
        if not RULE_CONDITION: return
        #Prevent false positives
        #Default climb speed is 7.8 and small slowdown upon mantling
        if eventPlayer.getVerticalSpeed() < 6: goto RULE_START
        if eventPlayer.skill_usedBhop:
            wait(0.8, Wait.ABORT_WHEN_FALSE)
        else:
            wait(0.8, Wait.ABORT_WHEN_FALSE)
            #This can give a false negative, but advantage is small
            if eventPlayer.skill_usedBhop: return
        #"   延二段跳已禁用!" checkCN "   save double banned!"
        smallMessage(eventPlayer, t"   Save Double » Is Banned!")
        CheckpointFailReset()
