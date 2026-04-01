#!include "settings.opy"

# Source: https://github.com/Zezombye/overpy/blob/master/examples/meipocalypse/meipocalypse.opy

#!include "debug_settings.opy"

##!obfuscate noRuleFilling noConstantObfuscation

enum SortOrderTop:
    CENTERING_HUD = -100,
    FFYL_HUD,
    REZ_HUD,
    OBJ_DESC_HIDING_HUD,

enum GameStatus:
    NOT_STARTED,
    IN_WAVE,
    BETWEEN_WAVES,

globalvar gameStatus
globalvar rotation
globalvar currentZone
globalvar playerSpawns
globalvar generateNewZone
globalvar currentMap
globalvar zoneZWalls
globalvar mapXLimits
globalvar zWallDrawIndex
globalvar mapYLimits
globalvar meiSpawns
globalvar spawnInterval = 5
globalvar nbMaxMeis = 12
globalvar nbMeisFighting
globalvar cryoChance = -10*5
globalvar zonesUnlockLocation
globalvar zonesUnlockMoney
globalvar score
globalvar currentWave
globalvar nbRemainingMeis

globalvar i

playervar isOob
playervar meiType
playervar icon
playervar effect
playervar noCrouchSpam
playervar isFighting
playervar blades
playervar randomVar
playervar isAvailable
playervar isDeflecting
playervar buyLoopIndex
subroutine resetPlayer
subroutine resetPlayerAbilities



#!define generateWalls(map, walls) __script__("generateWalls.js")
#!define generateZoneVariables(zoneData) __script__("generateZoneVariables.js")
#!define getFirstAvailableMei() [player for player in getPlayers(Team.2) if player.isAvailable][0]

#!include "shop.opy"
#!include "barricades.opy"
#!include "heroUnlock.opy"
#!include "mei_types.opy"
#!include "zones.opy"
#!include "debug.opy"
#!include "waves.opy"
#!include "fightforyourlife.opy"

rule "init":

    createInWorldText(getAllPlayers(), "Unlock next zone: ${}".format(zonesUnlockMoney[currentZone]), zonesUnlockLocation[currentZone]+vect(0,1,0), 2, Clip.SURFACES, WorldTextReeval.VISIBILITY_POSITION_AND_STRING, Color.AQUA, SpecVisibility.ALWAYS)
    #createInWorldText(getAllPlayers(), "(Ctrl+F)".format(score), zonesUnlockLocation[currentZone]-vect(0,0.5,0), 2, Clip.SURFACES, WorldTextReeval.VISIBILITY_POSITION_AND_STRING, Color.AQUA, SpecVisibility.ALWAYS)

    hudText(getAllPlayers(), "{} meis remaining".format(nbRemainingMeis), "Wave {}".format(currentWave), "{} meis fighting".format(nbMeisFighting), HudPosition.RIGHT, 0, Color.BLUE, Color.AQUA, Color.AQUA, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudHeader(getAllPlayers(), "Score: {}".format(score), HudPosition.LEFT, 0, Color.GREEN, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudSubtext(getAllPlayers(), " \n\n\n\n\n\n\n\n\n\n", HudPosition.TOP, SortOrderTop.CENTERING_HUD, Color.GREEN, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudSubtext(getAllPlayers(), " \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n", HudPosition.TOP, SortOrderTop.OBJ_DESC_HIDING_HUD, Color.GREEN, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)

    pauseMatchTime()
    disableGamemodeCompletion()
    disableInspector()

    if "a" == p"a":
        hudHeader(getAllPlayers(), "Warning: no obfuscation", HudPosition.RIGHT, 0, Color.RED, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)


rule "force meis":
    @Event eachPlayer
    @Team 2
    @Condition eventPlayer.getCurrentHero() != Hero.MEI
    do:
        eventPlayer.startForcingHero(Hero.MEI)
        wait()
    while RULE_CONDITION


rule "init humans":
	@Event eachPlayer
    @Team 1
    @Condition gameStatus != GameStatus.NOT_STARTED
    eventPlayer.setRespawnTime(99999)
    hudHeader(eventPlayer, "Money: ${}".format(eventPlayer.money), HudPosition.LEFT, 0, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.DEFAULT)
    hudSubtext(eventPlayer, "Damage: {}%".format(eventPlayer.dmgDealt), HudPosition.LEFT, 10, Color.WHITE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.DEFAULT)
    hudSubtext(eventPlayer, "Healing: {}%".format(eventPlayer.healingDealt), HudPosition.LEFT, 10, Color.WHITE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.DEFAULT)
    hudSubtext(eventPlayer, "Regen: {} hp/s".format(eventPlayer.regen), HudPosition.LEFT, 10, Color.WHITE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.DEFAULT)
    hudSubtext(eventPlayer, "Projectile speed: {}%".format(eventPlayer.projectileSpeed), HudPosition.LEFT, 10, Color.WHITE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.DEFAULT)
    chase(eventPlayer.secondWindRezTimer, 999999, rate=1, ChaseReeval.NONE)
    #chase(eventPlayer.shieldHealth, 0, rate=eventPlayer.getMaxHealth()/5, ChaseReeval.DESTINATION_AND_RATE)


rule "start game":
    /*score = 0
    nbRemainingMeis = 0
    #playersMoney = [99999,99999,99999,99999,99999,99999]
    playersMoney = [0,0,0,0,0,0]
    totalMoneyEarned = 0

    getPlayers(Team.1).resetHeroAvailability()
    getPlayers(Team.1).setInvisibility(Invis.NONE)
    getPlayers(Team.1).setPrimaryFireEnabled(true)
    getPlayers(Team.1).setSecondaryFireEnabled(true)
    getPlayers(Team.1).setAbility1Enabled(true)
    getPlayers(Team.1).setAbility2Enabled(true)
    getPlayers(Team.1).setUltEnabled(true)
    getPlayers(Team.1).stopAllHoT()
    getPlayers(Team.1).setDamageDealt(100)
    getPlayers(Team.1).setDamageReceived(100)
    getPlayers(Team.1).setProjectileSpeed(100)
    getPlayers(Team.1).clearStatusEffect(Status.UNKILLABLE)
    getPlayers(Team.1).clearStatusEffect(Status.PHASED_OUT)
    getPlayers(Team.1).nbRez = 0
    getPlayers(Team.1).secondWindRezTimer = 999999
    getPlayers(Team.1).respawn()
    getPlayers(Team.1).setRespawnTime(5)
    getPlayers(Team.1).shieldHealth = 0*/

    wait(5)
    gameStatus = GameStatus.IN_WAVE

rule "game started":
	@Condition gameStatus != GameStatus.NOT_STARTED
    bigMessage(getAllPlayers(), "Game started")

rule "new game (init players)":
	@Event eachPlayer
	@Team 1
	@Condition gameStatus != GameStatus.NOT_STARTED
    if not DEBUG_ALL_HEROES:
        eventPlayer.setAllowedHeroes(unlockedHeroes)
    #on join, get the money of the player with the least money * 0.9
    eventPlayer.money = max(0, round(sorted([p for p in getPlayers(Team.1) if p.money > 10], lambda p: p.money)[0].money*0.9))


def resetPlayerAbilities():
    eventPlayer.setMoveSpeed(100)
    eventPlayer.setAbility1Enabled(eventPlayer.unlockedAbility1)
    eventPlayer.setAbility2Enabled(eventPlayer.unlockedAbility2)
    eventPlayer.setUltEnabled(eventPlayer.unlockedUltimate)
    eventPlayer.setPrimaryFireEnabled(eventPlayer.unlockedPrimaryFire)
    eventPlayer.setSecondaryFireEnabled(eventPlayer.unlockedSecondaryFire)

def resetPlayer():
    resetPlayerAbilities()
    eventPlayer.stopAllHoT()
    eventPlayer.setDamageDealt(eventPlayer.dmgDealt)
    eventPlayer.setHealingDealt(eventPlayer.healingDealt)
    eventPlayer.setProjectileSpeed(eventPlayer.projectileSpeed)
    eventPlayer.setInvisibility(Invis.NONE)

/*
rule "reset weapon if not unlocked":
    @Event eachPlayer
    @Team 1
    @Condition not eventPlayer.unlockedWeapon[getAllHeroes().index(eventPlayer.getCurrentHero())]
    @Condition eventPlayer.getCurrentWeapon() != 1.1
    do:
        eventPlayer.setWeapon(1.1)
        wait()
    while RULE_CONDITION
*/

rule "tp player to current zone spawn":
	@Event eachPlayer
	@Team 1
	@Condition gameStatus == GameStatus.IN_WAVE
	@Condition eventPlayer.hasSpawned() 
	@Condition eventPlayer.isAlive()
    wait(0.1)
    eventPlayer.teleport(playerSpawns[currentZone])
    eventPlayer.setFacing(Vector.BACKWARD, Relativity.TO_WORLD)
    #eventPlayer.startForcingHero(eventPlayer.getCurrentHero())
    resetPlayer()
    eventPlayer.setStatusEffect(null, Status.UNKILLABLE, 9999)
    eventPlayer.shieldHealth = 0
    wait(0.1)
    eventPlayer.startHoT(null, 9999, eventPlayer.regen)


rule "human died":
    @Event eachPlayer
    @Team 1
    @Condition eventPlayer.isDead()
    eventPlayer.teleport(nearestWalkablePosition(eventPlayer.getPosition()))


rule "mei first spawn":
	@Event eachPlayer
	@Team 2
	@Condition eventPlayer.hasSpawned() and gameStatus != GameStatus.NOT_STARTED
    eventPlayer.setRespawnTime(999999999)
    wait(0.1)
    kill(eventPlayer, null)


rule "reset mei type on death":
	@Event playerDied
	@Team 2
	@Condition gameStatus == GameStatus.IN_WAVE
    if victim.isFighting:
        nbRemainingMeis--

        getPlayers(Team.1).money += {
            Mei.GENERIC: 10,
            Mei.MELEE: 10,
            Mei.TANK: 50,
            Mei.HEALER: 50,
            Mei.SPEEDY: 15,
            Mei.SNIPER: 20,
            Mei.ULT: 20,
            Mei.DEFLECT: 25,
            Mei.JUMPING: 30,
            Mei.HACKING: 15,
        }[victim.meiType]
        score += {
            Mei.GENERIC: 10,
            Mei.MELEE: 10,
            Mei.TANK: 50,
            Mei.HEALER: 50,
            Mei.SPEEDY: 15,
            Mei.SNIPER: 20,
            Mei.ULT: 20,
            Mei.DEFLECT: 25,
            Mei.JUMPING: 30,
            Mei.HACKING: 15,
        }[victim.meiType]

    wait(1)
    victim.meiType = Mei.NO_TYPE
    if victim.isFighting:
        nbMeisFighting--
    victim.isFighting = false
    wait()
    victim.isAvailable = true


rule "mei fights":
	@Event eachPlayer
	@Team 2
	@Condition eventPlayer.isFighting
    eventPlayer.clearStatusEffect(Status.ROOTED)
    eventPlayer.isAvailable = false
    nbMeisFighting++
    eventPlayer.teleport(random.choice(meiSpawns[currentZone]))
    wait()
    eventPlayer.resurrect()

rule "spawn meis":
	@Event global
    do:
        if nbMeisFighting < nbMaxMeis and nbMeisFighting < nbRemainingMeis:
            getFirstAvailableMei().isFighting = true
        if nbMeisFighting > 11:
            wait(spawnInterval)
        wait(1)
    while true


rule "no death plane for meis":
	@Event eachPlayer
	@Team 2
    @Condition eventPlayer.isFighting
    @Condition eventPlayer.getPosition().y < mapYLimits[currentMap*2]
    eventPlayer.applyImpulse(Vector.UP, 20, Relativity.TO_WORLD, Impulse.CANCEL_CONTRARY_MOTION)
    damage(eventPlayer, null, 0.1*eventPlayer.getMaxHealth())


rule "anti crash?":
	@Event global
	@Condition getServerLoad() > 240
    wait(1, Wait.ABORT_WHEN_FALSE)
    smallMessage(getAllPlayers(), "Anti-crash system activated")
    setSlowMotion(30)
    wait(0.5)
    setSlowMotion(100)

