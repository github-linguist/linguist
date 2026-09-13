# Source: https://github.com/Zezombye/overpy/blob/master/examples/santa.opy
# Shortened for linguist

/*
Gamemode I made for Elo Hell's Jingle Jam.
Naturally, Provoxin won
*/

settings {
    "lobby": {
        "ffaSlots": 6
    },
    "gamemodes": {
        "ffa": {
            "enabledMaps": [
                "kingsRowWinter"
            ]
        },
        "general": {
            "enableHeroSwitching": false,
            "heroLimit": "off",
            "enableRandomHeroes": true,
            "respawnTime%": 30
        }
    },
    "heroes": {
        "allTeams": {
            "mei": {
                "enablePrimaryFire": false,
                "enableSecondaryFire": false,
                "enableAbility1": false,
                "health%": 266.4,
                "enableAbility2": false,
                "passiveUltGen%": 0,
                "combatUltGen%": 0,
            },
            "enabledHeroes": [
                "mei"
            ]
        }
    }
}

globalvar i

globalvar rectangleChimneys = [vect(56.92, 24.863, -22.05), vect(55.639, 24.863, -21.332)]

globalvar rectangleChimneysAB = []
globalvar rectangleChimneysAD = []
globalvar rectangleChimneysABDot = []
globalvar rectangleChimneysADDot = []
globalvar rectangleChimneysSpheres = [vect(55.628, 25.742, -22.821), vect(49.844, 25.742, -37.986)]

globalvar sphereChimneys = [vect(-17.768, 76.344, -45.534), vect(-50.419, 28.464, -13.289)]

globalvar factoryChimneys = [vect(-103.743, 52.898, -7.883)]

globalvar chimneys = rectangleChimneysSpheres.concat(sphereChimneys).concat(factoryChimneys)

globalvar gameStarted = false

globalvar finisher = null

playervar pos = vect(-17.40, 39.19, -91.84)
#!define SPEED 15

playervar i
playervar gottenChimneys
playervar reindeer = null
playervar score
playervar nbChimneysGotten = 0

rule "init AB AD":
    for i in range(len(rectangleChimneys)):
        rectangleChimneysAB.append((vectorTowards(rectangleChimneys[i*4]*vect(1,0,1), rectangleChimneys[i*4+1]*vect(1,0,1))))
        rectangleChimneysAD.append((vectorTowards(rectangleChimneys[i*4]*vect(1,0,1), rectangleChimneys[i*4+3]*vect(1,0,1))))
        
        rectangleChimneysABDot.append(dotProduct(vectorTowards(rectangleChimneys[i*4]*vect(1,0,1), rectangleChimneys[i*4+1]*vect(1,0,1)), vectorTowards(rectangleChimneys[i*4]*vect(1,0,1), rectangleChimneys[i*4+1]*vect(1,0,1))))
        rectangleChimneysADDot.append(dotProduct(vectorTowards(rectangleChimneys[i*4]*vect(1,0,1), rectangleChimneys[i*4+3]*vect(1,0,1)), vectorTowards(rectangleChimneys[i*4]*vect(1,0,1), rectangleChimneys[i*4+3]*vect(1,0,1))))


rule "game instructions":
    @Condition isInSetup()
    setMatchTime(999)
    hudText(getAllPlayers(), " \n\n\n\n\n\n\n\n                                           \n\n\n\n\n\n\n\n\n\n\n\n\n", "                          ", null, HudPosition.RIGHT, 2, Color.TURQUOISE, Color.WHITE, Color.WHITE, HudReeval.VISIBILITY, SpecVisibility.DEFAULT)
    wait(5)
    hudSubtext(getAllPlayers(), " \n", HudPosition.TOP, 9, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudHeader(getAllPlayers(), "turn with mouse, {} to go up, {} to go down".format(buttonString(Button.JUMP), buttonString(Button.CROUCH)), HudPosition.TOP, 10, Color.YELLOW, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    wait(8)
    setMatchTime(5)

#!define sleighBeam(start, end) createBeam(getAllPlayers(), Beam.GRAPPLE, updateEveryTick(worldVector(start, eventPlayer, Transform.ROTATION_AND_TRANSLATION)), updateEveryTick(worldVector(end, eventPlayer, Transform.ROTATION_AND_TRANSLATION)), null, EffectReeval.VISIBILITY_POSITION_AND_RADIUS)

rule "init player":
    @Event eachPlayer
    @Condition not eventPlayer.isDummy()
    @Condition eventPlayer.hasSpawned()
    eventPlayer.gottenChimneys[len(chimneys)-1] = 0
    eventPlayer.disallowButton(Button.ULTIMATE)
    wait(0.1)
    eventPlayer.startFacing(vect(-0.18, 0, 0.98), 999, Relativity.TO_WORLD, FacingReeval.DIRECTION_AND_TURN_RATE)
    eventPlayer.startForcingPosition(eventPlayer.pos, true)
    eventPlayer.setStatusEffect(null, Status.PHASED_OUT, 9999)
    eventPlayer.disableKillFeed()

    #left
    sleighBeam(vect(1, 0, -2), vect(1, 0, 3))
    #right
    sleighBeam(vect(-1, 0, -2), vect(-1, 0, 3))
    #front
    sleighBeam(vect(1, 0, 3), vect(-1, 0, 3))
    #back
    sleighBeam(vect(1, 0, -2), vect(-1, 0, -2))

    #beam to orisa
    createBeam(getAllPlayers(), Beam.BAD, updateEveryTick(worldVector(vect(0, 1.5, 2), eventPlayer, Transform.ROTATION_AND_TRANSLATION)), updateEveryTick(worldVector(vect(0, 1.5, 6), eventPlayer, Transform.ROTATION_AND_TRANSLATION)), Color.RED, EffectReeval.VISIBILITY_POSITION_AND_RADIUS)

    #orisa
    createDummy(Hero.ORISA, Team.ALL, eventPlayer.getSlot()+6, null, null)
    eventPlayer.reindeer = getLastCreatedEntity()

    eventPlayer.reindeer.startForcingPosition((worldVector(vect(0, -0.5, 5.5), eventPlayer, Transform.ROTATION_AND_TRANSLATION)), true)
    eventPlayer.reindeer.attachTo(eventPlayer, vect(0, -0.5, 5.5))
    eventPlayer.reindeer.startFacing(eventPlayer.getFacingDirection()*vect(1,0,1), 999, Relativity.TO_WORLD, FacingReeval.DIRECTION_AND_TURN_RATE)
    eventPlayer.reindeer.setStatusEffect(null, Status.PHASED_OUT, 9999)
    eventPlayer.reindeer.disableEnvironmentCollision(true)


#!define createChimneyEffect(idx) createEffect([p for p in getAllPlayers() if p.gottenChimneys[idx]], Effect.SPHERE, Color.AQUA, chimneys[idx], 1.5 if (idx) < len(rectangleChimneysSpheres)+len(sphereChimneys) else 3, EffectReeval.VISIBILITY)

rule "init chimney effects":
    createChimneyEffect(0)
    createChimneyEffect(1)

subroutine gotChimney
def gotChimney():
    eventPlayer.gottenChimneys[eventPlayer.i] = true
    playEffect(eventPlayer, DynamicEffect.GOOD_PICKUP_EFFECT, Color.BLUE, chimneys[eventPlayer.i], 99)
    if eventPlayer.i == 5:
        eventPlayer.addToScore(3)
        smallMessage(eventPlayer, "{0} Secret chimney! +3 points {0}".format(abilityIconString(Hero.MEI, Button.ULTIMATE)))
    else:
        eventPlayer.addToScore(1)
        eventPlayer.nbChimneysGotten++
        smallMessage(eventPlayer, "{0} {1} {0}".format(abilityIconString(random.choice(getAllHeroes().exclude(Hero.BASTION)), random.choice([Button.ABILITY_1, Button.ABILITY_2, Button.ULTIMATE])), random.choice([
            "{} Woah! thank you mei, you're the best!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} what an a-mei-zing christmas!".format(heroIcon(random.choice(getAllHeroes()))),
        ])))
