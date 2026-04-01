# Source: https://github.com/Zezombye/overpy/blob/master/examples/santa.opy

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

globalvar rectangleChimneys = [vect(56.92, 24.863, -22.05), vect(55.639, 24.863, -21.332), vect(54.394, 24.863, -23.549), vect(55.675, 24.863, -24.274), vect(49.909, 24.863, -39.421), vect(48.644, 24.863, -38.668), vect(49.949, 24.863, -36.48), vect(51.213, 24.863, -37.232), vect(37.326, 27.757, -25.265), vect(38.763, 27.757, -25.573), vect(38.237, 27.757, -28.063), vect(36.8, 27.757, -27.759), vect(11.209, 27.553, -23.272), vect(11.427, 27.553, -21.816), vect(13.946, 27.553, -22.191), vect(13.725, 27.553, -23.647), vect(14.175, 27.553, 7.772), vect(14.397, 27.553, 6.316), vect(11.877, 27.553, 5.935), vect(11.656, 27.553, 7.389)]

globalvar rectangleChimneysAB = []
globalvar rectangleChimneysAD = []
globalvar rectangleChimneysABDot = []
globalvar rectangleChimneysADDot = []
globalvar rectangleChimneysSpheres = [vect(55.628, 25.742, -22.821), vect(49.844, 25.742, -37.986), vect(37.774, 28.637, -26.733), vect(12.577, 28.431, -22.653), vect(13.038, 28.433, 6.908)]

globalvar sphereChimneys = [vect(-17.768, 76.344, -45.534), vect(-50.419, 28.464, -13.289), vect(-57.093, 19.03, -19.035), vect(-66.214, 20.165, -3.68), vect(-59.483, 24.523, 2.352), vect(-76.523, 21.031, -4.917), vect(-78.695, 17.573, -1.593), vect(-86.024, 18.68, -2.707), vect(-83.486, 18.693, -18.72), vect(-92.699, 21.56, -39.226), vect(-68.925, 21.226, -39.552), vect(-76.176, 22.938, -50.524), vect(-94.822, 26.656, -50.721), vect(-115.554, 33.92, -46.68), vect(-103.124, 30.437, -67.742), vect(-95.643, 34.067, -83.714), vect(-79.087, 38.749, -93.692), vect(-60.955, 38.666, -96.733), vect(-66.401, 33.245, -67.933), vect(-52.739, 23.053, -46.693), vect(-60.028, 19.654, -43.366), vect(-58.474, 21.34, -41.916), vect(-66.637, 45.398, -159.728), vect(-60.037, 20.182, 31.968)]

globalvar factoryChimneys = [vect(-103.743, 52.898, -7.883), vect(-99.076, 43.257, 2.691), vect(-102.726, 42.959, -19.337), vect(-103.258, 89.307, 87.771), vect(-97.873, 105.254, 147.556), vect(-67.28, 74.67, 132.059), vect(-62.083, 73.813, 126.798), vect(-57.532, 72.746, 122.459), vect(-66.3, 75.973, 178.452), vect(-72.412, 76.24, 179.874), vect(-79.238, 76.447, 182.438), vect(-129.017, 89.269, 91.937), vect(-144.804, 74.397, -107.024), vect(-125.329, 74.932, -138.975), vect(-97.122, 75.045, -161.248), vect(-167.897, 81.816, -224.31), vect(-188.259, 81.99, -201.101), vect(-164.15, 87.863, -170.611), vect(-169.701, 86.455, -165.337), vect(-174.196, 86.825, -161.172), vect(-209.549, 81.825, -166.638), vect(-230.149, 81.958, -143.489), vect(-162.518, 73.322, -113.439), vect(-167.523, 72.674, -108.708), vect(-172.385, 72.515, -104.155), vect(-229.017, 98.44, -117.275), vect(-209.918, 102.064, -79.232), vect(-205.703, 101.668, -57.003), vect(-202.844, 107.499, -48.26), vect(-185.025, 85.572, -10.228), vect(-184.592, 78.455, -34.968), vect(-190.427, 77.486, -30.211), vect(-195.793, 77.252, -25.99), vect(-222.553, 64.858, -33.789), vect(-219.947, 65.109, -29.693), vect(-216.783, 65.778, -24.939), vect(-293.325, 80.563, 7.092), vect(-291.052, 79.977, 14.33), vect(-289.456, 79.565, 20.633), vect(-308.515, 79.794, 51.695), vect(-312.363, 79.568, 19.821), vect(-324.182, 79.531, -18.248), vect(-327.922, 79.545, -49.132)]

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


/*
rule "Teleport player on pressing interact":
    @Event eachPlayer
    @Condition eventPlayer.isHoldingButton(Button.INTERACT)
    eventPlayer.teleport(eventPlayer.getEyePosition() + eventPlayer.getFacingDirection()*5)
    #Hold the player in place, to reset falling velocity
    eventPlayer.startForcingPosition(eventPlayer.getPosition(), false)
    wait()
    eventPlayer.stopForcingPosition()
*/
/*
rule "Display position":
    @Event eachPlayer
    print("Position of {}: {}".format(eventPlayer, eventPlayer.getPosition()))
    print(eventPlayer.isUsingUltimate())
    print(hostPlayer.getFacingDirection())
    eventPlayer.setGravity(25)
    eventPlayer.disableEnvironmentCollision(false)
    eventPlayer.setProjectileSpeed(0)
    eventPlayer.setProjectileGravity(500)
*/
rule "game instructions":
    @Condition isInSetup()
    setMatchTime(999)
    
    hudSubtext(getAllPlayers(), " \n\n\n\n", HudPosition.RIGHT, 1, Color.WHITE, HudReeval.VISIBILITY, SpecVisibility.DEFAULT)
    hudText(getAllPlayers(), " \n\n\n\n\n\n\n\n                                           \n\n\n\n\n\n\n\n\n\n\n\n\n", "                          ", null, HudPosition.RIGHT, 2, Color.TURQUOISE, Color.WHITE, Color.WHITE, HudReeval.VISIBILITY, SpecVisibility.DEFAULT)
    wait(5)
    hudSubtext(getAllPlayers(), " \n\n\n", HudPosition.TOP, 1, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudHeader(getAllPlayers(), "{0} santa's satan?! {0}".format(abilityIconString(Hero.MEI, Button.ULTIMATE)), HudPosition.TOP, 2, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    wait(5)
    hudSubtext(getAllPlayers(), " \n", HudPosition.TOP, 3, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudHeader(getAllPlayers(), "it's christmas, and it's time to deliver the gifts...", HudPosition.TOP, 4, Color.BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    wait(5)
    hudSubtext(getAllPlayers(), " \n", HudPosition.TOP, 5, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudHeader(getAllPlayers(), "throw snowball ({}) in each chimney to win!".format(buttonString(Button.ULTIMATE)), HudPosition.TOP, 6, Color.RED, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    wait(5)
    hudSubtext(getAllPlayers(), " \n", HudPosition.TOP, 7, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudHeader(getAllPlayers(), "but it seems your reindeer is not so cooperative...", HudPosition.TOP, 8, Color.GREEN, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    wait(7)
    hudSubtext(getAllPlayers(), " \n", HudPosition.TOP, 9, Color.SKY_BLUE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    hudHeader(getAllPlayers(), "turn with mouse, {} to go up, {} to go down".format(buttonString(Button.JUMP), buttonString(Button.CROUCH)), HudPosition.TOP, 10, Color.YELLOW, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.ALWAYS)
    wait(8)
    setMatchTime(5)

rule "game started":
    @Condition isGameInProgress()
    destroyAllHudTexts()
    gameStarted = true
    disableGamemodeCompletion()
    disableScoring()
    disableInspector()
    bigMessage(getAllPlayers(), "go!")
    wait()
    hudSubheader(getAllPlayers(), "load: {} | avg: {} | peak: {}".format(getServerLoad(), getAverageServerLoad(), getPeakServerLoad()), HudPosition.LEFT, 1, Color.WHITE, HudReeval.VISIBILITY_AND_STRING, SpecVisibility.DEFAULT)

/*
rule "debug raycast":
    @Event eachPlayer
    createEffect(getAllPlayers(), Effect.SPHERE, Color.WHITE, raycast(eventPlayer.getEyePosition(), eventPlayer.getEyePosition()+eventPlayer.getFacingDirection()*999, [], [], false).getHitPosition(), 0.1, EffectReeval.VISIBILITY_POSITION_AND_RADIUS)
*/
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

    #back point left
    sleighBeam(vect(1, 0, -2), vect(0, 2, -3))
    #back point right
    sleighBeam(vect(-1, 0, -2), vect(0, 2, -3))

    #front point left
    sleighBeam(vect(1, 0, 3), vect(0, 1.5, 2))
    #front point right
    sleighBeam(vect(-1, 0, 3), vect(0, 1.5, 2))

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


rule "game started, players can move":
    @Event eachPlayer
    @Condition gameStarted
    chase(eventPlayer.pos, eventPlayer.pos + eventPlayer.getFacingDirection()*vect(1,0,1)*99, rate=SPEED, ChaseReeval.DESTINATION_AND_RATE)
    chase(eventPlayer.score, 9999, rate=0.001, ChaseReeval.DESTINATION_AND_RATE)
    eventPlayer.stopFacing()

/*
rule "init player":
    @Event eachPlayer
*/
rule "set ult":
    @Event eachPlayer
    @Condition gameStarted
    @Condition eventPlayer.getUltCharge() < 100
    do:
        eventPlayer.setUltCharge(eventPlayer.getUltCharge()+1)
        wait()
    while RULE_CONDITION


#!define createChimneyEffect(idx) createEffect([p for p in getAllPlayers() if p.gottenChimneys[idx]], Effect.SPHERE, Color.AQUA, chimneys[idx], 1.5 if (idx) < len(rectangleChimneysSpheres)+len(sphereChimneys) else 3, EffectReeval.VISIBILITY)

rule "init chimney effects":
    createChimneyEffect(0)
    createChimneyEffect(1)
    createChimneyEffect(2)
    createChimneyEffect(3)
    createChimneyEffect(4)
    createChimneyEffect(5)
    createChimneyEffect(6)
    createChimneyEffect(7)
    createChimneyEffect(8)
    createChimneyEffect(9)
    createChimneyEffect(10)
    createChimneyEffect(11)
    createChimneyEffect(12)
    createChimneyEffect(13)
    createChimneyEffect(14)
    createChimneyEffect(15)
    createChimneyEffect(16)
    createChimneyEffect(17)
    createChimneyEffect(18)
    createChimneyEffect(19)
    createChimneyEffect(20)
    createChimneyEffect(21)
    createChimneyEffect(22)
    createChimneyEffect(23)
    createChimneyEffect(24)
    createChimneyEffect(25)
    createChimneyEffect(26)
    createChimneyEffect(27)
    createChimneyEffect(28)
    createChimneyEffect(29)
    createChimneyEffect(30)
    createChimneyEffect(31)
    createChimneyEffect(32)
    createChimneyEffect(33)
    createChimneyEffect(34)
    createChimneyEffect(35)
    createChimneyEffect(36)
    createChimneyEffect(37)
    createChimneyEffect(38)
    createChimneyEffect(39)
    createChimneyEffect(40)
    createChimneyEffect(41)
    createChimneyEffect(42)
    createChimneyEffect(43)
    createChimneyEffect(44)
    createChimneyEffect(45)
    createChimneyEffect(46)
    createChimneyEffect(47)
    createChimneyEffect(48)
    createChimneyEffect(49)
    createChimneyEffect(50)
    createChimneyEffect(51)
    createChimneyEffect(52)
    createChimneyEffect(53)
    createChimneyEffect(54)
    createChimneyEffect(55)
    createChimneyEffect(56)
    createChimneyEffect(57)
    createChimneyEffect(58)
    createChimneyEffect(59)
    createChimneyEffect(60)
    createChimneyEffect(61)
    createChimneyEffect(62)
    createChimneyEffect(63)
    createChimneyEffect(64)
    createChimneyEffect(65)
    createChimneyEffect(66)
    createChimneyEffect(67)
    createChimneyEffect(68)
    createChimneyEffect(69)
    createChimneyEffect(70)
    createChimneyEffect(71)
/*
    for i in range(len(chimneys)):
        createInWorldText(getAllPlayers(), i, chimneys[i], 2, Clip.NONE, WorldTextReeval.VISIBILITY, Color.WHITE, SpecVisibility.ALWAYS)
*/
#(0<AM⋅AB<AB⋅AB)∧(0<AM⋅AD<AD⋅AD)


subroutine gotChimney
def gotChimney():
    eventPlayer.gottenChimneys[eventPlayer.i] = true
    playEffect(eventPlayer, DynamicEffect.GOOD_PICKUP_EFFECT, Color.BLUE, chimneys[eventPlayer.i], 99)
    playEffect(eventPlayer, DynamicEffect.BUFF_EXPLOSION_SOUND, null, eventPlayer, 200)
    if eventPlayer.i == 5:
        eventPlayer.addToScore(3)
        eventPlayer.score = floor(eventPlayer.score+3)
        smallMessage(eventPlayer, "{0} Secret chimney! +3 points {0}".format(abilityIconString(Hero.MEI, Button.ULTIMATE)))
    else:
        eventPlayer.addToScore(1)
        eventPlayer.score = floor(eventPlayer.score+1)
        eventPlayer.nbChimneysGotten++
        smallMessage(eventPlayer, "{0} {1} {0}".format(abilityIconString(random.choice(getAllHeroes().exclude(Hero.BASTION)), random.choice([Button.ABILITY_1, Button.ABILITY_2, Button.ULTIMATE])), random.choice([
            "{} Woah! thank you mei, you're the best!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} Woah! thank you mei, you're the best!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} Woah! thank you mei, you're the best!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} Woah! thank you mei, you're the best!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} Woah! thank you mei, you're the best!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} Woah! thank you mei, you're the best!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} what an a-mei-zing christmas!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} what an a-mei-zing christmas!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} what an a-mei-zing christmas!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} thanks satan!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} thanks santa!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} thanks santa!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} thanks santa!".format(heroIcon(random.choice(getAllHeroes()))),
            "{} thanks santa!".format(heroIcon(random.choice(getAllHeroes()))),
            w"OwO",
            w"UwU",
            "mei-rry christmas!",
            "a child will be very happy that morning...",
            "where does this chimney even go to?",
            "mei-rry christmas!",
            "a child will be very happy that morning...",
            "where does this chimney even go to?",
            "mei-rry christmas!",
            "a child will be very happy that morning...",
            "where does this chimney even go to?",
            "phew, this was hard!",
            "let's make some omnics happy.",
            "time for some gifts!",
            "let's make some omnics happy.",
            "time for some gifts!",
            "let's make some omnics happy.",
            "time for some gifts!",
            "if only my reindeer would stop...",
            "...and i still have the rest of the world to go!",
            "one more gift, done!",
            "nerf this!",
            "fire in the hole! wait...",
        ])))
    

rule "chimney check":
    @Event eachPlayer
    @Condition eventPlayer.isHoldingButton(Button.ULTIMATE)
    @Condition eventPlayer.getUltCharge() == 100
    eventPlayer.allowButton(Button.ULTIMATE)
    eventPlayer.forceButtonPress(Button.ULTIMATE)
    wait(0.5)
    eventPlayer.disallowButton(Button.ULTIMATE)
    if eventPlayer.getPosition().x > 0:
        #check 6-pronged chimneys with corners
        for eventPlayer.i in range(len(rectangleChimneysSpheres)):
            if eventPlayer.gottenChimneys[eventPlayer.i]:
                continue
            if (
                eventPlayer.getPosition().y >= rectangleChimneys[eventPlayer.i*4].y
                and 0 < dotProduct(vectorTowards(rectangleChimneys[eventPlayer.i*4]*vect(1,0,1), eventPlayer.getPosition()*vect(1,0,1)), rectangleChimneysAB[eventPlayer.i]) 
                and dotProduct(vectorTowards(rectangleChimneys[eventPlayer.i*4]*vect(1,0,1), eventPlayer.getPosition()*vect(1,0,1)), rectangleChimneysAB[eventPlayer.i]) < rectangleChimneysABDot[eventPlayer.i] 
                and 0 < dotProduct(vectorTowards(rectangleChimneys[eventPlayer.i*4]*vect(1,0,1), eventPlayer.getPosition()*vect(1,0,1)), rectangleChimneysAD[eventPlayer.i])
                and dotProduct(vectorTowards(rectangleChimneys[eventPlayer.i*4]*vect(1,0,1), eventPlayer.getPosition()*vect(1,0,1)), rectangleChimneysAD[eventPlayer.i]) < rectangleChimneysADDot[eventPlayer.i]
            ):
                gotChimney()
                break
    else:
        #check other chimneys
        for eventPlayer.i in range(len(rectangleChimneysSpheres), len(chimneys)):
            if eventPlayer.gottenChimneys[eventPlayer.i]:
                continue
            if eventPlayer.getPosition().y >= chimneys[eventPlayer.i].y and (
                eventPlayer.i < len(rectangleChimneysSpheres)+len(sphereChimneys) and distance(eventPlayer.getPosition()*vect(1,0,1), chimneys[eventPlayer.i]*vect(1,0,1)) <= 1.5
                or eventPlayer.i >= len(rectangleChimneysSpheres)+len(sphereChimneys) and distance(eventPlayer.getPosition()*vect(1,0,1), chimneys[eventPlayer.i]*vect(1,0,1)) <= 3
            ):
                gotChimney()
                break
                

    #createEffect(getAllPlayers(), Effect.LIGHT_SHAFT, Color.WHITE, raycast(eventPlayer.getEyePosition(), eventPlayer.getEyePosition()+Vector.DOWN*999, [], [], false).getHitPosition(), 0.1, EffectReeval.VISIBILITY)
    

rule "go up":
    @Event eachPlayer
    @Condition gameStarted
    @Condition eventPlayer.isHoldingButton(Button.JUMP)
    @Condition eventPlayer.getPosition().y < 150
    do:
        eventPlayer.pos += Vector.UP*0.1
        wait()
    while RULE_CONDITION
    
rule "go down":
    @Event eachPlayer
    @Condition gameStarted
    @Condition eventPlayer.isHoldingButton(Button.CROUCH)
    @Condition eventPlayer.getPosition().y > 15
    do:
        eventPlayer.pos += Vector.DOWN*0.1
        wait()
    while RULE_CONDITION



rule "player found all chimneys":
    @Event eachPlayer
    @Condition finisher == null
    @Condition eventPlayer.nbChimneysGotten >= 71
    bigMessage(getAllPlayers(), "{} delivered to all chimneys!".format(eventPlayer))
    finisher = eventPlayer
    wait(3)
    bigMessage(getAllPlayers(), "Game ends in 30 seconds")
    setMatchTime(30)

rule "game ended":
    @Condition getMatchTime() == 0
    @Condition isGameInProgress()
    declarePlayerVictory(sorted(getAllPlayers(), lambda p: p.score).last())


#chimneys with 6 prongs (5) (x > 0)
#[vect(49.844, 25.742, -37.986), vect(55.628, 25.742, -22.821), vect(37.774, 28.637, -26.733), vect(12.577, 28.431, -22.653), vect(13.038, 28.433, 6.908)]

#with corners
#[vect(56.92, 24.863, -22.05), vect(55.639, 24.863, -21.332), vect(54.394, 24.863, -23.549), vect(55.675, 24.863, -24.274), vect(49.909, 24.863, -39.421), vect(48.644, 24.863, -38.668), vect(49.949, 24.863, -36.48), vect(51.213, 24.863, -37.232), vect(37.326, 27.757, -25.265), vect(38.763, 27.757, -25.573), vect(38.237, 27.757, -28.063), vect(36.8, 27.757, -27.759), vect(11.209, 27.553, -23.272), vect(11.427, 27.553, -21.816), vect(13.946, 27.553, -22.191), vect(13.725, 27.553, -23.647), vect(14.175, 27.553, 7.772), vect(14.397, 27.553, 6.316), vect(11.877, 27.553, 5.935), vect(11.656, 27.553, 7.389)]


#with 3 (22)
#[vect(-50.419, 28.464, -13.289), vect(-57.093, 19.03, -19.035), vect(-66.214, 20.165, -3.68), vect(-59.483, 24.523, 2.352), vect(-76.523, 21.031, -4.917), vect(-78.695, 17.573, -1.593), vect(-86.024, 18.68, -2.707), vect(-83.486, 18.693, -18.72), vect(-92.699, 21.56, -39.226), vect(-68.925, 21.226, -39.552), vect(-76.176, 22.938, -50.524), vect(-94.822, 26.656, -50.721), vect(-115.554, 33.92, -46.68), vect(-103.124, 30.437, -67.742), vect(-95.643, 34.067, -83.714), vect(-79.087, 38.749, -93.692), vect(-60.955, 38.666, -96.733), vect(-66.401, 33.245, -67.933), vect(-52.739, 23.053, -46.693), vect(-60.028, 19.654, -43.366), vect(-58.474, 21.34, -41.916), vect(-66.637, 45.398, -159.728)]

#with hole (1)
#[vect(-60.037, 20.182, 31.968)]

#factory (43)
#[vect(-103.743, 52.898, -7.883), vect(-99.076, 43.257, 2.691), vect(-102.726, 42.959, -19.337), vect(-103.258, 89.307, 87.771), vect(-97.873, 105.254, 147.556), vect(-67.28, 74.67, 132.059), vect(-62.083, 73.813, 126.798), vect(-57.532, 72.746, 122.459), vect(-66.3, 75.973, 178.452), vect(-72.412, 76.24, 179.874), vect(-79.238, 76.447, 182.438), vect(-129.017, 89.269, 91.937), vect(-144.804, 74.397, -107.024), vect(-125.329, 74.932, -138.975), vect(-97.122, 75.045, -161.248), vect(-167.897, 81.816, -224.31), vect(-188.259, 81.99, -201.101), vect(-164.15, 87.863, -170.611), vect(-169.701, 86.455, -165.337), vect(-174.196, 86.825, -161.172), vect(-209.549, 81.825, -166.638), vect(-230.149, 81.958, -143.489), vect(-162.518, 73.322, -113.439), vect(-167.523, 72.674, -108.708), vect(-172.385, 72.515, -104.155), vect(-229.017, 98.44, -117.275), vect(-209.918, 102.064, -79.232), vect(-205.703, 101.668, -57.003), vect(-202.844, 107.499, -48.26), vect(-185.025, 85.572, -10.228), vect(-184.592, 78.455, -34.968), vect(-190.427, 77.486, -30.211), vect(-195.793, 77.252, -25.99), vect(-222.553, 64.858, -33.789), vect(-219.947, 65.109, -29.693), vect(-216.783, 65.778, -24.939), vect(-293.325, 80.563, 7.092), vect(-291.052, 79.977, 14.33), vect(-289.456, 79.565, 20.633), vect(-308.515, 79.794, 51.695), vect(-312.363, 79.568, 19.821), vect(-324.182, 79.531, -18.248), vect(-327.922, 79.545, -49.132)]

#secret
#[vect(-17.768, 76.344, -45.534)]
/*
rule "add chimney":
    @Event eachPlayer
    @Condition eventPlayer.isHoldingButton(Button.MELEE)
    chimneys.append(eventPlayer.getPosition())
    createEffect(getAllPlayers(), Effect.SPHERE, Color.ORANGE, eventPlayer.getPosition(), 3, EffectReeval.VISIBILITY)*/

/*
rule "add chimney":
    @Event eachPlayer
    @Condition eventPlayer.isHoldingButton(Button.MELEE)
    chimneys.append(raycast(eventPlayer.getEyePosition(), eventPlayer.getEyePosition()+eventPlayer.getFacingDirection()*999, [], [], false).getHitPosition())
    createEffect(getAllPlayers(), Effect.SPHERE, Color.WHITE, raycast(eventPlayer.getEyePosition(), eventPlayer.getEyePosition()+eventPlayer.getFacingDirection()*999, [], [], false).getHitPosition(), 0.1, EffectReeval.VISIBILITY)
*/

globalvar antiCrashActivated
rule "anti crash":
	@Event global
	@Condition not antiCrashActivated and getServerLoad() > 250
    wait(1, Wait.ABORT_WHEN_FALSE)
    smallMessage(getAllPlayers(), "anti-crash system activated")
    setSlowMotion(10)
    antiCrashActivated = true

rule "disable anti crash":
	@Event global
	@Condition antiCrashActivated and getServerLoad() < 230
    setSlowMotion(100)
    antiCrashActivated = false