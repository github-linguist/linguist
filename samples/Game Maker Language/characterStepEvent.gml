// Originally from /spelunky/Scripts/Platform Engine/characterStepEvent.gml in the Spelunky Community Update Project

/**********************************************************************************
    Copyright (c) 2008, 2009 Derek Yu and Mossmouth, LLC
    
    This file is part of Spelunky.

    You can redistribute and/or modify Spelunky, including its source code, under
    the terms of the Spelunky User License.

    Spelunky is distributed in the hope that it will be entertaining and useful,
    but WITHOUT WARRANTY. Please see the Spelunky User License for more details.

    The Spelunky User License should be available in "Game Information", which
    can be found in the Resource Explorer, or as an external file called COPYING.
    If not, please obtain a new copy of Spelunky from <http://spelunkyworld.com/>
    
***********************************************************************************/

/*
This script should be placed in the step event for the platform character.
It updates the keys used by the character, moves all of the solids, moves the
character, sets the sprite index, and sets the animation speed for the sprite.
*/
hangCountMax = 3;

//////////////////////////////////////
// KEYS
//////////////////////////////////////

kLeft = checkLeft();

if (kLeft) kLeftPushedSteps += 1;
else kLeftPushedSteps = 0;
  
kLeftPressed = checkLeftPressed();
kLeftReleased = checkLeftReleased();
  
kRight = checkRight();
  
if (kRight) kRightPushedSteps += 1;
else kRightPushedSteps = 0;
  
kRightPressed = checkRightPressed();
kRightReleased = checkRightReleased();
  
kUp = checkUp();
kDown = checkDown();
  
//key "run"
if canRun
    kRun = 0;
// kRun=runKey
else
    kRun=0
  
kJump = checkJump();
kJumpPressed = checkJumpPressed();
kJumpReleased = checkJumpReleased();
  
if (cantJump > 0)
{
    kJump = 0;
    kJumpPressed = 0;
    kJumpReleased = 0;
    cantJump -= 1;
}
else
{
    if (global.isTunnelMan and
        sprite_index == sTunnelAttackL and
        !holdItem)
    {
        kJump = 0;
        kJumpPressed = 0;
        kJumpReleased = 0;
        cantJump -= 1;
    }
}

kAttack = checkAttack();
kAttackPressed = checkAttackPressed();
kAttackReleased = checkAttackReleased();

kItemPressed = checkItemPressed();

xPrev = x;
yPrev = y;

if (stunned or dead)
{
    kLeft = false;
    kLeftPressed = false;
    kLeftReleased = false;
    kRight = false;
    kRightPressed = false;
    kRightReleased = false;
    kUp = false;
    kDown = false;
    kJump = false;
    kJumpPressed = false;
    kJumpReleased = false;
    kAttack = false;
    kAttackPressed = false;
    kAttackReleased = false;
    kItemPressed = false;
}

//////////////////////////////////////////
// Collisions
//////////////////////////////////////////

colSolidLeft = false;
colSolidRight = false;
colLeft = false;
colRight = false;
colTop = false;
colBot = false;
colLadder = false;
colPlatBot = false;
colPlat = false;
colWaterTop = false;
colIceBot = false;
runKey = false;
if (isCollisionMoveableSolidLeft(1)) colSolidLeft = true;
if (isCollisionMoveableSolidRight(1)) colSolidRight = true;
if (isCollisionLeft(1)) colLeft = true;
if (isCollisionRight(1)) colRight = true;
if (isCollisionTop(1)) colTop = true;
if (isCollisionBottom(1)) colBot = true;
if (isCollisionLadder()) colLadder = true;
if (isCollisionPlatformBottom(1)) colPlatBot = true;
if (isCollisionPlatform()) colPlat = true;
if (isCollisionWaterTop(1)) colWaterTop = true;
if (collision_point(x, y+8, oIce, 0, 0)) colIceBot = true;
if (checkRun())
{
    runHeld = 100;
    runKey = true;
}

if (checkAttack() and not whipping)
{
    runHeld += 1;
    runKey = true;
}

if (not runKey or (not kLeft and not kRight)) runHeld = 0;

// allows the character to run left and right
// if state!=DUCKING and state!=LOOKING_UP and state!=CLIMBING
if (state != CLIMBING and state != HANGING)
{
    if (kLeftReleased and approximatelyZero(xVel)) xAcc -= 0.5
    if (kRightReleased and approximatelyZero(xVel)) xAcc += 0.5
    
    if (kLeft and not kRight)
    {
        if (colSolidLeft)
        {
            // xVel = 3;
            if (platformCharacterIs(ON_GROUND) and state != DUCKING)
            {
                xAcc -= 1;
                pushTimer += 10;
                //if (not SS_IsSoundPlaying(global.sndPush)) playSound(global.sndPush);
            }
        }
        else if (kLeftPushedSteps > 2) and (facing=LEFT or approximatelyZero(xVel))
        {
            xAcc -= runAcc;
        }
        facing = LEFT;
        //if (platformCharacterIs(ON_GROUND) and abs(xVel) > 0 and alarm[3] < 1) alarm[3] = floor(16/-xVel);
    }
  
    if (kRight and not kLeft)
    {
        if (colSolidRight)
        {
            // xVel = 3;
            if (platformCharacterIs(ON_GROUND) and state != DUCKING)
            {
                xAcc += 1;
                pushTimer += 10;
                //if (not SS_IsSoundPlaying(global.sndPush)) playSound(global.sndPush);
            }
        }
        else if (kRightPushedSteps > 2 or colSolidLeft) and (facing=RIGHT or approximatelyZero(xVel))
        {
            xAcc += runAcc;
        }
        facing = RIGHT;
        //if (platformCharacterIs(ON_GROUND) and abs(xVel) > 0 and alarm[3] < 1) alarm[3] = floor(16/xVel);
    }
}

/******************************************

  LADDERS
  
*******************************************/

if (state == CLIMBING)
{
    if (instance_exists(oCape))
    {
        oCape.open = false;
    }
    kJumped = false;
    ladderTimer = 10;
    ladder = collision_point(x, y, oLadder, 0, 0);
    if (ladder) x = ladder.x + 8;

    if (kLeft) facing = LEFT;
    else if (kRight) facing = RIGHT;
    if (kUp)
    {
        if (collision_point(x, y-8, oLadder, 0, 0) or collision_point(x, y-8, oLadderTop, 0, 0))
        {
            yAcc -= climbAcc;
            if (alarm[2] < 1) alarm[2] = 8;
        }
    }
    else if (kDown)
    {
        if (collision_point(x, y+8, oLadder, 0, 0) or collision_point(x, y+8, oLadderTop, 0, 0))
        {
            yAcc += climbAcc;
            if (alarm[2] < 1) alarm[2] = 8;
        }
        else
            state = FALLING;
        if (colBot) state = STANDING;
    }
    
    if (kJumpPressed and not whipping)
    {
        if (kLeft)
            xVel = -departLadderXVel;
        else if (kRight)
            xVel = departLadderXVel;
        else
            xVel = 0;
        yAcc += departLadderYVel;
        state = JUMPING;
        jumpButtonReleased = 0;
        jumpTime = 0;
        ladderTimer = 5;
    }
}
else
{
     if (ladderTimer > 0) ladderTimer -= 1;
}

if (platformCharacterIs(IN_AIR) and state != HANGING)
{
    yAcc += gravityIntensity;
}

// Player has landed
if ((colBot or colPlatBot) and platformCharacterIs(IN_AIR) and yVel >= 0)
{
    if (not colPlat or colBot)
    {
        yVel = 0;
        yAcc = 0;
        state = RUNNING;
        jumps = 0;
    }
    //playSound(global.sndLand);
}
if ((colBot or colPlatBot) and not colPlat) yVel = 0;

// Player has just walked off of the edge of a solid
if (colBot == 0 and (not colPlatBot or colPlat) and platformCharacterIs(ON_GROUND))
{
    state = FALLING;
    yAcc += grav;
    kJumped = true;
    if (global.hasGloves) hangCount = 5;
}

if (colTop)
{
    if (dead or stunned) yVel = -yVel * 0.8;
    else if (state == JUMPING) yVel = abs(yVel*0.3)
}

if (colLeft and facing == LEFT) or (colRight and facing == RIGHT)
{
    if (dead or stunned) xVel = -xVel * 0.5;
    else xVel = 0;
}

/******************************************

  JUMPING
  
*******************************************/

if (kJumpReleased and platformCharacterIs(IN_AIR))
{
    kJumped = true;
}
else if (platformCharacterIs(ON_GROUND))
{
    oCape.open = false;
    kJumped = false;
}

if (kJumpPressed and collision_point(x, y, oWeb, 0, 0))
{
    obj = instance_place(x, y, oWeb);
    obj.life -= 1;
    yAcc += initialJumpAcc * 2;
    yVel -= 3;
    xAcc += xVel/2;
    
    state = JUMPING;
    jumpButtonReleased = 0;
    jumpTime = 0;
    
    grav = gravNorm;
}
else if (kJumpPressed and colWaterTop)
{
    yAcc += initialJumpAcc * 2;
    yVel -= 3;
    xAcc += xVel/2;
    
    state = JUMPING;
    jumpButtonReleased = 0;
    jumpTime = 0;
    
    grav = gravNorm;
}
else if (global.hasCape and kJumpPressed and kJumped and platformCharacterIs(IN_AIR))
{
    if (not oCape.open) oCape.open = true;
    else oCape.open = false;
}
else if (global.hasJetpack and kJump and kJumped and platformCharacterIs(IN_AIR) and jetpackFuel > 0)
{
    yAcc += initialJumpAcc;
    yVel = -1;
    jetpackFuel -= 1;
    if (alarm[10] < 1) alarm[10] = 3;
    
    state = JUMPING;
    jumpButtonReleased = 0;
    jumpTime = 0;
    
    grav = 0;
}
else if (platformCharacterIs(ON_GROUND) and kJumpPressed and fallTimer == 0)
{
    if (xVel > 3 or xVel < -3)
    {
        yAcc += initialJumpAcc * 2;
        xAcc += xVel * 2;
    }
    else
    {
        yAcc += initialJumpAcc * 2;
        xAcc += xVel/2;
    }
    
    if (global.hasJordans)
    {
        yAcc *= 3;
        yAccLimit = 12;
        grav = 0.5;
    }
    else if (global.hasSpringShoes) yAcc *= 1.5;
    else
    {
        yAccLimit = 6;
        grav = gravNorm;
    }
    
    playSound(global.sndJump);
    
    pushTimer = 0;

    // the "state" gets changed to JUMPING later on in the code
    state = FALLING;
    // "variable jumping" states
    jumpButtonReleased = 0;
    jumpTime = 0;
}

if (jumpTime < jumpTimeTotal) jumpTime += 1;
//let the character continue to jump
if (kJump == 0) jumpButtonReleased = 1;
if (jumpButtonReleased) jumpTime = jumpTimeTotal;

gravityIntensity = (jumpTime/jumpTimeTotal) * grav;

if (kUp and platformCharacterIs(ON_GROUND) and not colLadder)
{
    looking = UP;
    if (xVel == 0 and xAcc == 0) state = LOOKING_UP;
}
else looking = 0;

if (not kUp and state == LOOKING_UP)
{
    state=STANDING
}

/******************************************

  HANGING
  
*******************************************/

if (not colTop)
{
if (global.hasGloves and yVel > 0)
{
    if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and kRight and colRight and
        (collision_point(x+9, y-5, oSolid, 0, 0) or collision_point(x+9, y-6, oSolid, 0, 0)))
    {
        state = HANGING;
        move_snap(1, 8);
        yVel = 0;
        yAcc = 0;
        grav = 0;
    }
    else if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and kLeft and colLeft and
        (collision_point(x-9, y-5, oSolid, 0, 0) or collision_point(x-9, y-6, oSolid, 0, 0)))
    {
        state = HANGING;
        move_snap(1, 8);
        yVel = 0;
        yAcc = 0;
        grav = 0;
    }
}
else if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and kRight and colRight and
        (collision_point(x+9, y-5, oTree, 0, 0) or collision_point(x+9, y-6, oTree, 0, 0)))
{
    state = HANGING;
    move_snap(1, 8);
    yVel = 0;
    yAcc = 0;
    grav = 0;
}
else if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and kLeft and colLeft and
        (collision_point(x-9, y-5, oTree, 0, 0) or collision_point(x-9, y-6, oTree, 0, 0)))
{
    state = HANGING;
    move_snap(1, 8);
    yVel = 0;
    yAcc = 0;
    grav = 0;
}
else if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and kRight and colRight and
    (collision_point(x+9, y-5, oSolid, 0, 0) or collision_point(x+9, y-6, oSolid, 0, 0)) and
    not collision_point(x+9, y-9, oSolid, 0, 0) and not collision_point(x, y+9, oSolid, 0, 0))
{
  state = HANGING;
  move_snap(1, 8);
  yVel = 0;
  yAcc = 0;
  grav = 0;
}
else if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and kLeft and colLeft and
    (collision_point(x-9, y-5, oSolid, 0, 0) or collision_point(x-9, y-6, oSolid, 0, 0)) and
    not collision_point(x-9, y-9, oSolid, 0, 0) and not collision_point(x, y+9, oSolid, 0, 0))
{
  state = HANGING;
  move_snap(1, 8);
  yVel = 0;
  yAcc = 0;
  grav = 0;
}

if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and state == FALLING and
    (collision_point(x, y-5, oArrow, 0, 0) or collision_point(x, y-6, oArrow, 0, 0)) and
    not collision_point(x, y-9, oArrow, 0, 0) and not collision_point(x, y+9, oArrow, 0, 0))
{
    obj = instance_nearest(x, y-5, oArrow);
    if (obj.stuck)
    {
        state = HANGING;
        // move_snap(1, 8);
        yVel = 0;
        yAcc = 0;
        grav = 0;
    }
}

/*
if (hangCount == 0 and y > 16 and !platformCharacterIs(ON_GROUND) and state == FALLING and
    (collision_point(x, y-5, oTreeBranch, 0, 0) or collision_point(x, y-6, oTreeBranch, 0, 0)) and
    not collision_point(x, y-9, oTreeBranch, 0, 0) and not collision_point(x, y+9, oTreeBranch, 0, 0))
{
  state = HANGING;
  // move_snap(1, 8);
  yVel = 0;
  yAcc = 0;
  grav = 0;
}
*/

}

if (hangCount > 0) hangCount -= 1;

if (state == HANGING)
{
    if (instance_exists(oCape)) oCape.open = false;
    kJumped = false;
    
    if (kDown and kJumpPressed)
    {
        grav = gravNorm;
        state = FALLING;
        yAcc -= grav;
        hangCount = 5;
        if (global.hasGloves) hangCount = 10;
    }
    else if (kJumpPressed)
    {
        grav = gravNorm;
        if ((facing == RIGHT and kLeft) or (facing == LEFT and kRight))
        {
            state = FALLING;
            yAcc -= grav;
        }
        else
        {
            state = JUMPING;
            yAcc += initialJumpAcc * 2;
            if (facing == RIGHT) x -= 2;
            else x += 2;
        }
        hangCount = hangCountMax;
    }
    
    if ((facing == LEFT and not isCollisionLeft(2)) or
        (facing == RIGHT and not isCollisionRight(2)))
    {
        grav = gravNorm;
        state = FALLING;
        yAcc -= grav;
        hangCount = 4;
    }
}
else
{
    grav = gravNorm;
}

// pressing down while standing
if (kDown and platformCharacterIs(ON_GROUND) and not whipping)
{
    if (colBot)
    {
        state = DUCKING;
    }
    else if colPlatBot
    {
        // climb down ladder if possible, else jump down
        fallTimer = 0;
        if (not colBot)
        {
            ladder = 0;
            ladder = instance_place(x, y+16, oLadder);
            if (instance_exists(ladder))
            {
                if (abs(x-(ladder.x+8)) < 4)
                {
                    x = ladder.x + 8;

                    xVel = 0;
                    yVel = 0;
                    xAcc = 0;
                    yAcc = 0;
                    state = CLIMBING;
                }
            }
            else
            {
                y += 1;
                state = FALLING;
                yAcc += grav;
            }
        }
        else
        {
            //the character can't move down because there is a solid in the way
            state = RUNNING;
        }
    }
}
if (not kDown and state == DUCKING)
{
    state = STANDING;
    xVel = 0;
    xAcc = 0;
}
if (xVel == 0 and xAcc == 0 and state == RUNNING)
{
    state = STANDING;
}
if (xAcc != 0 and state == STANDING)
{
    state = RUNNING;
}
if (yVel < 0 and platformCharacterIs(IN_AIR) and state != HANGING)
{
    state = JUMPING;
}
if (yVel > 0 and platformCharacterIs(IN_AIR) and state != HANGING)
{
    state = FALLING;
    setCollisionBounds(-5, -6, 5, 8);
}
else setCollisionBounds(-5, -8, 5, 8);

// CLIMB LADDER
colPointLadder = collision_point(x, y, oLadder, 0, 0) or collision_point(x, y, oLadderTop, 0, 0);

if ((kUp and platformCharacterIs(IN_AIR) and collision_point(x, y-8, oLadder, 0, 0) and ladderTimer == 0) or
    (kUp and colPointLadder and ladderTimer == 0) or
    (kDown and colPointLadder and ladderTimer == 0 and platformCharacterIs(ON_GROUND) and collision_point(x, y+9, oLadderTop, 0, 0) and xVel == 0))
{
    ladder = 0;
    ladder = instance_place(x, y-8, oLadder);
    if (instance_exists(ladder))
    {
        if (abs(x-(ladder.x+8)) < 4)
        {
            x = ladder.x + 8;
            if (not collision_point(x, y, oLadder, 0, 0) and
                not collision_point(x, y, oLadderTop, 0, 0))
            {
                y = ladder.y + 14;
            }

            xVel = 0;
            yVel = 0;
            xAcc = 0;
            yAcc = 0;
            state = CLIMBING;
        }
    }
}

/*
if (sprite_index == sDuckToHangL or sprite_index == sDamselDtHL)
{
    ladder = 0;
    if (facing == LEFT and collision_rectangle(x-8, y, x, y+16, oLadder, 0, 0) and not collision_point(x-4, y+16, oSolid, 0, 0))
    {
        ladder = instance_nearest(x-4, y+16, oLadder);
    }
    else if (facing == RIGHT and collision_rectangle(x, y, x+8, y+16, oLadder, 0, 0) and not collision_point(x+4, y+16, oSolid, 0, 0))
    {
        ladder = instance_nearest(x+4, y+16, oLadder);
    }
    
    if (ladder)
    {
        x = ladder.x + 8;

        xVel = 0;
        yVel = 0;
        xAcc = 0;
        yAcc = 0;
        state = CLIMBING;
    }
}
*/
/*
if (colLadder and state == CLIMBING and kJumpPressed and not whipping)
{
    if (kLeft)
        xVel = -departLadderXVel;
    else if (kRight)
        xVel = departLadderXVel;
    else
        xVel = 0;
    yAcc += departLadderYVel;
    state = JUMPING;
    jumpButtonReleased = 0;
    jumpTime = 0;
    ladderTimer = 5;
}
*/

// Calculate horizontal/vertical friction
if (state == CLIMBING)
{
    xFric = frictionClimbingX;
    yFric = frictionClimbingY;
}
else
{
    if (runKey and platformCharacterIs(ON_GROUND) and runHeld >= 10)
    {
        if (kLeft) // run
        {
            xVel -= 0.1;
            xVelLimit = 6;
            xFric = frictionRunningFastX;
        }
        else if (kRight)
        {
            xVel += 0.1;
            xVelLimit = 6;
            xFric = frictionRunningFastX;
        }
    }
    else if (state == DUCKING)
    {
        if (xVel<2 and xVel>-2)
        {
            xFric = 0.2
            xVelLimit = 3;
            image_speed = 0.8;
        }
        else if (kLeft and global.downToRun) // run
        {
            xVel -= 0.1;
            xVelLimit = 6;
            xFric = frictionRunningFastX;
        }
        else if (kRight and global.downToRun)
        {
            xVel += 0.1;
            xVelLimit = 6;
            xFric = frictionRunningFastX;
        }
        else
        {
            xVel *= 0.8;
            if (xVel < 0.5) xVel = 0;
            xFric = 0.2
            xVelLimit = 3;
            image_speed = 0.8;
        }
    }
    else
    {
        //decrease the friction when the character is "flying"
        if (platformCharacterIs(IN_AIR))
        {
            if (dead or stunned) xFric = 1.0;
            else xFric = 0.8;
        }
        else
        {
            xFric = frictionRunningX;
        }
    }
    
    // Stuck on web or underwater
    if (collision_point(x, y, oWeb, 0, 0))
    {
        xFric = 0.2;
        yFric = 0.2;
        fallTimer = 0;
    }
    else if (collision_point(x, y, oWater, -1, -1))
    {
        if (instance_exists(oCape)) oCape.open = false;
    
        if (state == FALLING and yVel > 0)
        {
            yFric = 0.5;
        }
        else if (not collision_point(x, y-9, oWater, -1, -1))
        {
            yFric = 1;
        }
        else
        {
            yFric = 0.9;
        }
    }
    else
    {
        swimming = false;
        yFric = 1;
    }
}

if (colIceBot and state != DUCKING and not global.hasSpikeShoes)
{
    xFric = 0.98;
    yFric = 1;
}

// RUNNING

if (platformCharacterIs(ON_GROUND))
{
    if (state == RUNNING and kLeft and colLeft)
    {
        pushTimer += 1;
    }
    else if (state == RUNNING and kRight and colRight)
    {
        pushTimer += 1;
    }
    else
    {
        pushTimer = 0;
    }
    
    if (platformCharacterIs(ON_GROUND) and not kJump and not kDown and not runKey)
    {   
        xVelLimit = 3;
    }
    
    
    // ledge flip
    if (state == DUCKING and abs(xVel) < 3 and facing == LEFT and
        collision_point(x, y+9, oSolid, 0, 0) and not collision_line(x-1, y+9, x-10, y+9, oSolid, 0, 0) and kLeft)
    {
        state = DUCKTOHANG;
        
        if (holdItem)
        {
            holdItem.held = false;
            if (holdItem.type == "Gold Idol") holdItem.y -= 8;
            scrDropItem(-1, -4);
        }
        
        with oMonkey
        {
            // knock off monkeys that grabbed you
            if (status == 7)
            {
                xVel = -1;
                yVel = -4;
                status = 1;
                vineCounter = 20;
                grabCounter = 60;
            }
        }
    }
    else if (state == DUCKING and abs(xVel) < 3 and facing == RIGHT and
        collision_point(x, y+9, oSolid, 0, 0) and not collision_line(x+1, y+9, x+10, y+9, oSolid, 0, 0) and kRight)
    {
        state = DUCKTOHANG;
        
        if (holdItem)
        {
            // holdItem.held = false;
            if (holdItem.type == "Gold Idol") holdItem.y -= 8;
            scrDropItem(1, -4);
        }
        
        with oMonkey
        {
            // knock off monkeys that grabbed you
            if (status == 7)
            {
                xVel = 1;
                yVel = -4;
                status = 1;
                vineCounter = 20;
                grabCounter = 60;
            }
        }
    }
}

if (state == DUCKTOHANG)
{
    x = xPrev;
    y = yPrev;
    xVel = 0;
    yVel = 0;
    xAcc = 0;
    yAcc = 0;
    grav = 0;
}

// PARACHUTE AND CAPE
if (instance_exists(oParachute))
{
    yFric = 0.5;
}
if (instance_exists(oCape))
{
    if (oCape.open) yFric = 0.5;
}

if (pushTimer > 100) pushTimer = 100;

// limits the acceleration if it is too extreme
if (xAcc > xAccLimit) xAcc = xAccLimit;
else if (xAcc < -xAccLimit) xAcc = -xAccLimit;
if (yAcc > yAccLimit) yAcc = yAccLimit;
else if (yAcc < -yAccLimit) yAcc = -yAccLimit;

// applies the acceleration
xVel += xAcc;
if (dead or stunned) yVel += 0.6;
else yVel += yAcc;

// nullifies the acceleration
xAcc = 0;
yAcc = 0;

// applies the friction to the velocity, now that the velocity has been calculated
xVel *= xFric;
yVel *= yFric;

// apply ball and chain
if (instance_exists(oBall))
{
    if (distance_to_object(oBall) >= 24)
    {
        if (xVel > 0 and oBall.x < x and abs(oBall.x-x) > 24) xVel = 0;
        if (xVel < 0 and oBall.x > x and abs(oBall.x-x) > 24) xVel = 0;
        if (yVel > 0 and oBall.y < y and abs(oBall.y-y) > 24)
        {
            if (abs(oBall.x-x) < 1)
            {
                x = oBall.x;
            }
            else if (oBall.x < x and not kRight)
            {
                if (xVel > 0) xVel *= -0.25;
                else if (xVel == 0) xVel -= 1;
            }
            else if (oBall.x > x and not kLeft)
            {
                if (xVel < 0) xVel *= -0.25;
                else if (xVel == 0) xVel += 1;
            }
            yVel = 0;
            fallTimer = 0;
        }
        if (yVel < 0 and oBall.y > y and abs(oBall.y-y) > 24) yVel = 0;
    }
}

// apply the limits since the velocity may be too extreme
if (not dead and not stunned)
{
    if (xVel > xVelLimit) xVel = xVelLimit;
    else if (xVel < -xVelLimit) xVel = -xVelLimit;
}
if (yVel > yVelLimit) yVel = yVelLimit;
else if (yVel < -yVelLimit) yVel = -yVelLimit;
    
// approximates the "active" variables
if approximatelyZero(xVel) xVel=0
if approximatelyZero(yVel) yVel=0
if approximatelyZero(xAcc) xAcc=0
if approximatelyZero(yAcc) yAcc=0

// prepares the character to move up a hill
// we need to use the "slopeYPrev" variable later to know the "true" y previous value
// keep this condition the same
if maxSlope>0 and platformCharacterIs(ON_GROUND) and xVel!=0
{
  slopeYPrev=y
  for(y=y;y>=slopeYPrev-maxSlope;y-=1)
    if colTop
      break
  slopeChangeInY=slopeYPrev-y
}
else
  slopeChangeInY=0

// moves the character, and balances out the effects caused by other processes
// keep this condition the same
if maxSlope*abs(xVel)>0 and platformCharacterIs(ON_GROUND)
{
  // we need to check if we should dampen out the speed as the character runs on upward slopes
  xPrev=x
  yPrev=slopeYPrev       // we don't want to use y, because y is too high
  yPrevHigh=y            // we'll use the higher previous variable later
  moveTo(xVel,yVel+slopeChangeInY)
  dist=point_distance(xPrev,yPrev,x,y)// overall distance that has been traveled
  // we should have only ran at xVel
  if dist>abs(xVelInteger)
  {
    // show_message(string(dist)+ " "+string(abs(xVelInteger)))
    excess=dist-abs(xVelInteger)
    if(xVelInteger<0)
      excess*=-1
    // move back since the character moved too far
    x=xPrev
    y=yPrevHigh     // we need the character to be high so the character can move down
    // this time we'll move the correct distance, but we need to shorten out the xVel a little
    // these lines can be changed for different types of slowing down when running up hills
    ratio=abs(xVelInteger)/dist*0.9        //can be changed
    moveTo( round(xVelInteger*ratio),round(yVelInteger*ratio+slopeChangeInY) )
  }
}
else
{
  // we simply move xVel and yVel while in the air or on a ladder
  moveTo(xVel,yVel)
}
// move the character downhill if possible
// we need to multiply maxDownSlope by the absolute value of xVel since the character normally runs at an xVel larger than 1
if not colBot and maxDownSlope>0 and xVelInteger!=0 and platformCharacterIs(ON_GROUND)
{
  //the character is floating just above the slope, so move the character down
  upYPrev=y
  for(y=y;y<=upYPrev+maxDownSlope;y+=1)
    if colBot             // we hit a solid below
    {
      upYPrev=y           // I know that this doesn't seem to make sense, because of the name of the variable, but it all works out correctly after we break out of this loop
      break
    }
  y=upYPrev
}

//figures out what the sprite index of the character should be
characterSprite();

//sets the previous state and the previously previous state
statePrevPrev = statePrev;
statePrev = state;

//calculates the image_speed based on the character's velocity
if (state == RUNNING or state == DUCKING or state == LOOKING_UP)
{
    if (state == RUNNING or state == LOOKING_UP) image_speed = abs(xVel) * runAnimSpeed + 0.1;
}

if (state == CLIMBING) image_speed = sqrt(sqr(abs(xVel))+sqr(abs(yVel))) * climbAnimSpeed
if (xVel >= 4 or xVel <= -4)
{
    image_speed = 1;
    if (platformCharacterIs(ON_GROUND)) setCollisionBounds(-8, -8, 8, 8);
    else setCollisionBounds(-5, -8, 5, 8);
}
else setCollisionBounds(-5, -8, 5, 8);
if (whipping) image_speed = 1;
if (state == DUCKTOHANG)
{
    image_index = 0;
    image_speed = 0.8;
}
//limit the image_speed at 1 so the animation always looks good
if (image_speed > 1) image_speed = 1;
