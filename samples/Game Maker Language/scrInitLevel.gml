// Originally from /spelunky/Scripts/Level Generation/scrInitLevel.gml in the Spelunky Community Update Project

//
// scrInitLevel()
//
// Calls scrLevelGen(), scrRoomGen*(), and scrEntityGen() to build level.
//

/**********************************************************************************
    Copyright (c) 2008, 2009 Derek Yu and Mossmouth, LLC
    
    This file is part of Spelunky.

    You can redistribute and/or modify Spelunky, including its source code, under
    the terms of the Spelunky User License.

    Spelunky is distributed in the hope that it will be entertaining and useful,
    but WITHOUT WARRANTY.  Please see the Spelunky User License for more details.

    The Spelunky User License should be available in "Game Information", which
    can be found in the Resource Explorer, or as an external file called COPYING.
    If not, please obtain a new copy of Spelunky from <http://spelunkyworld.com/>
    
***********************************************************************************/

global.levelType = 0;
//global.currLevel = 16;
if (global.currLevel > 4 and global.currLevel < 9) global.levelType = 1;
if (global.currLevel > 8 and global.currLevel < 13) global.levelType = 2;
if (global.currLevel > 12 and global.currLevel < 16) global.levelType = 3;
if (global.currLevel == 16) global.levelType = 4;

if (global.currLevel <= 1 or
    global.currLevel == 5 or
    global.currLevel == 9 or
    global.currLevel == 13)
{
    global.hadDarkLevel = false;
}

// global.levelType = 3; // debug

// DEBUG MODE //
/*
if (global.currLevel == 2) global.levelType = 4;
if (global.currLevel == 3) global.levelType = 2;
if (global.currLevel == 4) global.levelType = 3;
if (global.currLevel == 5) global.levelType = 4;
*/

// global.levelType = 0;

global.startRoomX = 0;
global.startRoomY = 0;
global.endRoomX = 0;
global.endRoomY = 0;
oGame.levelGen = false;

// this is used to determine the path to the exit (generally no bombs required)
for (i = 0; i < 4; i += 1)
{
    for (j = 0; j < 4; j += 1)
    {
        global.roomPath[i,j] = 0;
    }
}

// side walls
if (global.levelType == 4)
    k = 54;
else if (global.levelType == 2)
    k = 38;
else if (global.lake)
    k = 41;
else
    k = 33;
for (i = 0; i <= 42; i += 1)
{
    for (j = 0; j <= k; j += 1)
    {
        if (not isLevel())
        {
            i = 999;
            j = 999;
        }
        else if (global.levelType == 2)
        {
            if (i*16 == 0 or
                i*16 == 656 or
                j*16 == 0)
            {
                obj = instance_create(i*16, j*16, oDark);
                obj.invincible = true;
                obj.sprite_index = sDark;
            }
        }
        else if (global.levelType == 4)
        {
            if (i*16 == 0 or
                i*16 == 656 or
                j*16 == 0)
            {
                obj = instance_create(i*16, j*16, oTemple);
                obj.invincible = true;
                if (not global.cityOfGold) obj.sprite_index = sTemple;
            }
        }
        else if (global.lake)
        {
            if (i*16 == 0 or
                i*16 == 656 or
                j*16 == 0 or
                j*16 >= 656)
            {
                obj = instance_create(i*16, j*16, oLush); obj.sprite_index = sLush;
                obj.invincible = true;
            }
        }
        else if (i*16 == 0 or
            i*16 == 656 or
            j*16 == 0 or
            j*16 >= 528)
        {
            if (global.levelType == 0) { obj = instance_create(i*16, j*16, oBrick); obj.sprite_index = sBrick; }
            else if (global.levelType == 1) { obj = instance_create(i*16, j*16, oLush); obj.sprite_index = sLush; }
            else { obj = instance_create(i*16, j*16, oTemple); if (not global.cityOfGold) obj.sprite_index = sTemple; }
            obj.invincible = true;
        }
    }
}

if (global.levelType == 2)
{
    for (i = 0; i <= 42; i += 1)
    {
        instance_create(i*16, 40*16, oDark);
        //instance_create(i*16, 35*16, oSpikes);
    }
}

if (global.levelType == 3)
{
    background_index = bgTemple;
}

global.temp1 = global.gameStart;
scrLevelGen();

global.cemetary = false;
if (global.levelType == 1 and rand(1,global.probCemetary) == 1) global.cemetary = true;

with oRoom
{
    if (global.levelType == 0) scrRoomGen();
    else if (global.levelType == 1)
    {
        if (global.blackMarket) scrRoomGenMarket();
        else scrRoomGen2();
    }
    else if (global.levelType == 2)
    {
        if (global.yetiLair) scrRoomGenYeti();
        else scrRoomGen3();
    }
    else if (global.levelType == 3) scrRoomGen4();
    else scrRoomGen5();
}

global.darkLevel = false;
//if (not global.hadDarkLevel and global.currLevel != 0 and global.levelType != 2 and global.currLevel != 16 and rand(1,1) == 1)
if (not global.hadDarkLevel and not global.noDarkLevel and global.currLevel != 0 and global.currLevel != 1 and global.levelType != 2 and global.currLevel != 16 and rand(1,global.probDarkLevel) == 1)
{
    global.darkLevel = true;
    global.hadDarkLevel = true;
    //instance_create(oPlayer1.x, oPlayer1.y, oFlare);
}

if (global.blackMarket) global.darkLevel = false;

global.genUdjatEye = false;
if (not global.madeUdjatEye)
{
    if (global.currLevel == 2 and rand(1,3) == 1) global.genUdjatEye = true;
    else if (global.currLevel == 3 and rand(1,2) == 1) global.genUdjatEye = true;
    else if (global.currLevel == 4) global.genUdjatEye = true;
}

global.genMarketEntrance = false;
if (not global.madeMarketEntrance)
{
    if (global.currLevel == 5 and rand(1,3) == 1) global.genMarketEntrance = true;
    else if (global.currLevel == 6 and rand(1,2) == 1) global.genMarketEntrance = true;
    else if (global.currLevel == 7) global.genMarketEntrance = true;
}

////////////////////////////
// ENTITY / TREASURES
////////////////////////////
global.temp2 = global.gameStart;
if (not isRoom("rTutorial") and not isRoom("rLoadLevel")) scrEntityGen();

if (instance_exists(oEntrance) and not global.customLevel)
{
    oPlayer1.x = oEntrance.x+8;
    oPlayer1.y = oEntrance.y+8;
}

if (global.darkLevel or
    global.blackMarket or
    global.snakePit or
    global.cemetary or
    global.lake or
    global.yetiLair or
    global.alienCraft or
    global.sacrificePit or
    global.cityOfGold)
{
    if (not isRoom("rLoadLevel"))
    {
        with oPlayer1 { alarm[0] = 10; }
    }
}

if (global.levelType == 4) scrSetupWalls(864);
else if (global.lake) scrSetupWalls(656);
else scrSetupWalls(528);

// add background details
if (global.graphicsHigh)
{
    repeat(20)
    {
        // bg = instance_create(16*rand(1,42), 16*rand(1,33), oCaveBG);
        if (global.levelType == 1 and rand(1,3) < 3)
            tile_add(bgExtrasLush, 32*rand(0,1), 0, 32, 32, 16*rand(1,42), 16*rand(1,33), 10002);
        else if (global.levelType == 2 and rand(1,3) < 3)
            tile_add(bgExtrasIce, 32*rand(0,1), 0, 32, 32, 16*rand(1,42), 16*rand(1,33), 10002);
        else if (global.levelType == 3 and rand(1,3) < 3)
            tile_add(bgExtrasTemple, 32*rand(0,1), 0, 32, 32, 16*rand(1,42), 16*rand(1,33), 10002);
        else
            tile_add(bgExtras, 32*rand(0,1), 0, 32, 32, 16*rand(1,42), 16*rand(1,33), 10002);
    }
}

oGame.levelGen = true;

// generate angry shopkeeper at exit if murderer or thief
if ((global.murderer or global.thiefLevel > 0) and isRealLevel())
{
    with oExit
    {
        if (type == "Exit")
        {
            obj = instance_create(x, y, oShopkeeper);
            obj.status = 4;
        }
    }
    // global.thiefLevel -= 1;
}

with oTreasure
{
    if (collision_point(x, y, oSolid, 0, 0))
    {
        obj = instance_place(x, y, oSolid);
        if (obj.invincible) instance_destroy();
    }
}

with oWater
{
    if (sprite_index == sWaterTop or sprite_index == sLavaTop)
    {
        scrCheckWaterTop();
    }
    /*
        obj = instance_place(x-16, y, oWater);
        if (instance_exists(obj))
        {
            if (obj.sprite_index == sWaterTop or obj.sprite_index == sLavaTop)
            {
                if (type == "Lava") sprite_index = sLavaTop;
                else sprite_index = sWaterTop;
            }
        }
        obj = instance_place(x+16, y, oWater);
        if (instance_exists(obj))
        {
            if (obj.sprite_index == sWaterTop or obj.sprite_index == sLavaTop)
            {
                if (type == "Lava") sprite_index = sLavaTop;
                else sprite_index = sWaterTop;
            }
        }
    */
}

global.temp3 = global.gameStart;
