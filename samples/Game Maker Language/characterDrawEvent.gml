// Originally from /spelunky/Scripts/Platform Engine/characterDrawEvent.gml in the Spelunky Community Update Project

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
This event should be placed in the draw event of the platform character.
*/
//draws the sprite
draw = true;
if (facing == RIGHT) image_xscale = -1;
else image_xscale = 1;

if (blinkToggle != 1)
{
    if ((state == CLIMBING or (sprite_index == sPExit or sprite_index == sDamselExit or sprite_index == sTunnelExit)) and global.hasJetpack and not whipping)
    {
        draw_sprite_ext(sprite_index, -1, x, y, image_xscale, image_yscale, image_angle, image_blend, image_alpha);
        //draw_sprite(sprite_index,-1,x,y);
        draw_sprite(sJetpackBack,-1,x,y);
        draw = false;
    }
    else if (global.hasJetpack and facing == RIGHT) draw_sprite(sJetpackRight,-1,x-4,y-1);
    else if (global.hasJetpack) draw_sprite(sJetpackLeft,-1,x+4,y-1);
    if (draw)
    {
        if (redColor > 0) draw_sprite_ext(sprite_index, -1, x, y, image_xscale, image_yscale, image_angle, make_color_rgb(200 + redColor,0,0), image_alpha);
        else draw_sprite_ext(sprite_index, -1, x, y, image_xscale, image_yscale, image_angle, image_blend, image_alpha);
    }
    if (facing == RIGHT)
    {
        if (holdArrow == ARROW_NORM)
        {
            draw_sprite(sArrowRight, -1, x+4, y+1);
        }
        else if (holdArrow == ARROW_BOMB)
        {
            if (holdArrowToggle) draw_sprite(sBombArrowRight, 0, x+4, y+2);
            else draw_sprite(sBombArrowRight, 1, x+4, y+2);
        }
    }
    else if (facing == LEFT)
    {
        if (holdArrow == ARROW_NORM)
        {
            draw_sprite(sArrowLeft, -1, x-4, y+1);
        }
        else if (holdArrow == ARROW_BOMB)
        {
            if (holdArrowToggle) draw_sprite(sBombArrowLeft, 0, x-4, y+2);
            else draw_sprite(sBombArrowLeft, 1, x-4, y+2);
        }
    }
}
/*
if canRun
{
  xOffset=80
  if player=1
    yOffset=120
  else
    yOffset=143
  //draw the "flySpeed" bar, which shows how much speed the character has acquired while holding the "run" button
  //draw_healthbar(view_xview[0]+224+xOffset,view_yview[0]+432+yOffset,view_xview[0]+400+xOffset,view_yview[0]+450+yOffset,flySpeed,make_color_rgb(0,64,128),c_blue,c_aqua,0,1,1)
}
*/
