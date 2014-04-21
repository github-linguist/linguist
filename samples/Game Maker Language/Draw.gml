/*
    Originally from /Source/gg2/Objects/InGameElements/Character.events/Draw.xml in Gang Garrison 2

    Copyright (C) 2008-2013 Faucet Software
    http://www.ganggarrison.com

    This program is free software; 
    you can redistribute it and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3 of the License, or (at your option)
    any later version.
    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
    See the GNU General Public License for more details.
    You should have received a copy of the GNU General Public License along with this program; if not,
    see <http://www.gnu.org/licenses>.

    Additional permission under GNU GPL version 3 section 7
    If you modify this Program, or any covered work, by linking or combining it with the Game Maker runtime library, 
    the 39dll library/extension, Hobbel's Download Manager DLL, or modified versions of these libraries,
    the licensors of this Program grant you additional permission to convey the resulting work.
*/

xoffset = view_xview[0];
yoffset = view_yview[0];
xsize = view_wview[0];
ysize = view_hview[0];

if (distance_to_point(xoffset+xsize/2,yoffset+ysize/2) > 800)
    exit;

var xr, yr;
xr = round(x);
yr = round(y);
    
image_alpha = cloakAlpha;

if (global.myself.team == team and canCloak)
    image_alpha = cloakAlpha/2 + 0.5;

if (invisible)
    exit;

if(stabbing)
    image_alpha -= power(currentWeapon.stab.alpha, 2);

if team == global.myself.team && (player != global.myself || global.showHealthBar == 1){
    draw_set_alpha(1);
    draw_healthbar(xr-10, yr-30, xr+10, yr-25,hp*100/maxHp,c_black,c_red,c_green,0,true,true);
}    
if(distance_to_point(mouse_x, mouse_y)<25) {
    if cloak && team!=global.myself.team exit;
    draw_set_alpha(1);
    draw_set_halign(fa_center);
    draw_set_valign(fa_bottom);
    if(team==TEAM_RED) {
        draw_set_color(c_red);
    } else {
        draw_set_color(c_blue);
    }
    draw_text(xr, yr-35, player.name);
    
    if(team == global.myself.team && global.showTeammateStats)
    {
        if(weapons[0] == Medigun)
            draw_text(xr,yr+50, "Superburst: " + string(currentWeapon.uberCharge/20) + "%");
        else if(weapons[0] == Shotgun)
            draw_text(xr,yr+50, "Nuts 'N' Bolts: " + string(nutsNBolts));
        else if(weapons[0] == Minegun)
            draw_text(xr,yr+50, "Lobbed Mines: " + string(currentWeapon.lobbed));
    }
}  

draw_set_alpha(1);
if team == TEAM_RED ubercolour = c_red;
if team == TEAM_BLUE ubercolour = c_blue;

var sprite, overlaySprite;
if zoomed
{
    if (team == TEAM_RED)
        sprite = SniperCrouchRedS;
    else
        sprite = SniperCrouchBlueS;
    overlaySprite = sniperCrouchOverlay;
}
else
{
    sprite = sprite_index;
    overlaySprite = overlay;
}
    
if (omnomnomnom)
{
    draw_sprite_ext_overlay(omnomnomnomSprite,omnomnomnomOverlay,omnomnomnomindex,xr,yr,image_xscale,image_yscale,image_angle,c_white,1);
    if (ubered)
        draw_sprite_ext_overlay(omnomnomnomSprite,omnomnomnomOverlay,omnomnomnomindex,xr,yr,image_xscale,image_yscale,image_angle,ubercolour,0.7);
}
else if (taunting)
{
    draw_sprite_ext_overlay(tauntsprite,tauntOverlay,tauntindex,xr,yr,image_xscale,image_yscale,image_angle,c_white,1);
    if (ubered)
        draw_sprite_ext_overlay(tauntsprite,tauntOverlay,tauntindex,xr,yr,image_xscale,image_yscale,image_angle,ubercolour,0.7);
}
else if (player.humiliated)
    draw_sprite_ext(humiliationPoses,floor(animationImage)+humiliationOffset,xr,yr,image_xscale,image_yscale,image_angle,c_white,image_alpha);
else if (!taunting)
{
    if (cloak)
    {
        if (!ubered)
            draw_sprite_ext(sprite,floor(animationImage+animationOffset),xr,yr,image_xscale,image_yscale,image_angle,c_white,image_alpha);
        else if (ubered)
        {
            draw_sprite_ext(sprite,floor(animationImage+animationOffset),xr,yr,image_xscale,image_yscale,image_angle,c_white,1);
            draw_sprite_ext(sprite,floor(animationImage+animationOffset),xr,yr,image_xscale,image_yscale,image_angle,ubercolour,0.7);
        }
    }
    else
    {
        if (!ubered)
            draw_sprite_ext_overlay(sprite,overlaySprite,floor(animationImage+animationOffset),xr,yr,image_xscale,image_yscale,image_angle,c_white,image_alpha);
        else if (ubered)
        {
            draw_sprite_ext_overlay(sprite,overlaySprite,floor(animationImage+animationOffset),xr,yr,image_xscale,image_yscale,image_angle,c_white,1);
            draw_sprite_ext_overlay(sprite,overlaySprite,floor(animationImage+animationOffset),xr,yr,image_xscale,image_yscale,image_angle,ubercolour,0.7);
        }
    }
}
if (burnDuration > 0 or burnIntensity > 0) {
    for(i = 0; i < numFlames * burnIntensity / maxIntensity; i += 1)
    {
        draw_sprite_ext(FlameS, alarm[5] + i + random(2), x + flameArray_x[i], y + flameArray_y[i], 1, 1, 0, c_white, burnDuration / maxDuration * 0.71 + 0.35);
    }  
}

// Copied from Lorgan's itemserver "angels" with slight modifications
// All credit be upon him
if (demon != -1)
{
    demonX = median(x-40,demonX,x+40);
    demonY = median(y-40,demonY,y);
    demonOffset += demonDir;
    if (abs(demonOffset) > 15)
        demonDir *= -1;

    var dir;
    if (demonX > x)
        dir = -1;
    else
        dir = 1;

    if (demonFrame > sprite_get_number(demon))
        demonFrame = 0;

    if (stabbing || ubered)
        draw_sprite_ext(demon,demonFrame+floor(animationImage)+7*player.team,demonX,demonY+demonOffset,dir*1,1,0,c_white,1);
    else
        draw_sprite_ext(demon,demonFrame+floor(animationImage)+7*player.team,demonX,demonY+demonOffset,dir*1,1,0,c_white,image_alpha);

    demonFrame += 1;
}
