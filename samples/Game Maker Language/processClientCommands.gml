/*
    Originally from /Source/gg2/Scripts/GameServer/processClientCommands.gml in Gang Garrison 2

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

var player, playerId, commandLimitRemaining;

player = argument0;
playerId = argument1;

// To prevent players from flooding the server, limit the number of commands to process per step and player.
commandLimitRemaining = 10;

with(player) {
    if(!variable_local_exists("commandReceiveState")) {
        // 0: waiting for command byte.
        // 1: waiting for command data length (1 byte)
        // 2: waiting for command data.
        commandReceiveState = 0;
        commandReceiveExpectedBytes = 1;
        commandReceiveCommand = 0;
    }
}

while(commandLimitRemaining > 0) {
    var socket;
    socket = player.socket;
    if(!tcp_receive(socket, player.commandReceiveExpectedBytes)) {
        return 0;
    }
    
    switch(player.commandReceiveState)
    {
    case 0:
        player.commandReceiveCommand = read_ubyte(socket);
        switch(commandBytes[player.commandReceiveCommand]) {
        case commandBytesInvalidCommand:
            // Invalid byte received. Wait for another command byte.
            break;
            
        case commandBytesPrefixLength1:
            player.commandReceiveState = 1;
            player.commandReceiveExpectedBytes = 1;
            break;

        case commandBytesPrefixLength2:
            player.commandReceiveState = 3;
            player.commandReceiveExpectedBytes = 2;
            break;

        default:
            player.commandReceiveState = 2;
            player.commandReceiveExpectedBytes = commandBytes[player.commandReceiveCommand];
            break;
        }
        break;
        
    case 1:
        player.commandReceiveState = 2;
        player.commandReceiveExpectedBytes = read_ubyte(socket);
        break;

    case 3:
        player.commandReceiveState = 2;
        player.commandReceiveExpectedBytes = read_ushort(socket);
        break;
        
    case 2:
        player.commandReceiveState = 0;
        player.commandReceiveExpectedBytes = 1;
        commandLimitRemaining -= 1;
        
        switch(player.commandReceiveCommand)
        {
        case PLAYER_LEAVE:
            socket_destroy(player.socket);
            player.socket = -1;
            break;
            
        case PLAYER_CHANGECLASS:
            var class;
            class = read_ubyte(socket);
            if(getCharacterObject(player.team, class) != -1)
            {
                if(player.object != -1)
                {
                    with(player.object)
                    {
                        if (collision_point(x,y,SpawnRoom,0,0) < 0)
                        {
                            if (!instance_exists(lastDamageDealer) || lastDamageDealer == player)
                            {
                                sendEventPlayerDeath(player, player, noone, BID_FAREWELL);
                                doEventPlayerDeath(player, player, noone, BID_FAREWELL);
                            }
                            else
                            {
                                var assistant;
                                assistant = secondToLastDamageDealer;
                                if (lastDamageDealer.object)
                                    if (lastDamageDealer.object.healer)
                                        assistant = lastDamageDealer.object.healer;
                                sendEventPlayerDeath(player, lastDamageDealer, assistant, FINISHED_OFF);
                                doEventPlayerDeath(player, lastDamageDealer, assistant, FINISHED_OFF);
                            }
                        }
                        else 
                        instance_destroy(); 
                        
                    }
                }
                else if(player.alarm[5]<=0)
                    player.alarm[5] = 1;
                class = checkClasslimits(player, player.team, class);
                player.class = class;
                ServerPlayerChangeclass(playerId, player.class, global.sendBuffer);
            }
            break;
            
        case PLAYER_CHANGETEAM:
            var newTeam, balance, redSuperiority;
            newTeam = read_ubyte(socket);
            
            redSuperiority = 0   //calculate which team is bigger
            with(Player)
            {
                if(team == TEAM_RED)
                    redSuperiority += 1;
                else if(team == TEAM_BLUE)
                    redSuperiority -= 1;
            }
            if(redSuperiority > 0)
                balance = TEAM_RED;
            else if(redSuperiority < 0)
                balance = TEAM_BLUE;
            else
                balance = -1;
            
            if(balance != newTeam)
            {
                if(getCharacterObject(newTeam, player.class) != -1 or newTeam==TEAM_SPECTATOR)
                {  
                    if(player.object != -1)
                    {
                        with(player.object)
                        {
                            if (!instance_exists(lastDamageDealer) || lastDamageDealer == player)
                            {
                                sendEventPlayerDeath(player, player, noone, BID_FAREWELL);
                                doEventPlayerDeath(player, player, noone, BID_FAREWELL);
                            }
                            else
                            {
                                var assistant;
                                assistant = secondToLastDamageDealer;
                                if (lastDamageDealer.object)
                                    if (lastDamageDealer.object.healer)
                                        assistant = lastDamageDealer.object.healer;
                                sendEventPlayerDeath(player, lastDamageDealer, assistant, FINISHED_OFF);
                                doEventPlayerDeath(player, lastDamageDealer, assistant, FINISHED_OFF);
                            }
                        }
                        player.alarm[5] = global.Server_Respawntime;
                    }
                    else if(player.alarm[5]<=0)
                        player.alarm[5] = 1;                    
                    var newClass;
                    newClass = checkClasslimits(player, newTeam, player.class);
                    if newClass != player.class
                    {
                        player.class = newClass;
                        ServerPlayerChangeclass(playerId, player.class, global.sendBuffer);
                    }
                    player.team = newTeam;
                    ServerPlayerChangeteam(playerId, player.team, global.sendBuffer);
                    ServerBalanceTeams();
                }
            }
            break;                   
            
        case CHAT_BUBBLE:
            var bubbleImage;
            bubbleImage = read_ubyte(socket);
            if(global.aFirst) {
                bubbleImage = 0;
            }
            write_ubyte(global.sendBuffer, CHAT_BUBBLE);
            write_ubyte(global.sendBuffer, playerId);
            write_ubyte(global.sendBuffer, bubbleImage);
            
            setChatBubble(player, bubbleImage);
            break;
            
        case BUILD_SENTRY:
            if(player.object != -1)
            {
                if(player.class == CLASS_ENGINEER
                        and collision_circle(player.object.x, player.object.y, 50, Sentry, false, true) < 0
                        and player.object.nutsNBolts == 100
                        and (collision_point(player.object.x,player.object.y,SpawnRoom,0,0) < 0)
                        and !player.sentry
                        and !player.object.onCabinet)
                {
                    write_ubyte(global.sendBuffer, BUILD_SENTRY);
                    write_ubyte(global.sendBuffer, playerId);
                    write_ushort(global.serializeBuffer, round(player.object.x*5));
                    write_ushort(global.serializeBuffer, round(player.object.y*5));
                    write_byte(global.serializeBuffer, player.object.image_xscale);
                    buildSentry(player, player.object.x, player.object.y, player.object.image_xscale);
                }
            }
            break;                                       

        case DESTROY_SENTRY:
            with(player.sentry)
                instance_destroy();
            break;                     
        
        case DROP_INTEL:
            if (player.object != -1)
            {
                if (player.object.intel)
                {
                    sendEventDropIntel(player);
                    doEventDropIntel(player);
                }
            }
            break;     
              
        case OMNOMNOMNOM:
            if(player.object != -1) {
                if(!player.humiliated
                    and !player.object.taunting
                    and !player.object.omnomnomnom
                    and player.object.canEat
                    and player.class==CLASS_HEAVY)
                {                            
                    write_ubyte(global.sendBuffer, OMNOMNOMNOM);
                    write_ubyte(global.sendBuffer, playerId);
                    with(player.object)
                    {
                        omnomnomnom = true;
                        if player.team == TEAM_RED {
                            omnomnomnomindex=0;
                            omnomnomnomend=31;
                        } else if player.team==TEAM_BLUE {
                            omnomnomnomindex=32;
                            omnomnomnomend=63;
                        } 
                        xscale=image_xscale;
                    }             
                }
            }
            break;
             
        case TOGGLE_ZOOM:
            if player.object != -1 {
                if player.class == CLASS_SNIPER {
                    write_ubyte(global.sendBuffer, TOGGLE_ZOOM);
                    write_ubyte(global.sendBuffer, playerId);
                    toggleZoom(player.object);
                }
            }
            break;
                                                      
        case PLAYER_CHANGENAME:
            var nameLength;
            nameLength = socket_receivebuffer_size(socket);
            if(nameLength > MAX_PLAYERNAME_LENGTH)
            {
                write_ubyte(player.socket, KICK);
                write_ubyte(player.socket, KICK_NAME);
                socket_destroy(player.socket);
                player.socket = -1;
            }
            else
            {
                with(player)
                {
                    if(variable_local_exists("lastNamechange")) 
                        if(current_time - lastNamechange < 1000)
                            break;
                    lastNamechange = current_time;
                    name = read_string(socket, nameLength);
                    if(string_count("#",name) > 0)
                    {
                        name = "I <3 Bacon";
                    }
                    write_ubyte(global.sendBuffer, PLAYER_CHANGENAME);
                    write_ubyte(global.sendBuffer, playerId);
                    write_ubyte(global.sendBuffer, string_length(name));
                    write_string(global.sendBuffer, name);
                }
            }
            break;
            
        case INPUTSTATE:
            if(player.object != -1)
            {
                with(player.object)
                {
                    keyState = read_ubyte(socket);
                    netAimDirection = read_ushort(socket);
                    aimDirection = netAimDirection*360/65536;
                    event_user(1);
                }
            }
            break;
        
        case REWARD_REQUEST:
            player.rewardId = read_string(socket, socket_receivebuffer_size(socket));
            player.challenge = rewardCreateChallenge();
            
            write_ubyte(socket, REWARD_CHALLENGE_CODE);
            write_binstring(socket, player.challenge);
            break;
            
        case REWARD_CHALLENGE_RESPONSE:
            var answer, i, authbuffer;
            answer = read_binstring(socket, 16);
            
            with(player)
                if(variable_local_exists("challenge") and variable_local_exists("rewardId"))
                    rewardAuthStart(player, answer, challenge, true, rewardId);
           
            break;

        case PLUGIN_PACKET:
            var packetID, buf, success;

            packetID = read_ubyte(socket);
            
            // get packet data
            buf = buffer_create();
            write_buffer_part(buf, socket, socket_receivebuffer_size(socket));

            // try to enqueue
            success = _PluginPacketPush(packetID, buf, player);
            
            // if it returned false, packetID was invalid
            if (!success)
            {
                // clear up buffer
                buffer_destroy(buf);

                // kick player
                write_ubyte(player.socket, KICK);
                write_ubyte(player.socket, KICK_BAD_PLUGIN_PACKET);
                socket_destroy(player.socket);
                player.socket = -1;
            }
            break;
            
        case CLIENT_SETTINGS:
            var mirror;
            mirror = read_ubyte(player.socket);
            player.queueJump = mirror;
            
            write_ubyte(global.sendBuffer, CLIENT_SETTINGS);
            write_ubyte(global.sendBuffer, playerId);
            write_ubyte(global.sendBuffer, mirror);
            break;
        
        }
        break;
    } 
}
