/**
The MIT License (MIT)

Copyright (c) 2014 Mateusz Cichon

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include <a_samp>

new OldVeh[MAX_PLAYERS];
new OldVehTime[MAX_PLAYERS];
new tmpVehId;
new warns[MAX_PLAYERS];

public OnPlayerUpdate(playerid){
  if(random(5)!=0 || !IsPlayerInAnyVehicle(playerid)) return 1;
  tmpVehId=GetPlayerVehicleID(playerid);
  if(OldVeh[playerid]!=tmpVehId && GetTickCount()-OldVehTime[playerid] < 700){
    new line[128];
    if(warns[playerid]==3){
      warns[playerid]=0;
      format(line,sizeof line,"Gracz %d (VEH %d) - ZBANOWANY za masowy spawn pojazdow",playerid,tmpVehId);
      CallRemoteFunction("NotifyAdmins","s",line);
      printf(line);
      BanEx(playerid,"Masowy spawn pojazdow");
      CallRemoteFunction("RespawnVehicles","d",1);
    }else{
      format(line,sizeof line,"Gracz %d (VEH %d) - Prawdopodobny masowy spawn pojazdow",playerid,tmpVehId);
      CallRemoteFunction("NotifyAdmins","s",line);
      printf(line);
      warns[playerid]++;
      CallRemoteFunction("RespawnVehicles","d",1);
    }
  }
  if(OldVeh[playerid]!=tmpVehId) OldVehTime[playerid]=GetTickCount();
  if(OldVeh[playerid]!=tmpVehId) OldVeh[playerid]=tmpVehId;
  return 1;
}
// EOF