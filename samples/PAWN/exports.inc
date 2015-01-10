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

forward GetPlayerFactionID(playerid);
forward IsPlayeRVIP(playerid);
forward GetFSAdminLeveL(playerid);
forward NotifyAdmins(msg[]);
forward Odliczanie(val,zasieg,isFreeze,owner);
forward MarkVehicleAsUsed(vid);
forward GetPlayerAccountID(playerid);
forward GivePlayerScoreEx(playerid,amount);
forward GetPlayerScoreFS(playerid);
forward MsgExport(playerid,color,msg[]);
forward ShowAnnouncementExport(params[]);
forward GetPlayerOnlineHoursBySession(playerid);
forward GetPlayerOnlineMinutesBySession(playerid);
forward IsPlayerCountedAndNotExpired(playerid);

public IsPlayerCountedAndNotExpired(playerid)
{
  if(pData[playerid][isCountedForRestore] && pData[playerid][disconnectSession] + 300000 >= GetTickCount())
  {
    new serial[50],IP[16],PlayerName[32];
    gpci(playerid, serial, sizeof serial);
    GetPlayerIp(playerid,IP,sizeof(IP));
    GetPlayerName(playerid,PlayerName,sizeof(PlayerName));
    
    if(strcmp(pData[playerid][lastNick],PlayerName,false)==0 && strcmp(pData[playerid][lastIp],IP,false)==0 && strcmp(pData[playerid][lastSerial],serial,false)==0){
      return 1;
    }
  }
  return 0;
}

public GetPlayerFactionID(playerid){
    if (!IsPlayerConnected(playerid)) return 0;
    return pTemp[playerid][faction];
}

public GetPlayerAccountID(playerid){
  if (!IsPlayerConnected(playerid)) return 0;
  if (!pData[playerid][loggedIn]) return 0;
  return pData[playerid][accountID];
}

public GetFSAdminLeveL(playerid) {
  if (!pData[playerid][loggedIn] || !pData[playerid][accountID]) return 0;
  else return pData[playerid][adminLevel];
}

public IsPlayeRVIP(playerid) {
  return pData[playerid][vipEnabled];
}

public NotifyAdmins(msg[])
  MSGToAdmins(COLOR_ERROR, msg, false);

public MsgExport(playerid,color,msg[]){
  if(color==1) color=COLOR_ERROR;
  else if(color==2) color=COLOR_INFO;
  else if(color==3) color=COLOR_INFO2;
    return Msg(playerid,color,msg);
}

public GivePlayerScoreEx(playerid,amount)
  GivePlayerScore(playerid,amount,"Zewnetrzny skrypt", true);

public GetPlayerScoreFS(playerid)
  return pData[playerid][respect];

public Odliczanie(val,zasieg,isFreeze,owner){
  if (zasieg>100 || zasieg<0) return;
  new
    str[15],
    pVector2[e_Vectors],
    int,vw;

  if (val>0)
    format(str,sizeof str, "_~n~_~n~%d", val);
  else
    format(str,sizeof str, "_~n~_~n~~r~GO");

  foreach(i)
    if(pData[i][pAttraction]==A_NONE && (pData[i][hudSetting][HUD_ANNOUNCEMENTS] || IsGM(owner))) {
        if(zasieg==0){
            GameTextForPlayer(i,str,1000,6);
        PlayerPlaySound(i, val>0?1056:1057, val*val, val*val, val*val);
      }else{
        int=GetPlayerInterior(owner);
        vw=GetPlayerVirtualWorld(owner);
        if (IsPlayerSpawned(i) && GetPlayerInterior(i)==int && GetPlayerVirtualWorld(i)==vw){
          GetPlayerPos(i, pVector2[X], pVector2[Y], pVector2[Z]);
          if(GetPlayerDistanceFromPoint(owner, pVector2[X], pVector2[Y], pVector2[Z]) <= zasieg){
            GameTextForPlayer(i,str,1000,6);
            PlayerPlaySound(i, val>0?1056:1057, val*val, val*val, val*val);
            if(isFreeze){
              if(val>0) TogglePlayerControllable(i, false);
              if(val<=0) TogglePlayerControllable(i, true);
            }
          }
        }
      }
    }

  if (val>0)
    SetTimerEx("Odliczanie", 900, false, "dddd", val-1,zasieg,isFreeze,owner);

  return;
}

public MarkVehicleAsUsed(vid){
  tVehicleUsed[vid]=true;
  tVehicles[vid][vo_used]=true;
}
// EOF
