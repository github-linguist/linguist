ON 1:START:{ 
  unset %tc.* | unset %tcchain.* | unset %revive*
  tc.cfgcheck | tc.init
} 

ON 1:CONNECT:{ ; Start crons that require the bot to be connected
  tc.refilltimer 
}

ON 1:DISCONNECT:{ ; End crons that require the bot to be connected
  .timer0.refill* off 
}

;    NOTE: DO NOT CHANGE VERSION NUMBER.
; This is used in several subs to keep data file contents in sync and let the script know when it needs to trigger updates.
alias -l tc.botvers { return 9.4 }  

; Ched breaks these a lot. Getting tired of code digging to fix... this keeps in one place. 
; $1 = ID. No checks are done in these since they exist where it's being called.
alias -l tc.revlink { return https://www.torn.com/profiles.php?XID= $+ $1 }
alias -l tc.bountylink { return https://www.torn.com/bounties.php#/p=add&XID= $+ $1 }
alias -l tc.atklink { return https://www.torn.com/loader2.php?sid=getInAttack&user2ID= $+ $1 }
alias -l tc.bustlink { return https://www.torn.com/profiles.php?XID= $+ $1 }
alias -l tc.baillink { return https://www.torn.com/profiles.php?XID= $+ $1 }
alias -l tc.complink { return https://www.torn.com/joblist.php#/p=corp&ID= $+ $1 }
alias -l tc.proflink { return https://www.torn.com/profiles.php?XID= $+ $1 }
alias -l tc.bazaarlink { return https://www.torn.com/bazaar.php#/p=bazaar&userID= $+ $1 }
alias -l tc.pstatslink { return https://www.torn.com/personalstats.php?ID= $+ $1 }
alias -l tc.dcaselink { return https://www.torn.com/displaycase.php?userID= $+ $1 }
alias -l tc.tradelink { return https://www.torn.com/trade.php#step=start&userID= $+ $1 }
alias -l tc.cashlink { return https://www.torn.com/sendcash.php#/XID= $+ $1 }
alias -l tc.maillink { return https://www.torn.com/messages.php#/p=compose&XID= $+ $1 }
alias -l tc.flistlink { return https://www.torn.com/friendlist.php#/p=add&XID= $+ $1 }
alias -l tc.blistlink { return https://www.torn.com/blacklist.php#/p=add&XID= $+ $1 }

;  File location "constants". Doing it this way means no need to store as variables. Also fixes a lot of weird bugs with empty file variables.
alias -l tc.cfgfile { return $qt($scriptdir $+ config.ini) }
alias -l tc.admfile { return $qt($scriptdir $+ adm.hsh) }
alias -l tc.crimesfile { return $qt($scriptdir $+ crimes.db) }
alias -l tc.quotefile { return $qt($scriptdir $+ quote.db) }
alias -l tc.spyfile { return $qt($scriptdir $+ spy.db) }
alias -l tc.psetfile { return $qt($scriptdir $+ pset.ini) }
alias -l tc.idsfile { return $qt($scriptdir $+ id.hsh) }
alias -l tc.idsnamefile { return $qt($scriptdir $+ idnames.hsh) }

; Per-ID file/folder locations. For files: $1 = ID is required. Folder does not use $qt because it breaks some functions.
alias -l tc.statsdir { return $scriptdir $+ stats\ }
alias -l tc.statsfile { return $qt($tc.statsdir $+ $1 $+ .txt) } 
alias -l tc.drugsdir { return $scriptdir $+ drugs\ }
alias -l tc.drugsfile { return $qt($tc.drugsdir $+ $1 $+ .txt) } 


; chain log file locations. Folder does not use $qt because it breaks some functions.
alias -l tc.chlogdir { return $scriptdir $+ chaindat\ }
alias -l tc.chlogfile { return $qt($tc.chlogdir $+ chain $+ $1 $+ .hsh) }
; chainmonth tally files. $1 = year, $2 = month. If none is specified, uses current.
alias -l tc.cmlogfile { 
  if ($1 != $null) { var %yr = $1 } | else { var %yr = $asctime($tc.time,yyyy) }
  if ($2 != $null) { var %mn = $2 } | else { var %mn = $asctime($tc.time,mm) }
  return $qt($tc.chlogdir $+ %yr $+ $chr(45) $+ %mn $+ .hsh) 
}

; quick and dirty 'constants' - too lazy to make new options in admin panel for these atm. 
; Chain wartally reset time.
alias -l tc.wtreset { return $calc(60 * 60 * 24 * 14) }
; Timer maximum. Default = 3 days in seconds.
alias -l tc.timemax { return $calc(60 * 60 * 24 * 3) }   
; Global Cooldown for spammy scripts. Default = 5 seconds for normal. 3 seconds for short
alias -l tc.gcd { return 5 } 
alias -l tc.sgcd { return 3 } 

alias -l tc.init {
  if ($readini($tc.cfgfile,n,File,vers) != $tc.botvers) { tc.upgrade }
  if ($server != $null) { tc.refilltimer }
  if (!$exists($qt($tc.chlogdir))) { mkdir $qt($tc.chlogdir) }
  if (!$exists($qt($tc.statsdir))) { mkdir $qt($tc.statsdir) }
  if (!$exists($qt($tc.drugsdir))) { mkdir $qt($tc.drugsdir) }
  if ($hget(tc.adm)) { hfree tc.adm } | hmake tc.adm 100
  if (!$exists($tc.admfile)) { echo -sg 4Note: Noone on admin list! The bot will still work but no-one can access admin commands until you add someone. To do so, right click their name in the nick list. }
  else { hload tc.adm $tc.admfile }
  if ($hget(tc.ids)) { hfree tc.ids } | hmake tc.ids 100 | if ($exists($tc.idsfile)) { hload tc.ids $tc.idsfile }  
  if ($hget(tc.idsname)) { hfree tc.idsname } | hmake tc.idsname 100 | if ($exists($tc.idsnamefile)) { hload tc.idsname $tc.idsnamefile }  
  set %tc.chan $readini($tc.cfgfile,n,Config,chan) 
  set %tc.chchan $readini($tc.cfgfile,n,Chain,chan)
  set %tc.statsaving $readini($tc.cfgfile,n,Config,statsaving)
  set %tc.col $readini($tc.cfgfile,n,Color,color) $readini($tc.cfgfile,n,Color,pri) $readini($tc.cfgfile,n,Color,sec) $readini($tc.cfgfile,n,Color,help) $readini($tc.cfgfile,n,Color,note) $readini($tc.cfgfile,n,Color,error) $readini($tc.cfgfile,n,Color,good) $readini($tc.cfgfile,n,Color,bad) $readini($tc.cfgfile,n,Color,subnote)  
  echo -sg 10Universal TC Assistant made by:7 PMV 10[07157799310] v07 $+ $tc.botvers 10- Initialized Successfully!
}

; --------------------------------- crons ---------------------------------

; Refill timer cron. At 12:00AM TC time every day.
alias -l tc.refillcron { return $asctime($calc($ctime($date 00:00) + ($ctime - $tc.time)),HH:nn) }
alias -l tc.refilltimer { 
  .timer0.refill* off
  if ($1 == post && $readini($tc.cfgfile,n,Config,ereset) == yes) { 
    var %tcrefill.ptr = 1, %tcrefill.loop = $numtok(%tc.chan,44)
    while (%tcrefill.ptr <= %tcrefill.loop) { 
      if ($gettok(%tc.chan,%tcrefill.ptr,44) ischan) { msg $gettok(%tc.chan,%tcrefill.ptr,44) $tc.col(4) $+ Note: Energy and nerve refills have reset! }
      else { echo -s 4Note: $gettok(%tc.chan,%tcrefill.ptr,44) refill notice skipped, bot not in chan. }
      inc %tcrefill.ptr 
    }
  }
  .timer0.refillnew 1 70 .timer0.refill $tc.refillcron 1 1 tc.refilltimer post
}

; Quick debug
alias tc.showcron {
  echo -s Refill cron on: $tc.refillcron Local Time - Duration: $duration($calc($ctime($asctime($calc($tc.time + 86400),dd/mm/yy) 00:05) - $tc.time))
  echo -s Decay cron on: $tc.decaycron Local Time - Duration: $duration($calc($ctime($asctime($calc($tc.time + 86400),dd/mm/yy) 04:20) - $tc.time))
}

; --------------------------------- timer encode ---------------------------------

; found this from: http://en.wikichip.org/wiki/MSL_Injection_-_mIRC 
; seems easier then using eval,0 on everything.
alias -l s { return $!decode( $encode($1-, m) ,m) }

; --------------------------------- admin control panel ---------------------------------

dialog -l tc.cfgview { 
  title "Universal TC Assistant Config" 
  size -1 -1 305 175 
  option dbu 
  tab "Channels",1,1 1 300 160
  tab "Admin Users",2
  tab "Faction",3
  tab "Access",4
  tab "Customize",5
  tab "Misc",6
  tab "API",7
  text "",9,4 165 302 10, center
  text "Main Channel(s):",10,4 30 85 10, right nowrap tab 1
  edit %tc.chan,101,90 28 200 10, autohs tab 1
  text "Chain Channel(s):",1001,4 44 85 10, right nowrap tab 1
  text "Revive Channel(s):",1002,4 58 85 10, right nowrap tab 1
  text "Note: to add multiple channels, seperate by commas. Spaces will be removed.",1003,60 18 200 10, tab 1
  edit %tc.chchan,14,90 42 200 10, autohs tab 1
  edit "",19,90 56 200 10, autohs tab 1
  button "Reset",12,100 98 25 10,tab 1
  button "Save",13,130 98 25 10,tab 1
  list 21,4 18 250 144, sort vsbar tab 2
  button "Delete",22,258 18 40 10,tab 2
  text "Note: you can add someone by right clicking their name in nick list. If the panel to the left is empty, noone is on the admin list.",1004,258 32 40 60, tab 2
  text "Faction Name:",1007,6 22 35 10, nowrap tab 3
  edit "",41,43 20 90 10, tab 3
  button "Set",42,135 20 15 10,tab 3
  text "Chaining Upgrade:",1016,180 22 50 10, nowrap tab 3
  edit "",43,227 20 20 10, tab 3
  button "Set",44,250 24 15 15,tab 3
  text "Hermetic Upgrade:",1017,180 34 50 10, nowrap tab 3
  edit "",45,227 32 20 10, tab 3
  text "Access Permissions:",1005,16 20 100 10, tab 4
  button "?",50,4 18 10 10, tab 4
  check "!delid - Delete IDs",51,4 30 100 10, 3state tab 4
  check "!delspy - Delete Spies",52,4 42 100 10, 3state tab 4
  check "!delquote - Delete Quotes",53,4 54 100 10, 3state tab 4
  check "!wartally/!warresp reset - Reset the war tally and respect markers",54,4 66 180 10, 3state tab 4
  check "!setbday - Set another person's birthday",55,4 78 180 10, 3state tab 4
  text "Minimum access required to use bot:",1006,210 20 90 10, tab 4
  radio "@ (Operator)",56,230 30 60 10, tab 4
  radio "% (Half-Operator)",57,230 42 60 10, tab 4
  radio "+ (Voiced)",58,230 54 60 10, tab 4
  radio "No access level",59,230 66 60 10, tab 4
  check "Enable colors",61,4 18 40 10, tab 5
  text "Primary Colors",1061, 104 18 80 8, tab 5
  edit "",2061, 80 16 20 10, tab 5
  text "Secondary Colors",1062, 104 30 80 8, tab 5
  edit "",2062, 80 28 20 10, tab 5
  text "Help",1063, 104 42 80 8, tab 5
  edit "",2063, 80 40 20 10, tab 5
  text "Error Messages",1065, 104 54 80 8, tab 5
  edit "",2065, 80 52 20 10, tab 5
  text "Good",1066, 104 78 80 8, tab 5
  edit "",2066, 80 76 20 10, tab 5
  text "Bad",1067, 104 90 80 8, tab 5
  edit "",2067, 80 88 20 10, tab 5
  text "Notes",1064, 104 66 80 8, tab 5
  edit "",2064, 80 64 20 10, tab 5
  text "Subnotes",1068, 104 102 80 8, tab 5
  edit "",2068, 80 100 20 10, tab 5
  text "",2161, 200 18 45 120, tab 5
  button "Reset",64,18 44 30 10, tab 5
  button "Save",65,18 58 30 10, tab 5
  button "Default",66,18 72 30 10, tab 5
  check "Energy Reset Notice",71,4 18 90 10, tab 6
  check "Revive string recorded Notice",72,4 30 90 10, tab 6
  check "Require 'Battle Stats' line before updating stats.",75,4 42 150 10, tab 6
  check "Send chain starting/ending notices to other chain channels.",79,4 54 150 10, tab 6
  check "Each channel has seperate quote database",171,4 66 150 10, tab 6
  check "Enable spy tracker (disable if using another spy storage script)",172,4 90 200 10, tab 6
  check "Always add ID to bot with !find",173,4 78 100 10, tab 6
  check "Additional 'Please revive' line on xanax overdose",174,4 102 200 10, tab 6
  check "Enable !banker and !setbanker command",176,4 114 200 10, tab 6
  check "Enable battle stat saving",177,4 126 200 10, tab 6
  check "Enable API usage",270,4 18 90 10, tab 7
  text "API key (used for !info):",260,105 20 65 10, right nowrap tab 7
  edit "",261,173 18 50 10, autohs tab 7
  button "Set",262,226 18 20 10,tab 7
  text "To find your API key, in Torn, go to 'Settings' (gear icon, top right) - 'API Key'.",258,6 130 290 20, center tab 7
  text "All API keys that are provided is stored locally with the bot's settings. Unchecking 'Enable API usage' will disable the bot's ability to use the API.",259,6 140 290 20, tab 7
}

dialog -l tc.pophelp { 
  title "Quick Help" 
  size -l -l 150 100
  option dbu 
  text "",1,1 1 150 100
}

on 1:DIALOG:tc.cfgview:sclick:12:{ 
  did -r tc.cfgview 101 | did -a tc.cfgview 101 $readini($tc.cfgfile,n,Config,chan)
  did -r tc.cfgview 14 | did -a tc.cfgview 14 $readini($tc.cfgfile,n,Chain,chan)
  did -r tc.cfgview 19 | did -a tc.cfgview 19 $readini($tc.cfgfile,n,Chain,revive)
  did -a tc.cfgview 9 Channels lists reset to last saved setting.
}
on 1:DIALOG:tc.cfgview:sclick:13:{   
  if ($did(tc.cfgview,101).text == $null || $did(tc.cfgview,14).text == $null) { did -a tc.cfgview 9 You need at least one entry in both Main Channel and Chaining Channels for the bot to work. }
  else { 
    set %tc.chan $remove($did(tc.cfgview,101).text,$chr(32)) | writeini -n $tc.cfgfile Config chan %tc.chan
    set %tc.chchan $remove($did(tc.cfgview,14).text,$chr(32)) | writeini -n $tc.cfgfile Chain chan %tc.chchan
    if (%tcchain.stchan != $null) { set %tcchain.revchan $did(tc.cfgview,19).text }
    if ($did(tc.cfgview,19).text != $null) { writeini -n $tc.cfgfile Chain revive $did(tc.cfgview,19).text }
    else { if ($readini($tc.cfgfile,n,Chain,revive) != $null) { remini $tc.cfgfile Chain revive } }
    did -r tc.cfgview 101 | did -a tc.cfgview 101 %tc.chan
    did -r tc.cfgview 14 | did -a tc.cfgview 14 %tc.chchan 
    did -a tc.cfgview 9 Channels lists saved.   
    flushini $tc.cfgfile
  }
}
on 1:DIALOG:tc.cfgview:sclick:22:{ 
  if ($did(tc.cfgview,21).seltext == $null) { halt }
  tc.adm del $gettok($did(tc.cfgview,21).seltext,1,32) 
  did -a tc.cfgview 9 Removed $gettok($did(tc.cfgview,21).seltext,1,32) from admin list. 
  did -d tc.cfgview 21 $did(tc.cfgview,21).sel 
}
on 1:DIALOG:tc.cfgview:sclick:42:{ 
  if ($did(tc.cfgview,41).text == $null) { did -a tc.cfgview 9 You need a faction name. Change not saved. | halt }
  else { writeini -n $tc.cfgfile Config faction $did(tc.cfgview,41).text | flushini $tc.cfgfile }  
  did -r tc.cfgview 41 | did -a tc.cfgview 41 $readini($tc.cfgfile,n,Config,faction)
  did -a tc.cfgview 9 Updated faction name. 
}
on 1:DIALOG:tc.cfgview:sclick:44:{ 
  if ($did(tc.cfgview,43).text < 0 || $did(tc.cfgview,43).text > 6) { did -a tc.cfgview 9 Invalid chaining bonus, please enter a number between 0-6. Change not saved. | halt }
  writeini -n $tc.cfgfile Chain upg $did(tc.cfgview,43).text | flushini $tc.cfgfile
  did -r tc.cfgview 43 | did -a tc.cfgview 43 $readini($tc.cfgfile,n,Chain,upg)
  if ($did(tc.cfgview,45).text < 0 || $did(tc.cfgview,45).text > 10 || $did(tc.cfgview,45).text == $null || $did(tc.cfgview,45).text !isnum) { var %herm = 0 }
  else { var %herm = $did(tc.cfgview,45).text }
  writeini -n $tc.cfgfile Config hermetic %herm | flushini $tc.cfgfile
  did -r tc.cfgview 43 | did -a tc.cfgview 43 $readini($tc.cfgfile,n,Chain,upg)
  did -r tc.cfgview 45 | did -a tc.cfgview 45 $readini($tc.cfgfile,n,Config,hermetic)
  did -a tc.cfgview 9 Updated faction bonuses. 
}
on 1:DIALOG:tc.cfgview:sclick:50:{
  dialog -m tc.pophelp tc.pophelp
  did -a tc.pophelp 1 This section controls the access level required to use certain commands on the bot. $crlf $crlf - When the box is checked, the bot will require a person to be added to the admin list before the command will work. $crlf $crlf - When it's checked with a greyed out background, the bot will require them to have operator status (@) in channel regardless of the minimum access level setting or admin list. $crlf $crlf - When it is unchecked, it will use the minimum requirement setting and the admin list is not used. 
}
on 1:DIALOG:tc.cfgview:sclick:51:{ 
  writeini -n $tc.cfgfile Access chgid $did(tc.cfgview,51).state | flushini $tc.cfgfile
  writeini -n $tc.cfgfile Access delid $did(tc.cfgview,51).state | flushini $tc.cfgfile
  if ($readini($tc.cfgfile,n,Access,delid) == 0) { did -a tc.cfgview 9 !delid access set to min access level. | did -u tc.cfgview 51 }
  if ($readini($tc.cfgfile,n,Access,delid) == 1) { did -a tc.cfgview 9 !delid access set to bot admins. | did -c tc.cfgview 51 }
  if ($readini($tc.cfgfile,n,Access,delid) == 2) { did -a tc.cfgview 9 !delid access set to channel operators. | did -cu tc.cfgview 51 }
}
on 1:DIALOG:tc.cfgview:sclick:52:{ 
  writeini -n $tc.cfgfile Access delspy $did(tc.cfgview,52).state | flushini $tc.cfgfile
  if ($readini($tc.cfgfile,n,Access,delspy) == 0) { did -a tc.cfgview 9 !delspy access set to min access level. | did -u tc.cfgview 52 }
  if ($readini($tc.cfgfile,n,Access,delspy) == 1) { did -a tc.cfgview 9 !delspy access set to bot admins. | did -c tc.cfgview 52 }
  if ($readini($tc.cfgfile,n,Access,delspy) == 2) { did -a tc.cfgview 9 !delspy access set to channel operators. | did -cu tc.cfgview 52 }
}
on 1:DIALOG:tc.cfgview:sclick:53:{ 
  writeini -n $tc.cfgfile Access delquote $did(tc.cfgview,53).state | flushini $tc.cfgfile
  if ($readini($tc.cfgfile,n,Access,delquote) == 0) { did -a tc.cfgview 9 !delquote access set to min access level. | did -u tc.cfgview 53 }
  if ($readini($tc.cfgfile,n,Access,delquote) == 1) { did -a tc.cfgview 9 !delquote access set to bot admins. | did -c tc.cfgview 53 }
  if ($readini($tc.cfgfile,n,Access,delquote) == 2) { did -a tc.cfgview 9 !delquote access set to channel operators. | did -cu tc.cfgview 53 }
}
on 1:DIALOG:tc.cfgview:sclick:54:{ 
  writeini -n $tc.cfgfile Access wartally $did(tc.cfgview,54).state | flushini $tc.cfgfile
  if ($readini($tc.cfgfile,n,Access,wartally) == 0) { did -a tc.cfgview 9 !wartally reset access set to min access level. | did -u tc.cfgview 54 }
  if ($readini($tc.cfgfile,n,Access,wartally) == 1) { did -a tc.cfgview 9 !wartally reset access set to bot admins. | did -c tc.cfgview 54 }
  if ($readini($tc.cfgfile,n,Access,wartally) == 2) { did -a tc.cfgview 9 !wartally reset access set to channel operators. | did -cu tc.cfgview 54 }  
}
on 1:DIALOG:tc.cfgview:sclick:55:{ 
  writeini -n $tc.cfgfile Access setbday $did(tc.cfgview,55).state | flushini $tc.cfgfile
  if ($readini($tc.cfgfile,n,Access,setbday) == 0) { did -a tc.cfgview 9 !setbday access set to min access level. | did -u tc.cfgview 55 }
  if ($readini($tc.cfgfile,n,Access,setbday) == 1) { did -a tc.cfgview 9 !setbday access set to bot admins. | did -c tc.cfgview 55 }
  if ($readini($tc.cfgfile,n,Access,setbday) == 2) { did -a tc.cfgview 9 !setbday access set to channel operators. | did -cu tc.cfgview 55 }  
}
on 1:DIALOG:tc.cfgview:sclick:56:{ writeini -n $tc.cfgfile Access min o | flushini $tc.cfgfile | did -a tc.cfgview 9 Min access requirement set to OP. }
on 1:DIALOG:tc.cfgview:sclick:57:{ writeini -n $tc.cfgfile Access min h | flushini $tc.cfgfile | did -a tc.cfgview 9 Min access requirement set to HALFOP. }
on 1:DIALOG:tc.cfgview:sclick:58:{ writeini -n $tc.cfgfile Access min v | flushini $tc.cfgfile | did -a tc.cfgview 9 Min access requirement set to VOICE. }
on 1:DIALOG:tc.cfgview:sclick:59:{ writeini -n $tc.cfgfile Access min n | flushini $tc.cfgfile | did -a tc.cfgview 9 Min access requirement set to NONE. }
on 1:DIALOG:tc.cfgview:sclick:61:{ 
  if ($readini($tc.cfgfile,n,Color,color) == n) { 
    set %tc.col $puttok(%tc.col,y,1,32) 
    writeini -n $tc.cfgfile Color color y 
    did -c tc.cfgview 61 | did -a tc.cfgview 9 Bot will now respond with colors. 
  } 
  else { 
    set %tc.col $puttok(%tc.col,n,1,32) 
    writeini -n $tc.cfgfile Color color n 
    did -u tc.cfgview 61 | did -a tc.cfgview 9 Bot will now respond in plain text. 
  }   
  flushini $tc.cfgfile
}
on 1:DIALOG:tc.cfgview:sclick:64:{ tc.cfgcol | did -a tc.cfgview 9 Reloaded color theme from last saved values. }
on 1:DIALOG:tc.cfgview:sclick:65:{
  if ($did(tc.cfgview,2061).text !isnum || $did(tc.cfgview,2061).text > 15 || $did(tc.cfgview,2061).text < 0) { did -a tc.cfgview 9 Primary color value invalid, proper values are 0-15. | halt }
  if ($did(tc.cfgview,2062).text !isnum || $did(tc.cfgview,2062).text > 15 || $did(tc.cfgview,2062).text < 0) { did -a tc.cfgview 9 Secondary color value invalid, proper values are 0-15. | halt }
  if ($did(tc.cfgview,2063).text !isnum || $did(tc.cfgview,2063).text > 15 || $did(tc.cfgview,2063).text < 0) { did -a tc.cfgview 9 Help color value invalid, proper values are 0-15. | halt }
  if ($did(tc.cfgview,2064).text !isnum || $did(tc.cfgview,2064).text > 15 || $did(tc.cfgview,2064).text < 0) { did -a tc.cfgview 9 Note color value invalid, proper values are 0-15. | halt }
  if ($did(tc.cfgview,2065).text !isnum || $did(tc.cfgview,2065).text > 15 || $did(tc.cfgview,2065).text < 0) { did -a tc.cfgview 9 Error color value invalid, proper values are 0-15. | halt }
  if ($did(tc.cfgview,2066).text !isnum || $did(tc.cfgview,2066).text > 15 || $did(tc.cfgview,2066).text < 0) { did -a tc.cfgview 9 Good color value invalid, proper values are 0-15. | halt }
  if ($did(tc.cfgview,2067).text !isnum || $did(tc.cfgview,2067).text > 15 || $did(tc.cfgview,2067).text < 0) { did -a tc.cfgview 9 Bad color value invalid, proper values are 0-15. | halt }
  if ($did(tc.cfgview,2068).text !isnum || $did(tc.cfgview,2068).text > 15 || $did(tc.cfgview,2068).text < 0) { did -a tc.cfgview 9 Subnote color value invalid, proper values are 0-15. | halt }
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2061).text,2,32) | writeini -n $tc.cfgfile Color pri $did(tc.cfgview,2061).text
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2062).text,3,32) | writeini -n $tc.cfgfile Color sec $did(tc.cfgview,2062).text
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2063).text,4,32) | writeini -n $tc.cfgfile Color help $did(tc.cfgview,2063).text
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2064).text,5,32) | writeini -n $tc.cfgfile Color note $did(tc.cfgview,2064).text
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2065).text,6,32) | writeini -n $tc.cfgfile Color error $did(tc.cfgview,2065).text
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2066).text,7,32) | writeini -n $tc.cfgfile Color good $did(tc.cfgview,2066).text
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2067).text,8,32) | writeini -n $tc.cfgfile Color bad $did(tc.cfgview,2067).text
  set %tc.col $puttok(%tc.col,$did(tc.cfgview,2068).text,9,32) | writeini -n $tc.cfgfile Color subnote $did(tc.cfgview,2068).text
  flushini $tc.cfgfile | did -a tc.cfgview 9 New color theme saved. 
}
on 1:DIALOG:tc.cfgview:sclick:66:{
  did -r tc.cfgview 2061 | did -a tc.cfgview 2061 10
  did -r tc.cfgview 2062 | did -a tc.cfgview 2062 7
  did -r tc.cfgview 2063 | did -a tc.cfgview 2063 3
  did -r tc.cfgview 2064 | did -a tc.cfgview 2064 13
  did -r tc.cfgview 2065 | did -a tc.cfgview 2065 4
  did -r tc.cfgview 2066 | did -a tc.cfgview 2066 3
  did -r tc.cfgview 2067 | did -a tc.cfgview 2067 4
  did -r tc.cfgview 2068 | did -a tc.cfgview 2068 6
  did -a tc.cfgview 9 Set all color values to default settings. (Not saved)
}
on 1:DIALOG:tc.cfgview:sclick:71:{ 
  if ($readini($tc.cfgfile,n,Config,ereset) == no) { 
    writeini -n $tc.cfgfile Config ereset yes 
    did -c tc.cfgview 71 | did -a tc.cfgview 9 Energy reset notice switched ON. 
  } 
  else { 
    writeini -n $tc.cfgfile Config ereset no 
    did -u tc.cfgview 71 | did -a tc.cfgview 9 Energy reset notice switched OFF. 
  } 
  flushini $tc.cfgfile
}
on 1:DIALOG:tc.cfgview:sclick:72:{ 
  if ($readini($tc.cfgfile,n,Chain,revnote) == no) { 
    writeini -n $tc.cfgfile Chain revnote yes 
    did -c tc.cfgview 72 | did -a tc.cfgview 9 Revive string confirmation notice switched ON. 
  } 
  else { 
    writeini -n $tc.cfgfile Chain revnote no 
    did -u tc.cfgview 72 | did -a tc.cfgview 9 Revive string confirmation notice switched OFF. 
  } 
  flushini $tc.cfgfile
}
on 1:DIALOG:tc.cfgview:sclick:75:{ 
  if ($readini($tc.cfgfile,n,Config,bstats) == no) { 
    writeini -n $tc.cfgfile Config bstats yes 
    did -c tc.cfgview 75 | did -a tc.cfgview 9 Updating stats now requires "Battle Stats" string. 
  } 
  else { 
    writeini -n $tc.cfgfile Config bstats no 
    did -u tc.cfgview 75 | did -a tc.cfgview 9 Updating stats now does NOT require the "Battle Stats" string. 
  } 
  flushini $tc.cfgfile
}
on 1:DIALOG:tc.cfgview:sclick:79:{ 
  if ($readini($tc.cfgfile,n,Chain,postchains) == no) { 
    writeini -n $tc.cfgfile Chain postchains yes 
    did -c tc.cfgview 79 | did -a tc.cfgview 9 Notices will now be posted in other channels at the start/end of chains. 
  } 
  else { 
    writeini -n $tc.cfgfile Chain postchains no 
    did -u tc.cfgview 79 | did -a tc.cfgview 9 Notices will no longer be posted in other channels at the start/end of chains. 
  } 
  flushini $tc.cfgfile
}
on 1:DIALOG:tc.cfgview:sclick:171:{ 
  if ($readini($tc.cfgfile,n,Quote,split) == no) { 
    writeini -n $tc.cfgfile Quote split yes 
    did -c tc.cfgview 171 | did -a tc.cfgview 9 Quote bot will now keep each channel seperate. 
  } 
  else { 
    writeini -n $tc.cfgfile Quote split no 
    did -u tc.cfgview 171 | did -a tc.cfgview 9 Quote bot will now only use one database. 
  } 
  flushini $tc.cfgfile
}

on 1:DIALOG:tc.cfgview:sclick:172:{ 
  if ($readini($tc.cfgfile,n,Config,spyen) == no) { 
    writeini -n $tc.cfgfile Config spyen yes 
    did -c tc.cfgview 172 | did -a tc.cfgview 9 Spy storage commands enabled. 
  } 
  else { 
    writeini -n $tc.cfgfile Config spyen no 
    did -u tc.cfgview 172 | did -a tc.cfgview 9 Spy storage commands disabled. 
  } 
  flushini $tc.cfgfile
}

on 1:DIALOG:tc.cfgview:sclick:173:{ 
  if ($readini($tc.cfgfile,n,Config,findadd) == no) { 
    writeini -n $tc.cfgfile Config findadd yes 
    did -c tc.cfgview 173 | did -a tc.cfgview 9 The bot will now always add IDs if they're not already in when someone uses !find. 
  } 
  else { 
    writeini -n $tc.cfgfile Config findadd no 
    did -u tc.cfgview 173 | did -a tc.cfgview 9 The bot will only add IDs if they're in your faction and not already in when someone uses !find. 
  } 
  flushini $tc.cfgfile
}

on 1:DIALOG:tc.cfgview:sclick:174:{ 
  if ($readini($tc.cfgfile,n,Config,odrevive) == no) { 
    writeini -n $tc.cfgfile Config odrevive yes 
    did -c tc.cfgview 174 | did -a tc.cfgview 9 The bot will now paste a revive request after someone pastes an overdose. 
  } 
  else { 
    writeini -n $tc.cfgfile Config odrevive no 
    did -u tc.cfgview 174 | did -a tc.cfgview 9 The bot will NOT paste a revive request after someone pastes an overdose. 
  } 
  flushini $tc.cfgfile
}

on 1:DIALOG:tc.cfgview:sclick:176:{ 
  if ($readini($tc.cfgfile,n,Banker,enable) == d) { 
    writeini -n $tc.cfgfile Banker enable e
    did -c tc.cfgview 176 | did -a tc.cfgview 9 Banker commands have been ENABLED. Use !setbanker to change the line. (req bot admin)
  } 
  else { 
    writeini -n $tc.cfgfile Banker enable d
    did -u tc.cfgview 176 | did -a tc.cfgview 9 Banker commands have been DISABLED
  } 
  flushini $tc.cfgfile
}

on 1:DIALOG:tc.cfgview:sclick:177:{ 
  if ($readini($tc.cfgfile,n,Config,statsaving) == d) { 
    writeini -n $tc.cfgfile Config statsaving e
    did -c tc.cfgview 177 | did -a tc.cfgview 9 Battle stat saving/commands have been ENABLED. 
  } 
  else { 
    writeini -n $tc.cfgfile Config statsaving d
    did -u tc.cfgview 177 | did -a tc.cfgview 9 Battle stat saving/commands have been DISABLED
  } 
  flushini $tc.cfgfile
  set %tc.statsaving $readini($tc.cfgfile,n,Config,statsaving)
}

on 1:DIALOG:tc.cfgview:sclick:262:{   
  if ($did(tc.cfgview,261).text != $null) { 
    writeini -n $tc.cfgfile API key $did(tc.cfgview,261).text
    did -a tc.cfgview 9 API key has been updated.
  }
  else {
    remini $tc.cfgfile API key
    did -a tc.cfgview 9 API key has been cleared.
  }
  flushini $tc.cfgfile
}

on 1:DIALOG:tc.cfgview:sclick:270:{ 
  if ($readini($tc.cfgfile,n,API,enable) == d) { 
    writeini -n $tc.cfgfile API enable e
    did -c tc.cfgview 270 | did -a tc.cfgview 9 API usage has been ENABLED. Make sure you have set your key correctly.
  } 
  else { 
    writeini -n $tc.cfgfile API enable d
    did -u tc.cfgview 270 | did -a tc.cfgview 9 API usage has been DISABLED. 
  } 
  flushini $tc.cfgfile
}

menu status,channel { 
  Universal TC Assistant v $+ $tc.botvers Config:tc.cfg
}

menu nicklist {
  Universal TC Assistant v $+ $tc.botvers
  . Add to Admin list:tc.adm add $1
  . Delete from Admin list:tc.adm del $1
}

alias -l tc.adm { tc.cfgcheck
  if ($1 == add) { var %admip = $address($2,3) | hadd -m tc.adm $2 %admip | hsave tc.adm $tc.admfile | echo -ag $2 added to admin list with IP: %admip } 
  if ($1 == del) { hdel tc.adm $2 | hsave tc.adm $tc.admfile | echo -ag $2 removed from admin list. }
}

alias -l tc.cfg { tc.cfgcheck
  if ($dialog(tc.cfgview)) { halt } | dialog -m tc.cfgview tc.cfgview 
  var %counter 1 | while (%counter <= $hget(tc.adm,0).item) { did -a tc.cfgview 21 $hget(tc.adm,%counter).item - $hget(tc.adm,%counter).data | inc %counter }
  did -a tc.cfgview 19 $readini($tc.cfgfile,n,Chain,revive)
  did -a tc.cfgview 41 $readini($tc.cfgfile,n,Config,faction)
  did -a tc.cfgview 43 $readini($tc.cfgfile,n,Chain,upg)
  did -a tc.cfgview 45 $readini($tc.cfgfile,n,Config,hermetic)
  if ($readini($tc.cfgfile,n,Access,delid) == 0) { did -u tc.cfgview 51 } | if ($readini($tc.cfgfile,Access,delid) == 1) { did -c tc.cfgview 51 } | if ($readini($tc.cfgfile,Access,delid) == 2) { did -cu tc.cfgview 51 }
  if ($readini($tc.cfgfile,n,Access,delspy) == 0) { did -u tc.cfgview 52 } | if ($readini($tc.cfgfile,Access,delspy) == 1) { did -c tc.cfgview 52 } | if ($readini($tc.cfgfile,Access,delspy) == 2) { did -cu tc.cfgview 52 }
  if ($readini($tc.cfgfile,n,Access,delquote) == 0) { did -u tc.cfgview 53 } | if ($readini($tc.cfgfile,Access,delquote) == 1) { did -c tc.cfgview 53 } | if ($readini($tc.cfgfile,Access,delquote) == 2) { did -cu tc.cfgview 53 }
  if ($readini($tc.cfgfile,n,Access,wartally) == 0) { did -u tc.cfgview 54 } | if ($readini($tc.cfgfile,Access,wartally) == 1) { did -c tc.cfgview 54 } | if ($readini($tc.cfgfile,Access,wartally) == 2) { did -cu tc.cfgview 54 }
  if ($readini($tc.cfgfile,n,Access,setbday) == 0) { did -u tc.cfgview 55 } | if ($readini($tc.cfgfile,Access,setbday) == 1) { did -c tc.cfgview 55 } | if ($readini($tc.cfgfile,Access,setbday) == 2) { did -cu tc.cfgview 55 }
  if ($readini($tc.cfgfile,n,Access,min) == o) { did -c tc.cfgview 56 }
  elseif ($readini($tc.cfgfile,n,Access,min) == h) {  did -c tc.cfgview 57 }
  elseif ($readini($tc.cfgfile,n,Access,min) == v) { did -c tc.cfgview 58 }
  elseif ($readini($tc.cfgfile,n,Access,min) == n) { did -c tc.cfgview 59 }
  if ($readini($tc.cfgfile,n,Color,color) == y) { did -c tc.cfgview 61 } | else { did -u tc.cfgview 61 }
  if ($readini($tc.cfgfile,n,Config,ereset) == yes) { did -c tc.cfgview 71 } | else { did -u tc.cfgview 71 }
  if ($readini($tc.cfgfile,n,Chain,revnote) == yes) { did -c tc.cfgview 72 } | else { did -u tc.cfgview 72 }
  if ($readini($tc.cfgfile,n,Config,bstats) == yes) { did -c tc.cfgview 75 } | else { did -u tc.cfgview 75 }
  if ($readini($tc.cfgfile,n,Chain,postchains) == yes) { did -c tc.cfgview 79 } | else { did -u tc.cfgview 79 }
  if ($readini($tc.cfgfile,n,Quote,split) == yes) { did -c tc.cfgview 171 } | else { did -u tc.cfgview 171 }
  tc.cfgcol
  did -a tc.cfgview 2161 0 = white $+ $crlf $+ 1 = black $+ $crlf $+ 2 = dark blue $+ $crlf $+ 3 = dark green $+ $crlf $+ 4 = red $+ $crlf $+ 5 = brown $+ $crlf $+ 6 = dark purple $+ $crlf $+ 7 = orange $+ $crlf $+ 8 = yellow $+ $crlf $+ 9 = light green $+ $crlf $+ 10 = dark teal $+ $crlf $+ 11 = light teal $+ $crlf $+ 12 = blue $+ $crlf $+ 13 = light purple $+ $crlf $+ 14 = dark grey $+ $crlf $+ 15 = grey
  if ($readini($tc.cfgfile,n,Config,spyen) == yes) { did -c tc.cfgview 172 } | else { did -u tc.cfgview 172 }
  if ($readini($tc.cfgfile,n,Config,findadd) == yes) { did -c tc.cfgview 173 } | else { did -u tc.cfgview 173 }
  if ($readini($tc.cfgfile,n,Config,odrevive) == yes) { did -c tc.cfgview 174 } | else { did -u tc.cfgview 174 }
  if ($readini($tc.cfgfile,n,Banker,enable) == e) { did -c tc.cfgview 176 } | else { did -u tc.cfgview 176 }
  if ($readini($tc.cfgfile,n,Config,statsaving) == e) { did -c tc.cfgview 177 } | else { did -u tc.cfgview 177 }
  if ($readini($tc.cfgfile,n,API,key)) { did -a tc.cfgview 261 $readini($tc.cfgfile,n,API,key) } 
  if ($readini($tc.cfgfile,n,API,enable) == e) { did -c tc.cfgview 270 } | else { did -u tc.cfgview 270 }

}

alias -l tc.cfgcol {
  did -r tc.cfgview 2061 | did -a tc.cfgview 2061 $readini($tc.cfgfile,n,Color,pri)
  did -r tc.cfgview 2062 | did -a tc.cfgview 2062 $readini($tc.cfgfile,n,Color,sec)
  did -r tc.cfgview 2063 | did -a tc.cfgview 2063 $readini($tc.cfgfile,n,Color,help)
  did -r tc.cfgview 2064 | did -a tc.cfgview 2064 $readini($tc.cfgfile,n,Color,note)
  did -r tc.cfgview 2065 | did -a tc.cfgview 2065 $readini($tc.cfgfile,n,Color,error)
  did -r tc.cfgview 2066 | did -a tc.cfgview 2066 $readini($tc.cfgfile,n,Color,good)
  did -r tc.cfgview 2067 | did -a tc.cfgview 2067 $readini($tc.cfgfile,n,Color,bad)
  did -r tc.cfgview 2068 | did -a tc.cfgview 2068 $readini($tc.cfgfile,n,Color,subnote)
}

; Config file check, this is called in several places.
alias -l tc.cfgcheck { if (!$exists($tc.cfgfile)) { echo -ag Config file not found! Need to type /tcsetup for initial setup! | halt } }

alias -l tc.chkaccess {
  var %admaccess = 0, %ptr = 1
  if ($1 == wartally) { var %comm = wartally/warresp reset }
  else { var %comm = $1 }
  while (%ptr <= $hget(tc.adm,0).item) { if ($2 == $hget(tc.adm,%ptr).item && $address($2,3) == $hget(tc.adm,%ptr).data) { var %admaccess = 1 } | inc %ptr }
  if ($readini($tc.cfgfile,n,Access,$1) == 1 && %admaccess != 1) { msg $3 $tc.col(5) $+ No can do, you need to request a bot admin to use $+ $tc.col(2) $chr(33) $+ %comm $+ $tc.col(5) $+ . | halt }
  if ($readini($tc.cfgfile,n,Access,$1) == 2 && $2 !isop $3) { msg $3 $tc.col(5) $+ No can do, you need to request a channel op to use $+ $tc.col(2) $chr(33) $+ %comm $+ $tc.col(5) $+ . | halt }
}

; checks admin status. Does not halt, returns 1 if allowed, nothing if not. Handle at returning sub.
alias -l tc.chkadm {
  var %admaccess = 0, %ptr = 1
  while (%ptr <= $hget(tc.adm,0).item) { 
    if ($1 == $hget(tc.adm,%ptr).item && $address($1,3) == $hget(tc.adm,%ptr).data) { var %admaccess = 1 } 
    inc %ptr 
  }
  if (%admaccess == 1) { return 1 }
  else { return $null }
}

; --------------------------------- personal setting + social functions ---------------------------------

alias -l tc.set { writeini -n $tc.psetfile $1 $2 $3- }
alias -l tc.getset { return $readini($tc.psetfile,n,$1,$2) }

; Split quote chan scanner. 
; $1 = channel, $2 = requested quote
; Takes input, scans through quote file collecting data as needed and:
;    if $2 exists - returns requested quote file location.
;    otherwise - returns number of channel only quotes.
; Any checks are done in the returning subs. This is used in many many places throughout the quote system.
; Better for one spot since fixing bugs = having to go find this shit everywhere and fix it there too.
alias -l tc.qscan {
  var %eof = $lines($tc.quotefile)  
  var %ptr = 1, %ctr = 0
  while (%ptr <= %eof) { 
    if ($gettok($read($tc.quotefile,n,%ptr),1,9) == $1) { 
      inc %ctr 
      if ($2 == %ctr) { var %loc = %ptr }
    }
    inc %ptr
  }
  if (%loc != $null) { return %loc }
  else { return %ctr }
}

; Quote spammer alias
; $1 is the timer, $2 is the channel
alias -l tc.qspam {
  if (%tcchain.stchan == $null) { 
    if ($readini($tc.cfgfile,n,Quote,split) == yes) {
      var %ctr = $tc.qscan($2), %qrand = $rand(1,%ctr)
      if (%qrand != $null) { var %loc = $tc.qscan($2,%qrand) }
      else { echo -s 4Note: $2 random quote timer skipped, no matched quotes for channel. Next one in $duration($1) }
    }
    else { var %ctr = $lines($tc.quotefile), %qrand = $rand(1,%ctr), %loc = %qrand }
    var %quote = $gettok($read($tc.quotefile,n,%loc),4,9)
    if (%quote != $null) { msg $2 $tc.col(8) $+ Quote $+ $tc.col(2) %qrand $+ $tc.col(8) $+ / $+ $tc.col(2) $+ %ctr $+ $tc.col(8) $+ : $+ $tc.col(1) $eval(%quote,1) }
    else { echo -s 4Note: $2 random quote timer skipped, no matched quotes. Next one in $duration($1) }
  }
  else { echo -s 4Note: $2 random quote timer skipped, chain is running. Next one in $duration($1) }
  .timer0.tcflag.qspam $+ $remove($2,$chr(35)) 1 $1 tc.qspam $1 $2 
}

; --------------------------------- stat bot functions ---------------------------------

; stats subroutine that takes a players ID, looks for a stats table for that player in memory.
; once it has the table, it saves it to the players stat file, posts a summary update and performs garbage cleanup
; this is called on a catchup timer after a person pastes their stats so it's only run once instead of after every single paste
alias -l tc.statsave {
  if ($timer($1 $+ $chr(46) $+ stat)) { .timer $+ $1 $+ $chr(46) $+ stat off }
  if (%tc.bstatadd != $null) {
    var %statstablename = stat $+ $1
    var %statsfile = $tc.statsfile($1)    
    var %pastsaved = $read(%statsfile, $lines(%statsfile)) 
    var %name = $tc.getname($1)  
    var %pasttime = $gettok(%pastsaved,1,32) | if (%pasttime == $null) { var %pasttime = 0 }
    var %paststr = $gettok(%pastsaved,2,32) | if (%paststr == $null) { var %paststr = 0 }
    var %pastdef = $gettok(%pastsaved,3,32) | if (%pastdef == $null) { var %pastdef = 0 }
    var %pastspd $gettok(%pastsaved,4,32) | if (%pastspd == $null) { var %pastspd = 0 }
    var %pastdex $gettok(%pastsaved,5,32) | if (%pastdex == $null) { var %pastdex = 0 }
    var %pasttotal = $calc(%paststr + %pastdef + %pastspd + %pastdex)
    var %currstr = $gettok($hget(%statstablename,str),1,32) | if (%currstr == $null) { var %currstr = %paststr }
    var %currdef = $gettok($hget(%statstablename,def),1,32) | if (%currdef == $null) { var %currdef = %pastdef }
    var %currspd = $gettok($hget(%statstablename,spd),1,32) | if (%currspd == $null) { var %currspd = %pastspd }
    var %currdex = $gettok($hget(%statstablename,dex),1,32) | if (%currdex == $null) { var %currdex = %pastdex }
    var %currtotal = $calc(%currstr + %currdef + %currspd + %currdex)
    var %timediff = $calc($tc.time - %pasttime)
    var %pstr = $gettok($hget(%statstablename,str),2,32) | if (%pstr !isnum) { var %pstr = 0 }
    var %pdef = $gettok($hget(%statstablename,def),2,32) | if (%pdef !isnum) { var %pdef = 0 }
    var %pspd = $gettok($hget(%statstablename,spd),2,32) | if (%pspd !isnum) { var %pspd = 0 }
    var %pdex = $gettok($hget(%statstablename,dex),2,32) | if (%pdex !isnum) { var %pdex = 0 }
    tc.set $1 cstat %pstr %pdef %pspd %pdex    
    if (%currtotal != %pasttotal) {
      if (%timediff <= 3600 && $lines(%statsfile) != 1) { 
        write -dl $+ $lines(%statsfile) $qt(%statsfile)
        var %saveline = %pasttime %currstr %currdef %currspd %currdex
        write $qt(%statsfile) %saveline
      }
      else { 
        var %saveline = $tc.time %currstr %currdef %currspd %currdex
        write $qt(%statsfile) %saveline
      }
      if ($lines(%statsfile) != 1) {
        var %chanpost = $tc.col(2) $+ %name $+ $tc.col(1), you have 
        if (%currstr < %paststr) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(7) $+ lost $+ $tc.col(2) $tc.addComma($calc(%paststr - %currstr)) $tc.col(1) $+ strength }
        if (%currstr > %paststr) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(6) $+ gained $+ $tc.col(2) $tc.addComma($calc(%currstr - %paststr)) $tc.col(1) $+ strength }
        if (%currstr != %paststr) { var %chanpost = %chanpost $+ $chr(44) }
        if (%currdef < %pastdef) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(7) $+ lost $+ $tc.col(2) $tc.addComma($calc(%pastdef - %currdef)) $tc.col(1) $+ defense }
        if (%currdef > %pastdef) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(6) $+ gained $+ $tc.col(2) $tc.addComma($calc(%currdef - %pastdef)) $tc.col(1) $+ defense }
        if (%currdef != %pastdef) { var %chanpost = %chanpost $+ $chr(44) }
        if (%currspd < %pastspd) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(7) $+ lost $+ $tc.col(2) $tc.addComma($calc(%pastspd - %currspd)) $tc.col(1) $+ speed }
        if (%currspd > %pastspd) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(6) $+ gained $+ $tc.col(2) $tc.addComma($calc(%currspd - %pastspd)) $tc.col(1) $+ speed }
        if (%currspd != %pastspd) { var %chanpost = %chanpost $+ $chr(44) }
        if (%currdex < %pastdex) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(7) $+ lost $+ $tc.col(2) $tc.addComma($calc(%pastdex - %currdex)) $tc.col(1) $+ dexterity }
        if (%currdex > %pastdex) { var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(6) $+ gained $+ $tc.col(2) $tc.addComma($calc(%currdex - %pastdex)) $tc.col(1) $+ dexterity }
        var %chanpost = %chanpost $+ $chr(32) $+ $tc.col(1) $+ since your last update on $+ $tc.col(2) $asctime(%pasttime,mmm d h:nn:ss tt) $tc.col(1) $+ (GMT/TC time) - $+ $tc.col(2) $duration($calc($tc.time - %pasttime)) $tc.col(1) $+ ago.
        if ($calc(%pstr + %pdef + %pspd + %pdex) != 0) {
          msg $2 %chanpost $tc.col(1) $+ Your current passives have been saved, use $tc.col(2) $+ !cstats $tc.col(1) $+ to see what you look like right now. $tc.col(8) $+ To see all stat display commands, use !shelp
        } 
        else {
          msg $2 %chanpost $tc.col(8) $+ Use !shelp to see all stat display commands.
        }
      } 
      else { msg $2 $tc.col(2) $+ %name $+ $tc.col(1) $+ , thank you for posting your initial stats. They have been saved. }
    }
    else { msg $2 $tc.col(2) $+ %name $+ $tc.col(1) $+ , your stats have not changed since your last paste on $+ $tc.col(2) $asctime(%pasttime,mmm d h:nn:ss tt) $tc.col(1) $+ (GMT/TC time) - $+ $tc.col(2) $duration($calc($tc.time - %pasttime)) $tc.col(1) $+ ago. }
    hfree %statstablename
  }
  if ($readini($tc.cfgfile,n,Config,bstats) == yes && $findtok(%tc.bstats,$3,1,32)) { 
    set %tc.bstats $deltok(%tc.bstats,$findtok(%tc.bstats,$3,1,32),32) 
    if (%tc.bstats == $null) { unset %tc.bstats }
  } 
  unset %tc.bstatadd
}

alias -l tc.hitlistclr {
  if ($hget(tc.atklist)) {
    hfree tc.atklist
    msg $1 $tc.col(4) $+ Note: Cleared attackers list due to 20 minutes of chain inactivity.
  }
}

alias -l tc.groupatk {
  msg $1 $tc.col(6) $+ Make with the beatings! Go! Fighty Stabby Time!
  .notice $1 $tc.col(7) $+ Let the games begin! Group attack started on $tc.atklink(%tcgroup.targ) <- join if you can!
  unset %tcgroup.*
}

; --------------------------------- spy bot functions ---------------------------------

alias -l tc.addspyhelp { return $tc.col(3) $+ Adds a spy to the bot. Manual usage: $+ $tc.col(2) !addspy Name TornID Str Spd Dex Def Total $tc.col(1) $+ Faction (optional). $tc.col(1) $+ To add automatically, use: $+ $tc.col(2) !addspy auto $tc.col(1) $+ if you have the full paste, or $+ $tc.col(2) !addspy Name TornID auto $tc.col(1) $+ if you only have the stats. $tc.col(8) $+ For automatic pre-RESPO reports, use: !addspy old }

alias -l tc.spysave {
  if (%tc.spy != $null) { var %table = spy. $+ %tc.spy, %addedby = %tc.spy }
  else { var %table = spy. $+ %tc.oldspy, %addedby = %tc.oldspy }
  if ($hget(%table,id) == $null || $hget(%table,id) !isnum) { 
    msg %tc.spychan $tc.col(5) $+ The ID was not successfully read. When using automatic mode, paste your spy exactly how Torn sends it to you without modification. 
    msg %tc.spychan $tc.col(1) $+ Use: $+ $tc.col(2) !addspy $+ $tc.col(1) to see manual input help. 
  }
  else {
    if (!$hfind(%table,str) || $hget(%table,str) !isnum) { hadd -m %table str 0 }
    if (!$hfind(%table,spd) || $hget(%table,spd) !isnum) { hadd -m %table spd 0 }
    if (!$hfind(%table,dex) || $hget(%table,dex) !isnum) { hadd -m %table dex 0 }
    if (!$hfind(%table,def) || $hget(%table,def) !isnum) { hadd -m %table def 0 }
    if (!$hfind(%table,totb) || $hget(%table,totb) !isnum) { hadd -m %table totb 0 }
    if (!$hfind(%table,man) || $hget(%table,man) !isnum) { hadd -m %table man 0 }
    if (!$hfind(%table,int) || $hget(%table,int) !isnum) { hadd -m %table int 0 }
    if (!$hfind(%table,end) || $hget(%table,end) !isnum) { hadd -m %table end 0 }
    if (!$hfind(%table,totw) || $hget(%table,totw) !isnum) { hadd -m %table totw 0 }
    if (!$hfind(%table,cash) || $hget(%table,cash) !isnum) { hadd -m %table cash -1 }
    if ($tc.getapikey != $null) {
      sockclose spyprof 
      sockopen -e spyprof www.torn.com 443
      msg %tc.spychan $tc.col(8) $+ Attempting to get additional info from Torn's API (5 seconds max)... 
      .timer $+ 0.tc.spyscrape 1 5 tc.spywrite 
    }
    else { tc.spywrite }
  }
}

alias -l tc.spywrite {
  sockclose spyprof
  if ($timer(0.tc.spyscrape)) { .timer $+ 0.tc.spyscrape off }
  if (%tc.spy != $null) { var %table = spy. $+ %tc.spy, %addedby = %tc.spy }
  else { var %table = spy. $+ %tc.oldspy, %addedby = %tc.oldspy }
  if (%tcsprof.lvl != $null) { hadd -m %table lvl %tcsprof.lvl }
  elseif (!$hfind(%table,lvl) || $hget(%table,lvl) !isnum) { hadd -m %table lvl 0 }
  else {  }
  if (%tcsprof.fact != $null) { hadd -m %table fact %tcsprof.fact }
  elseif (!$hfind(%table,fact) || $hget(%table,fact) == $null) { hadd -m %table fact N/A } 
  else {  }
  var %filename = $tc.spyfile
  var %savestring = $hget(%table,name) $+ $chr(9) $+ $hget(%table,id) $+ $chr(9) $+  $hget(%table,lvl) $+ $chr(9) $+ $hget(%table,fact) $+ $chr(9) $+ $hget(%table,spd) $+ $chr(9) $+ $hget(%table,str) $+ $chr(9) $+ $hget(%table,def) $+ $chr(9) $+ $hget(%table,dex) $+ $chr(9) $+ $hget(%table,totb) $+ $chr(9) $+ $hget(%table,man) $+ $chr(9) $+ $hget(%table,int) $+ $chr(9) $+ $hget(%table,end) $+ $chr(9) $+ $hget(%table,totw) $+ $chr(9) $+ $hget(%table,cash) $+ $chr(9) $+ %addedby $+ $chr(9) $+ $tc.time
  if ($exists($tc.spyfile)) { 
    var %eof = $lines($tc.spyfile), %ptr = 1
    while (%ptr <= %eof) {
      var %readid = $gettok($read($tc.spyfile,%ptr),2,9)
      if (%readid == $hget(%table,id)) { var %spyold = 1 | write -dl $+ %ptr $tc.spyfile | break }
      inc %ptr
    }
  }
  write $tc.spyfile %savestring
  if (%spyold != $null) { msg %tc.spychan $tc.col(1) $+ Spy entry for $+ $tc.col(2) $hget(%table,name) $tc.colid($hget(%table,id),1,2) $tc.col(1) $+ was successfully updated! }
  else { msg %tc.spychan $tc.col(1) $+ Spy entry for $+ $tc.col(2) $hget(%table,name) $tc.colid($hget(%table,id),1,2) $tc.col(1) $+ was successfully added! }
  hfree %table | .timer0.tcflag.spyauto off | unset %tc.spy | unset %tc.oldspy | unset %tc.spychan | unset %tcsprof.*
}


alias -l tc.spycancel {
  msg $1 $tc.col(1) $+ Spy auto-entry cancelled.
  if (%tc.spy != $null) { var %table = spy. $+ %tc.spy }
  else { var %table = spy. $+ %tc.oldspy }
  if ($hget(%table)) { hfree %table }
  unset %tc.spy | unset %tc.oldspy
}

; --------------------------------- crimes functions ---------------------------------

; crimes subroutine that takes a players ID, looks for a crimes table for that player in memory, saves the table to the database, then frees it.
; this is called on a catchup timer after a person pastes their crimes so it's only run once instead of after every single line. 
; it should also be immediately called after all crimes have been posted
alias -l tc.crimesave {
  .timer $+ $1 $+ $chr(46) $+ crimes off
  var %cstr = %tc.crnew. [ $+ [ $1 ] ], %pstr = %tc.crnochg. [ $+ [ $1 ] ], %ptr = 1, %eof = $lines($tc.crimesfile)
  while (%ptr <= %eof) { if ($gettok(%cstr,1,32) == $gettok($read($tc.crimesfile,%ptr),1,32)) { write -dl $+ %ptr $tc.crimesfile | break } | inc %ptr }
  if (%pstr != $null) { msg $2 $tc.col(1) $+ Your $+ $tc.col(2) %pstr $tc.col(1) $+ have not changed since your last update. }  
  var %p = 3, %e = 10 | while (%p <= %e) { if ($gettok(%cstr,%p,32) == -1) { var %cstr = $puttok(%cstr,0,%p,32) } | inc %p }
  write $tc.crimesfile %cstr 
  ; msg $2 $tc.col(1) $+ Type $+ $tc.col(2) !crimes $tc.col(1) $+ to see your current medal/merit position details.
  unset %tc.crnew. [ $+ [ $1 ] ] | unset %tc.crold. [ $+ [ $1 ] ] | unset %tc.crnochg. [ $+ [ $1 ] ] | unset %tc.crupd. [ $+ [ $1 ] ]
}

; Big stupid crime data lookup subroutine.
; Takes a type var and crime count and returns a tokenized string indicating their next merits/medal point.
; The sub that it's returned to takes this string and formats it into something that looks nice.
; This is in an alias to avoid having to copy/paste for every time it needs to look this stuff up. 
; Type starts at 2 because position 1 in the saved file is the ID. This just keeps locations in sync.
alias -l tc.crimedata {
  if ($1 == 3) {
    var %type = Selling illegal products, %max = 5000
    if ($2 >= 1 && $2 < 5000) { var %merit = 5000, %mername = Civil Offense, %medal = 0 }
  }
  if ($1 == 4) {
    var %type = Theft, %max = 25000
    if ($2 >= 1 && $2 < 1000) { var %merit = 1000, %mername = Candy Man, %medal = 1000, %medname = Sneak Thief (1) }
    if ($2 >= 1000 && $2 < 2500) { var %merit = 2500, %mername = Smile You're On Camera, %medal = 2500, %medname = Prowler (2) }
    if ($2 >= 2500 && $2 < 5000) { var %merit = 5000, %mername = Smokin' Barrels, %medal = 5000, %medname = Safe Cracker (3) } 
    if ($2 >= 5000 && $2 < 7500) { var %merit = 7500, %mername = Breaking and Entering, %medal = 7500, %medname = Marauder (4) }
    if ($2 >= 7500 && $2 < 10000) { var %merit = 10000, %mername = Stroke Bringer, %medal = 10000, %medname = Cat Burgler (5) }
    if ($2 >= 10000 && $2 < 12500) { var %merit = 0, %medal = 12500, %medname = Pilferer (6) }
    if ($2 >= 12500 && $2 < 15000) { var %merit = 0, %medal = 15000, %medname = Desperado (7) }
    if ($2 >= 15000 && $2 < 17500) { var %merit = 0, %medal = 17500, %medname = Rustler (8) }
    if ($2 >= 17500 && $2 < 20000) { var %merit = 0, %medal = 20000, %medname = Pick-Pocket (9) }
    if ($2 >= 20000 && $2 < 22500) { var %merit = 0, %medal = 22500, %medname = Vandal (10) }
    if ($2 >= 22500 && $2 < 25000) { var %merit = 0, %medal = 25000, %medname = Kleptomaniac (11) }
  }
  if ($1 == 5) {
    var %type = Auto Theft, %max = 10000
    if ($2 >= 1 && $2 < 200) { var %merit = 5000, %mername = Joy Rider, %medal = 200, %medname = Gone in 300 Seconds (1) }
    if ($2 >= 200 && $2 < 400) { var %merit = 5000, %mername = Joy Rider, %medal = 400, %medname = Gone in 240 Seconds (2) }
    if ($2 >= 400 && $2 < 600) { var %merit = 5000, %mername = Joy Rider, %medal = 600, %medname = Gone in 180 Seconds (3) }
    if ($2 >= 600 && $2 < 800) { var %merit = 5000, %mername = Joy Rider, %medal = 800, %medname = Gone in 120 Seconds (4) }
    if ($2 >= 800 && $2 < 1000) { var %merit = 5000, %mername = Joy Rider, %medal = 1000, %medname = Gone in 60 Seconds (5) }
    if ($2 >= 1000 && $2 < 1200) { var %merit = 5000, %mername = Joy Rider, %medal = 1200, %medname = Gone in 45 Seconds (6) }
    if ($2 >= 1200 && $2 < 1500) { var %merit = 5000, %mername = Joy Rider, %medal = 1500, %medname = Gone in 30 Seconds (7) }
    if ($2 >= 1500 && $2 < 2000) { var %merit = 5000, %mername = Joy Rider, %medal = 2000, %medname = Gone in 15 Seconds (8) } 
    if ($2 >= 2000 && $2 < 2500) { var %merit = 5000, %mername = Joy Rider, %medal = 2500, %medname = Booster (9) } 
    if ($2 >= 2500 && $2 < 3000) { var %merit = 5000, %mername = Joy Rider, %medal = 3000, %medname = Joyrider (10) } 
    if ($2 >= 3000 && $2 < 3500) { var %merit = 5000, %mername = Joy Rider, %medal = 3500, %medname = Super Booster (11) } 
    if ($2 >= 3500 && $2 < 4000) { var %merit = 5000, %mername = Joy Rider, %medal = 4000, %medname = Master Carjacker (12) }
    if ($2 >= 4000 && $2 < 4500) { var %merit = 5000, %mername = Joy Rider, %medal = 4500, %medname = Slim Jim (13) }
    if ($2 >= 4500 && $2 < 5000) { var %merit = 5000, %mername = Joy Rider, %medal = 5000, %medname = Novice Joy Rider (14) }
    if ($2 >= 5000 && $2 < 5500) { var %merit = 0, %medal = 5500, %medname = Novice Slim Jim (15) }
    if ($2 >= 5500 && $2 < 6000) { var %merit = 0, %medal = 6000, %medname = Professional Joy Rider (16) }
    if ($2 >= 6000 && $2 < 6500) { var %merit = 0, %medal = 6500, %medname = Professional Booster (17) }
    if ($2 >= 6500 && $2 < 7000) { var %merit = 0, %medal = 7000, %medname = Professional Slim Jim (18) }
    if ($2 >= 7000 && $2 < 8000) { var %merit = 0, %medal = 8000, %medname = Master Joy Rider (19) }
    if ($2 >= 8000 && $2 < 9000) { var %merit = 0, %medal = 9000, %medname = Master Booster (20) }
    if ($2 >= 9000 && $2 < 10000) { var %merit = 0, %medal = 10000, %medname = Master Slim Jim (21) }
  }
  if ($1 == 6) {
    var %type = Drug Deals, %max = 10000
    if ($2 >= 1 && $2 < 250) { var %merit = 5000, %mername = Escobar, %medal = 250, %medname = Drug Pusher (1) }
    if ($2 >= 250 && $2 < 500) { var %merit = 5000, %mername = Escobar, %medal = 500, %medname = Drug Runner (2) }
    if ($2 >= 500 && $2 < 1000) { var %merit = 5000, %mername = Escobar, %medal = 1000, %medname = Drug Dealer (3) }
    if ($2 >= 1000 && $2 < 2000) { var %merit = 5000, %mername = Escobar, %medal = 2000, %medname = Drug Lord (4) }
    if ($2 >= 2000 && $2 < 4000) { var %merit = 5000, %mername = Escobar, %medal = 4000, %medname = Candy Man (5) }
    if ($2 >= 4000 && $2 < 5000) { var %merit = 5000, %mername = Escobar, %medal = 6000, %medname = Connection (6) }
    if ($2 >= 5000 && $2 < 6000) { var %merit = 0, %medal = 6000, %medname = Connection (6) }
    if ($2 >= 6000 && $2 < 8000) { var %merit = 0, %medal = 8000, %medname = King Pin (7) }
    if ($2 >= 8000 && $2 < 10000) { var %merit = 0, %medal = 10000, %medname = Supplier (8) }
  }
  if ($1 == 7) {
    var %type = Computer Crimes, %max = 10000
    if ($2 >= 1 && $2 < 500) { var %merit = 1000, %mername = Bug, %medal = 500, %medname = Ub3rn00b Hacker (1) }
    if ($2 >= 500 && $2 < 1000) { var %merit = 1000, %mername = Bug, %medal = 1000, %medname = N00b Hacker (2) }
    if ($2 >= 1000 && $2 < 1500) { var %merit = 5000, %mername = We Have A Breach, %medal = 1500, %medname = 1337n00b Hacker (3) }
    if ($2 >= 1500 && $2 < 2000) { var %merit = 5000, %mername = We Have A Breach, %medal = 2000, %medname = Ph34r3dn00b Hacker (4) }
    if ($2 >= 2000 && $2 < 2500) { var %merit = 5000, %mername = We Have A Breach, %medal = 2500, %medname = Ph34r3d Hacker (5) }
    if ($2 >= 2500 && $2 < 3000) { var %merit = 5000, %mername = We Have A Breach, %medal = 3000, %medname = Ph34r3d1337 Hacker (6) }
    if ($2 >= 3000 && $2 < 3500) { var %merit = 5000, %mername = We Have A Breach, %medal = 3500, %medname = Ub3rph34r3d Hacker (7) }
    if ($2 >= 3500 && $2 < 4000) { var %merit = 5000, %mername = We Have A Breach, %medal = 4000, %medname = Ub3r Hacker (8) }
    if ($2 >= 4000 && $2 < 4500) { var %merit = 5000, %mername = We Have A Breach, %medal = 4500, %medname = 1337 Hacker (9) }
    if ($2 >= 4500 && $2 < 5000) { var %merit = 5000, %mername = We Have A Breach, %medal = 5000, %medname = Ub3r1337 Hacker (10) }
    if ($2 >= 5000 && $2 < 5500) { var %merit = 0, %medal = 5500, %medname = Key Puncher (11) }
    if ($2 >= 5500 && $2 < 6000) { var %merit = 0, %medal = 6000, %medname = Script Kid (12) }
    if ($2 >= 6000 && $2 < 7000) { var %merit = 0, %medal = 7000, %medname = Geek Speak (13) }
    if ($2 >= 7000 && $2 < 8000) { var %merit = 0, %medal = 8000, %medname = Techie (14) }
    if ($2 >= 8000 && $2 < 9000) { var %merit = 0, %medal = 9000, %medname = Cyber Punk (15) }
    if ($2 >= 9000 && $2 < 10000) { var %merit = 0, %medal = 10000, %medname = Programmer (16) }
  }
  if ($1 == 8) {
    var %type = Fraud, %max = 10000
    if ($2 >= 1 && $2 < 300) { var %merit = 5000, %mername = Fire Starter, %medal = 300, %medname = Fake (1) }
    if ($2 >= 300 && $2 < 600) { var %merit = 5000, %mername = Fire Starter, %medal = 600, %medname = Counterfeit (2) }
    if ($2 >= 600 && $2 < 900) { var %merit = 5000, %mername = Fire Starter, %medal = 900, %medname = Pretender (3) }
    if ($2 >= 900 && $2 < 1200) { var %merit = 5000, %mername = Fire Starter, %medal = 1200, %medname = Clandestine (4) }
    if ($2 >= 1200 && $2 < 1500) { var %merit = 5000, %mername = Fire Starter, %medal = 1500, %medname = Imposter (5) }
    if ($2 >= 1500 && $2 < 2000) { var %merit = 5000, %mername = Fire Starter, %medal = 2000, %medname = Pseudo (6) }
    if ($2 >= 2000 && $2 < 2500) { var %merit = 5000, %mername = Fire Starter, %medal = 2500, %medname = Imitation (7) }
    if ($2 >= 2500 && $2 < 3000) { var %merit = 5000, %mername = Fire Starter, %medal = 3000, %medname = Simulated (8) }
    if ($2 >= 3000 && $2 < 3500) { var %merit = 5000, %mername = Fire Starter, %medal = 3500, %medname = Hoax (9) }
    if ($2 >= 3500 && $2 < 4000) { var %merit = 5000, %mername = Fire Starter, %medal = 4000, %medname = Faux (10) }
    if ($2 >= 4000 && $2 < 5000) { var %merit = 5000, %mername = Fire Starter, %medal = 5000, %medname = Poser (11) }
    if ($2 >= 5000 && $2 < 6000) { var %merit = 0, %medal = 6000, %medname = Deception (12) }
    if ($2 >= 6000 && $2 < 7000) { var %merit = 0, %medal = 7000, %medname = Phony (13) }
    if ($2 >= 7000 && $2 < 8000) { var %merit = 0, %medal = 8000, %medname = Parody (14) }
    if ($2 >= 8000 && $2 < 9000) { var %merit = 0, %medal = 9000, %medname = Travesty (15) }
    if ($2 >= 9000 && $2 < 10000) { var %merit = 0, %medal = 10000, %medname = Pyro (16) }
  }
  if ($1 == 9) {
    var %type = Murder, %max = 10000
    if ($2 >= 1 && $2 < 1000) { var %merit = 5000, %mername = Professional, %medal = 1000, %medname = Beginner Assassin (1) }
    if ($2 >= 1000 && $2 < 2000) { var %merit = 5000, %mername = Professional, %medal = 2000, %medname = Novice Assassin (2) }
    if ($2 >= 2000 && $2 < 3000) { var %merit = 5000, %mername = Professional, %medal = 3000, %medname = Competant Assassin (3) }
    if ($2 >= 3000 && $2 < 4000) { var %merit = 5000, %mername = Professional, %medal = 4000, %medname = Elite Assassin (4) }
    if ($2 >= 4000 && $2 < 5000) { var %merit = 5000, %mername = Professional, %medal = 5000, %medname = Deadly Assassin (5) }
    if ($2 >= 5000 && $2 < 6000) { var %merit = 0, %medal = 6000, %medname = Lethal Assassin (6) }
    if ($2 >= 6000 && $2 < 7000) { var %merit = 0, %medal = 7000, %medname = Fatal Assassin (7) }
    if ($2 >= 7000 && $2 < 8000) { var %merit = 0, %medal = 8000, %medname = Trigger Man (8) }
    if ($2 >= 8000 && $2 < 9000) { var %merit = 0, %medal = 9000, %medname = Hit Man (9) }
    if ($2 >= 9000 && $2 < 10000) { var %merit = 0, %medal = 10000, %medname = Executioner (10) }
  }
  if ($1 == 10) {
    var %type = Other, %max = 5000
    if ($2 >= 1 && $2 < 5000) { var %merit = 5000, %mername = Find a Penny Pick it Up, %medal = 0 }
  }
  ; this weird if statement keeps the token structure constant, prevents weird errors in the returning sub
  if (%mername == $null) { var %mername = 0 } | if (%medname == $null) { var %medname = 0 } 
  return %type $+ $chr(124) $+ %max $+ $chr(124) $+ %merit $+ $chr(124) $+ %mername $+ $chr(124) $+ %medal $+ $chr(124) $+ %medname
}

; --------------------------------- drug calc functions ---------------------------------

; Adds a xanax od
alias -l tc.xanod {
  var %id = $tc.getid($1)
  if (%id != $null) {
    if ($exists($tc.psetfile)) { var %last = $tc.getset(%id,xanod) }
    tc.set %id xanod $tc.time
    if (%last != $null) { msg $2 $tc.col(1) $+ You have last overdosed on Xanax $+ $tc.col(2) $duration($calc($tc.time - %last)) $tc.col(1) $+ ago on $+ $tc.col(2) $asctime(%last, mmm d yyyy) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%last, h:nn:ss tt) $tc.col(1) $+ (GMT/TC Time). }
    else { msg $2 $tc.col(1) $+ I have recorded your overdose. You can keep track of how long ago it was using: $+ $tc.col(2) !xanod }    
    if ($readini($tc.cfgfile,n,Config,odrevive) == yes) { msg $2 $tc.col(3) $+ If anyone is feeling generous, please revive $+ $tc.col(2) $1 $+ $tc.col(3) $+ ! $+ $tc.col(2) $tc.revlink(%id) }    
  }
}

; Reads hermetic setting and returns. Used in a few places.
alias -l tc.getherm { return $readini($tc.cfgfile,n,Config,hermetic) }

; --------------------------------- other encyclopaedia functions ---------------------------------

; Book display - is done this way so multiple books can be displayed for one term.
; The data is separated by pipe characters since the data can have spaces in them,
; and mirc has problems with passing spaces. Need to reconstruct the input string, 
; then use gettok to split.
alias -l tc.bookdisp { 
  var %input = $1-$999, %chan = $gettok(%input,1,124), %title = $gettok(%input,2,124), %bene = $gettok(%input,3,124)
  msg %chan $tc.col(1) $+ Book: $+ $tc.col(2) %title $+ $tc.col(1) - Benefit: $+ $tc.col(2) %bene
}

; --------------------------------- api key stuff -------------------------------------

alias -l tc.getapikey {
  if ($readini($tc.cfgfile,n,API,enable) == e) { return $readini($tc.cfgfile,n,API,key) }
  else { return $null }
}

; --------------------------------- variable manipulation functions ---------------------------------

; seperates a number into neat little commas. Stolen off alchemy's bot, lol, I'm horrible at regexes.
alias -l tc.addComma { var %a, %b = $regsub($ticks,$1,/\G([+-]?\d+?)(?=(?:\d{3})++(?=\.\d++$|$))/g,\1 $+ $chr(44),%a) | return %a }

; strips all input ascii chars aside from decimals and numbers. Used to clean accidental/intentional letters in inputs that expect numbers.
alias -l tc.cleanN { 
  var %a = 0, %b = $1
  while (%a < 255) { if (%a != 46 && %a !isnum 48-57) { var %b = $remove(%b, $chr(%a)) } | inc %a }
  return %b
}

; This is used in cleaning current stat passives. 
; Ched for some reason used a fucking weird unicode instead of a -, so it should check for that.
alias -l tc.cleanStatPass {
  var %ptr = 1, %str = $1, %len = $len($1), %pos = $chr(45)
  while (%ptr <= %len) {
    var %chk = $mid(%str,%ptr,1)
    if (%chk isnum) { var %ret = %ret $+ %chk }
    if (%chk == $chr(43)) { var %pos = $null }
    inc %ptr
  }
  return %pos $+ %ret
}


; strips all HTML tags
alias -l tc.cleanH { return $regsubex($1,/^[^<]*>|<[^>]*>|<[^>]*$/g,$null) }

; strips leading and trailing whitespace
alias -l tc.cleanW { var %a, %b = $regsub($ticks,$1,^[ \t]+|[ \t]+$,$null,%a) | return %a }    

; encodes an input string into an url friendly string
alias -l tc.uencode { 
  var %a = $replace($1-,$chr(37),$+($chr(37),25)) | var %ptr = 32
  while (%ptr <= 126) {
    if (%ptr == 37) { inc %ptr | continue }
    if (%ptr >= 48 && %ptr <= 57) { inc %ptr | continue }
    if (%ptr >= 65 && %ptr <= 90) { inc %ptr | continue }
    if (%ptr >= 97 && %ptr <= 122) { inc %ptr | continue }
    var %a = $replace(%a,$chr(%ptr),$+($chr(37),$base(%ptr,10,16)))
    inc %ptr
  }  
  return %a
}

; Old TC time alias. 
; calculates using the offset set in the ACP. This is because $gmt is a broken pos that doesn't work properly with daylight savings sometimes.
; alias -l tc.time { var %adj = $readini($tc.cfgfile,n,Config,timeadj) | if (%adj == $null) { var %adj = 0 } | return $calc($ctime - (%adj * 60 * 60)) }

; New TC time alias. Uses $daylight to account for DST, which I didn't know existed.
alias -l tc.time { return $calc($gmt - $daylight) }

; converts hh:mm:ss into total seconds used for the timer command. If 0 is returned = invalid input.
alias -l tc.timeconv { 
  if ($1 == 0 || $calc($remove($1,$chr(58))) == 0) { return 0 }
  if ($1 !isnum && $gettok($1,2,58) == $null) { return 0 }
  if ($1 isnum) { if ($1 < 1) { return 0 } | else { return $1 } }
  else {
    var %hr = $gettok($1,1,58), %min = $gettok($1,2,58), %sec = $gettok($1,3,58)
    if (%sec == $null) { var %sec = 0 }
    if (%hr isnum && %min isnum && %sec isnum) { return $calc(((%hr * 60) + %min) * 60 + %sec) } 
    else { return 0 }   
  }  
}

alias -l tc.col { 
  if ($1 isnum) {
    if ($gettok(%tc.col,1,32) == y) { return  $+ $base($gettok(%tc.col,$calc($1 + 1),32),10,10,2) }
    else { if ($1 == 2) { return  } | else { return  } }
  }
  else { return $null }
}

alias -l tc.colid { return $tc.col($2) $+ $chr(91) $+ $tc.col($3) $+ $1 $+ $tc.col($2) $+ $chr(93) }

; --------------------------------- id lookup functions ---------------------------------

; Separate subs for returning IDs and names - replaced the old single return point.
alias -l tc.getid { return $hget(tc.ids,$1) }
alias -l tc.getname { 
  if ($hget(tc.idsname,$1) != $null) { return $hget(tc.idsname,$1) }
  else { return $hfind(tc.ids,$1).data }
}

; --------------------------------- chain bot functions ---------------------------------

alias -l tc.chainend {
  if (%tcchain.hits == $null || %tcchain.hits < 0) { set %tcchain.hits 0 }
  if (%tcchain.loss == $null) { set %tcchain.loss 0 }
  var %bonusresp 0
  if (%tcchain.hits > 9) { var %bonusresp = 10 }
  if (%tcchain.hits > 24) { var %bonusresp = $calc(10 + 25) }
  if (%tcchain.hits > 49) { var %bonusresp = $calc(10 + 25 + 50) }
  if (%tcchain.hits > 74) { var %bonusresp = $calc(10 + 25 + 50 + 75) }
  if (%tcchain.hits > 99) { var %bonusresp = $calc(10 + 25 + 50 + 75 + 100) }
  if ($1 == $null) { 
    var %hitsresp = $calc(%tcchain.startresp + (%tcchain.hits * 3)), %endresp = $calc(%hitsresp + %bonusresp)
    msg %tcchain.stchan $tc.col(1) $+ Ending chain $+ $tc.col(2) %tcchain.chid $tc.col(1) $+ automatically due to 20 minutes of inactivity. 
    msg %tcchain.stchan $tc.col(1) $+ The chain started at $+ $tc.col(2) $asctime(%tcchain.start,h:nn tt) $tc.col(1) $+ TC time, so we have been chaining for $+ $tc.col(2) $duration($calc($tc.time - %tcchain.start)) $+ $tc.col(1) $+ .
    msg %tcchain.stchan $tc.col(1) $+ We started with $+ $tc.col(2) $tc.addComma(%tcchain.startresp) $tc.col(1) $+ respect, using $+ $tc.col(2) 3 $tc.col(1) $+ respect per hit and a total of $+ $tc.col(2) %tcchain.hits $tc.col(1) $+ hits and $+ $tc.col(2) %tcchain.loss $tc.col(1) $+ losses, final respect should be around $+ $tc.col(2) $tc.addComma(%endresp) $+ $tc.col(1) $+ .
  }
  else { 
    var %endresp = $1
    msg %tcchain.stchan $tc.col(1) $+ Ending chain $+ $tc.col(2) %tcchain.chid $tc.col(1) $+ with a total of $+ $tc.col(2) %tcchain.hits $tc.col(1) $+ hits and $+ $tc.col(2) %tcchain.loss $tc.col(1) $+ losses.
    msg %tcchain.stchan $tc.col(1) $+ The chain started at $+ $tc.col(2) $asctime(%tcchain.start,h:nn tt) $tc.col(1) $+ TC time, so we have been chaining for $+ $tc.col(2) $duration($calc($tc.time - %tcchain.start)) $+ $tc.col(1) $+ .
    msg %tcchain.stchan $tc.col(1) $+ We started with $+ $tc.col(2) $tc.addComma(%tcchain.startresp) $tc.col(1) $+ respect and ended with $+ $tc.col(2) $tc.addComma(%endresp) $+ $tc.col(1) $+ , so our change was $+ $tc.col(2) $tc.addComma($calc(%endresp - %tcchain.startresp)) $tc.col(1) $+ respect! We averaged around $+ $tc.col(2) $tc.addComma($round($calc((%endresp - %tcchain.startresp) / %tcchain.hits),2)) $tc.col(1) $+ respect per hit.
  }
  if (%tcchain.hits > 0) {
    hadd tc.chain s %tcchain.startresp %endresp %tcchain.hits %tcchain.loss %tcchain.start $tc.time %tcchain.totrev
    hsave tc.chain %tcchain.log | hsave tc.chmonth %tcchain.cmlog
    writeini $tc.cfgfile Chain resp %endresp 
    writeini $tc.cfgfile Chain id %tcchain.chid      
    flushini $tc.cfgfile
    tc.chainpost %tcchain.stchan
    if (%tcchain.revtot > 0) {
      msg %tcchain.stchan $tc.col(1) $+ To see the list of revivers for this chain, type: $+ $tc.col(2) !revhist %tcchain.chid 
      var %revptr = 1 
      while (%revptr <= $numtok(%tcchain.revchan,44)) { 
        tc.chainrevpost $gettok(%tcchain.revchan,%revptr,44)
        inc %revptr 
      }
    }
  } 
  else {
    if ($exists(%tcchain.log)) { .remove %tcchain.log }
    msg %tcchain.stchan $tc.col(1) $+ There were no attacks recorded so nothing was saved.    
  }
  if ($readini($tc.cfgfile,n,Chain,postchains) == yes) {
    var %ptr = 1, %loops = $numtok(%tc.chchan,44)  
    if (%tcchain.hits < 1) { set %tcchain.hits $tc.col(7) $+ no }  
    while (%ptr <= %loops) {     
      if ($gettok(%tc.chchan,%ptr,44) ischan && $gettok(%tc.chchan,%ptr,44) != %tcchain.stchan) { 
        msg $gettok(%tc.chchan,%ptr,44) $tc.col(1) $+ The chain in $+ $tc.col(2) %tc.chain.stchan $tc.col(1) $+ has now ended with $+ $tc.col(2) %tcchain.hits $tc.col(1) $+ total hits.
      }
      inc %ptr 
    } 
  }
  if (%tcchain.revchan != $null) {
    var %ptr = 1, %loops = $numtok(%tcchain.revchan,44)  
    if (%tcchain.hits < 1) { set %tcchain.hits $tc.col(7) $+ no }  
    while (%ptr <= %loops) {     
      if ($gettok(%tcchain.revchan,%ptr,44) ischan && $gettok(%tcchain.revchan,%ptr,44) != %tcchain.stchan) { 
        msg $gettok(%tcchain.revchan,%ptr,44) $tc.col(1) $+ The chain has now ended with $+ $tc.col(2) %tcchain.hits $tc.col(1) $+ total hits.
      }
      inc %ptr 
    } 
  }
  .timer $+ 0.tcchain* off | unset %tcchain.* 
  if ($hget(tc.chain)) { hfree tc.chain }
  if ($hget(tc.chmonth)) { hfree tc.chmonth }
}

alias -l tc.chainpost {
  if ($2 == $null) { var %logfile = %tcchain.log }
  else { var %logfile = $tc.chlogfile($2) }
  if (!$exists(%logfile)) { halt }
  window -hn @chainpost | window -hn @chainpostsort
  hmake tc.chsumm | hload tc.chsumm %logfile
  var %ptr = 1, %loop = $hget(tc.chsumm,0).item
  while (%ptr <= %loop) {
    var %key = $hget(tc.chsumm,%ptr).item, %val = $hget(tc.chsumm,%key)
    if (%key != s) { aline @chainpost $tc.getname(%key) $gettok(%val,1,32) }    
    inc %ptr
  }
  filter -twwceu 2 32 @chainpost @chainpostsort
  msg $1 $tc.col(1) $+ List of Chainers:
  var %ptr = 1, %loop = $line(@chainpostsort,0), %hitstot = 0, %chanlinenum = 1
  while (%ptr <= %loop) {
    var %read = $line(@chainpostsort,%ptr)
    if ($gettok(%read,2,32) > 0) {
      var %hitstot = $calc(%hitstot + %atks)
      var %chanline = %chanline $+ $tc.col(2) $gettok(%read,1,32) $tc.col(1) $+ - $+ $tc.col(2) $gettok(%read,2,32) $tc.col(1) $+ hits $+ $chr(44)
      inc %chanlinenum
      if (%chanlinenum == 6) {
        msg $1 %chanline
        var %chanlinenum = 1
        var %chanline = $null
      }
    }
    inc %ptr
  }
  if (%chanline != $null) { msg $1 $left(%chanline,$calc($len(%chanline) - 1)) $+ $chr(46) } 
  .timer0.tcflag.spam 1 $tc.gcd noop
  window -c @chainpost | window -c @chainpostsort | hfree tc.chsumm
}

; Revive post
; $1 = chan, $2 = chain id
alias -l tc.chainrevpost {
  var %chan = $1, %revtotal = 0
  if ($2 == $null) { 
    var %logfile = %tcchain.log, %chid = %tcchain.chid
    if (%tcchain.revtot == 0) { msg $1 $tc.col(5) $+ There have been no revives recorded for this chain yet! | halt }
  }
  else { 
    var %logfile = $tc.chlogfile($2), %chid = $2
    if (!$exists(%logfile)) { msg $1 $tc.col(5) $+ No chain data for chain $+ $tc.col(2) %chid | halt }
  }
  if ($hget(tc.postrevtally)) { hfree tc.postrevtally }
  hmake tc.postrevtally 100 | hload tc.postrevtally %logfile
  window -hn @revtally | window -hn @revtallysort 
  var %chanline = $null, %chanlinenum = 1, %ptr = 1, %maxloop = $hget(tc.postrevtally,0).item
  while (%ptr <= %maxloop) {
    var %key = $hget(tc.postrevtally,%ptr).item, %val = $hget(tc.postrevtally,%key)
    if (%key != s) { aline @revtally $tc.getname(%key) $gettok(%val,5,32) }    
    inc %ptr
  }
  filter -twwceu 2 32 @revtally @revtallysort
  var %ptr = 1, %loop = $line(@revtallysort,0) | var %revtot = 0
  if (%loop > 0) {
    msg %chan $tc.col(1) $+ List of Revivers for chain $+ $tc.col(2) %chid $+ $tc.col(1) $+ :
    while (%ptr <= %loop) {
      var %read = $line(@revtallysort,%ptr), %name = $gettok(%read,1,32), %rev = $gettok(%read,2,32), %revtot = $calc(%revtot + %rev)
      if (%rev > 0) {
        var %chanline = %chanline $+ $tc.col(2) %name $tc.col(1) $+ - $+ $tc.col(2) %rev $tc.col(1) $+ revives $+ $chr(44)
        inc %chanlinenum
        inc %revtotal
        if (%chanlinenum == 6) {
          msg %chan %chanline
          var %chanlinenum = 1, %chanline = $null
        }
      }
      inc %ptr
    }
    if (%chanline != $null) { msg %chan $left(%chanline,$calc($len(%chanline) - 1)) $+ $chr(46) } 
    msg %chan $tc.col(1) $+ That's a total of $+ $tc.col(2) %revtotal $tc.col(1) $+ revivers and $+ $tc.col(2) %revtot $tc.col(1) $+ total revives! 
  } 
  else { msg %chan $tc.col(5) $+ There were no revives recorded for chain $+ $tc.col(2) %chid }
  window -c @revtally | window -c @revtallysort | hfree tc.postrevtally
}

alias -l tc.chaintwomin {
  msg %tcchain.stchan $tc.col(2) $+ Two minutes $tc.col(1) $+ remain! Someone should make a hit soon. 
  if (%tcchain.pinglist != $null) { msg %tcchain.stchan $tc.col(8) $+ Ping List: $+ $tc.col(4) %tcchain.pinglist }
}

; Adds offliner hits
; $1 = quantity of hits
alias -l tc.chainoffl { 
  set %tcchain.hits $calc(%tcchain.hits + $1) 
  if (%tcchain.hits > 0) {
    if ($readini($tc.cfgfile,n,Chain,upg) == 0) { var %chmin = 5 }
    elseif ($readini($tc.cfgfile,n,Chain,upg) == 1) { var %chmin = 5.5, %firstwarn = $tc.col(1) $+ Two minutes has elapsed since the last hit! $+ $tc.col(2) 3 $tc.col(1) $+ minutes and $+ $tc.col(2) 30 $tc.col(1) $+ seconds remain. }
    else { var %chmin = $calc(4 + $readini($tc.cfgfile,n,Chain,upg)) }
    var %dur = $calc(%chmin * 60) 
    if (%firstwarn == $null) { var %firstwarn = $tc.col(1) $+ Two minutes has elapsed since the last hit! $+ $tc.col(2) $floor($calc((%dur - 120) / 60)) $tc.col(1) $+ minutes remain. }
    .timer $+ 0.tcchain1 -co 1 120 msg %tcchain.stchan %firstwarn 
    .timer $+ 0.tcchain2 -co 1 $calc(%dur - 120) tc.chaintwomin
    .timer $+ 0.tcchain3 -co 1 $calc(%dur - 60) msg %tcchain.stchan $tc.col(7) $+ One minute remains! Someone should make a hit now!
    .timer $+ 0.tcchain4 -co 1 $calc(%dur - 30) .notice %tcchain.stchan $tc.col(7) $+ THIRTY SECONDS REMAIN, SOMEONE MAKE A HIT NOW!
    .timer $+ 0.tcchain5 -co 1 %dur msg %tcchain.stchan $tc.col(1) $+ Has the chain broken? Use $+ $tc.col(2) !chain off EndRespect $tc.col(1) $+ if so.
    .timer $+ 0.tcchainend -co 1 $calc(%dur + (20 * 60)) tc.chainend
    msg %tcchain.stchan $tc.col(1) $+ I have added $+ $tc.col(2) $tc.addComma($1) $tc.col(1) $+ new hits for offliners and reset the timers. $tc.col(8) $+ Chain Counter: $+ $tc.col(2) $tc.addComma(%tcchain.hits) $+ $tc.col(8) $+ /100. 
  }
  else { msg %tcchain.stchan $tc.col(1) $+ The first hit has been made by $+ $tc.col(7) an offliner $+ $tc.col(1) $+ ! One more hit is needed to start the chain timers. } 
}

; Chain control sub
; $1 = code: h = hit, l = loss, s = stalemate, d = dko
; $2 = chainers name
; $3 = quantity of attacks
; $4 = targets name (use ? if someone)
alias -l tc.chainadd {
  if ($hget(tc.atklist)) { .timer $+ 0.hitlistclr -co 1 $calc(20 * 60) tc.hitlistclr %tcchain.stchan }
  if ($hget(tc.atklist,$2)) { hadd -m tc.atklist $2 $calc($hget(tc.atklist,$2) - 1) }
  var %findid = $tc.getid($2)
  if (%findid == $null) { msg %tcchain.stchan $tc.col(5) $+ I do not know who $+ $tc.col(2) $2 $tc.col(5) $+ is, please add their ID to the bot using !addid. If you just want to add offliner hits to reset timers, use $+ $tc.col(2) !offl (Num) | halt }
  var %read = $hget(tc.chain,%findid)
  if (%read != $null) { var %phits = $gettok(%read,1,32), %ploss = $gettok(%read,2,32), %pdko = $gettok(%read,3,32), %psm = $gettok(%read,4,32), %prev = $gettok(%read,5,32) }
  if (%phits == $null) { var %phits = 0 } | if (%ploss == $null) { var %ploss = 0 } 
  if (%psm == $null) { var %psm = 0 } | if (%pdko == $null) { var %pdko = 0 } | if (%prev == $null) { var %prev = 0 }
  var %readcm = $hget(tc.chmonth,%findid)
  if (%readcm != $null) { var %cmhits = $gettok(%readcm,1,32), %cmloss = $gettok(%readcm,2,32), %cmdko = $gettok(%readcm,3,32), %cmsm = $gettok(%readcm,4,32) }
  if (%cmhits == $null) { var %cmhits = 0 } | if (%cmloss == $null) { var %cmloss = 0 } 
  if (%cmsm == $null) { var %cmsm = 0 } | if (%cmdko == $null) { var %cmdko = 0 }
  var %lhc = %tcchain.hits
  if ($1 = h) {
    set %tcchain.hits $calc(%tcchain.hits + $3) | var %phits = $calc(%phits + $3), %cmhits = $calc(%cmhits + $3)
    if (%tcchain.hits > 0) {
      if ($readini($tc.cfgfile,n,Chain,upg) == 0) { var %chmin = 5 }
      elseif ($readini($tc.cfgfile,n,Chain,upg) == 1) { var %chmin = 5.5, %firstwarn = $tc.col(1) $+ Two minutes has elapsed since the last hit! $+ $tc.col(2) 3 $tc.col(1) $+ minutes and $+ $tc.col(2) 30 $tc.col(1) $+ seconds remain. }
      else { var %chmin = $calc(4 + $readini($tc.cfgfile,n,Chain,upg)) }
      var %dur = $calc(%chmin * 60) 
      if (%firstwarn == $null) { var %firstwarn = $tc.col(1) $+ Two minutes has elapsed since the last hit! $+ $tc.col(2) $floor($calc((%dur - 120) / 60)) $tc.col(1) $+ minutes remain. }
      if (!%tcchain.endless) {
        .timer $+ 0.tcchain1 -co 1 120 msg %tcchain.stchan %firstwarn 
        .timer $+ 0.tcchain2 -co 1 $calc(%dur - 120) tc.chaintwomin
        .timer $+ 0.tcchain3 -co 1 $calc(%dur - 60) msg %tcchain.stchan $tc.col(7) $+ One minute remains! Someone should make a hit now!
        .timer $+ 0.tcchain4 -co 1 $calc(%dur - 30) .notice %tcchain.stchan $tc.col(7) $+ THIRTY SECONDS REMAIN, SOMEONE MAKE A HIT NOW!
        .timer $+ 0.tcchain5 -co 1 %dur msg %tcchain.stchan $tc.col(1) $+ Has the chain broken? Use $+ $tc.col(2) !chain off EndRespect $tc.col(1) $+ if so.
        .timer $+ 0.tcchainend -co 1 $calc(%dur + (20 * 60)) tc.chainend %tcchain.stchan
        .timer $+ 0.hitlistclr -co 1 $calc(%dur + (20 * 60)) tc.hitlistclr %tcchain.stchan
      }
      if ($3 > 1) { msg %tcchain.stchan $tc.col(1) $+ Nice! $+ $tc.col(2) $2 $tc.col(1) $+ has made $+ $tc.col(2) $tc.addComma($3) $tc.col(1) $+ new hits for a total of $+ $tc.col(2) $tc.addComma(%phits) $tc.col(1) $+ attacks this chain! $tc.col(8) $+ Chain Counter: $+ $tc.col(2) $tc.addComma(%tcchain.hits) $+ $tc.col(8) $+ /100. }
      else { 
        if ($4 != $chr(63)) {
          msg %tcchain.stchan $tc.col(1) $+ Nice! $+ $tc.col(2) $2 $tc.col(1) $+ just beat down $+ $tc.col(6) $4 $tc.col(1) $+ for a total of $+ $tc.col(2) $tc.addComma(%phits) $tc.col(1) $+ attacks this chain! $tc.col(8) $+ Chain Counter: $+ $tc.col(2) $tc.addComma(%tcchain.hits) $+ $tc.col(8) $+ /100. 
        }
        else {
          msg %tcchain.stchan $tc.col(1) $+ Nice! $+ $tc.col(2) $2 $tc.col(1) $+ has made a hit for a total of $+ $tc.col(2) $tc.addComma(%phits) $tc.col(1) $+ attacks this chain! $tc.col(8) $+ Chain Counter: $+ $tc.col(2) $tc.addComma(%tcchain.hits) $+ $tc.col(8) $+ /100.       
        }
      }
    }
    else { msg %tcchain.stchan $tc.col(1) $+ The first hit has been made by $+ $tc.col(2) $2 $+ $tc.col(1) $+ ! One more hit is needed to start the chain timers. } 
  }
  if (%tcchain.hits < 1) { var %hitpst = zero } | else { var %hitpst = $tc.col(2) $+ $tc.addComma(%tcchain.hits) $+ $tc.col(7) $+ /100 }
  if ($1 = l) {
    inc %tcchain.loss | inc %ploss | inc %cmloss
    if ($4 != $chr(63)) {
      msg %tcchain.stchan $tc.col(7) $+ Damn, $+ $tc.col(2) $2 $tc.col(7) $+ caught a beatdown from $4 $+ ! Chain counter is still at: %hitpst $+ !
    } 
    else {
      msg %tcchain.stchan $tc.col(7) $+ Damn, $+ $tc.col(2) $2 $tc.col(7) $+ has lost! Chain counter is still at: %hitpst $+ !    
    }
  }
  if ($1 = d) {
    inc %tcchain.loss | inc %pdko | inc %cmdko
    msg %tcchain.stchan $tc.col(7) $+ Damn, $+ $tc.col(2) $2 $tc.col(7) $+ has DKOed! Chain counter is still at: %hitpst $+ !
  }
  if ($1 = s) {
    inc %tcchain.loss | inc %psm | inc %cmsm
    msg %tcchain.stchan $tc.col(7) $+ Damn, $+ $tc.col(2) $2 $tc.col(7) $+ has stalemated! Chain counter is still at: %hitpst $+ !
  }
  hadd tc.chain %findid %phits %ploss %pdko %psm %prev
  hadd tc.chmonth %findid %cmhits %cmloss %cmdko %cmsm
  if ((%lhc == 4 && %tcchain.hits == 5) || (%lhc == 19 && %tcchain.hits == 20) || (%lhc == 44 && %tcchain.hits == 45) || (%lhc == 69 && %tcchain.hits == 70) || (%lhc == 94 && %tcchain.hits == 95)) { msg %tcchain.stchan $tc.col(8) $+ Note: The $+ $tc.col(2) $calc(%tcchain.hits + 5) $+ th $tc.col(8) $+ bonus attack is coming up in $+ $tc.col(2) five $tc.col(8) $+ attacks. }
  if ((%lhc == 6 && %tcchain.hits == 7) || (%lhc == 21 && %tcchain.hits == 22) || (%lhc == 46 && %tcchain.hits == 47) || (%lhc == 71 && %tcchain.hits == 72) || (%lhc == 96 && %tcchain.hits == 97)) { 
    if ($readini($tc.cfgfile,n,Chain,bonus) == $null) { var %bonusfact = $tc.col(5) $+ not set } | else { var %bonusfact = $readini($tc.cfgfile,n,Chain,bonus) }
    msg %tcchain.stchan $tc.col(8) $+ Plan the $+ $tc.col(2) $calc(%tcchain.hits + 3) $+ th $tc.col(8) $+ bonus hit now, it is in $+ $tc.col(2) three $tc.col(8) $+ attacks! Bonus faction is: $+ $tc.col(2) %bonusfact 
  }
  if ((%lhc == 8 && %tcchain.hits == 9) || (%lhc == 23 && %tcchain.hits == 24) || (%lhc == 48 && %tcchain.hits == 49) || (%lhc == 73 && %tcchain.hits == 74) || (%lhc == 98 && %tcchain.hits == 99)) { 
    if ($readini($tc.cfgfile,n,Chain,bonus) == $null) { msg %tcchain.stchan $tc.col(4) $+ Careful! The $+ $tc.col(2) $calc(%tcchain.hits + 1) $+ th $tc.col(4) $+ bonus hit is $+ $tc.col(2) next attack $+ $tc.col(4) $+ , make sure it is on the correct faction! }  
    else { msg %tcchain.stchan $tc.col(4) $+ Careful! The $+ $tc.col(2) $calc(%tcchain.hits + 1) $+ th $tc.col(4) $+ bonus hit is $+ $tc.col(2) next attack $+ $tc.col(4) $+ , make sure it is on $+ $tc.col(2) $readini($tc.cfgfile,n,Chain,bonus) $+ $tc.col(4) $+ ! }
  }
  if ((%lhc == 9 && %tcchain.hits == 10) || (%lhc == 24 && %tcchain.hits == 25) || (%lhc == 49 && %tcchain.hits == 50) || (%lhc == 74 && %tcchain.hits == 75) || (%lhc == 99 && %tcchain.hits == 100)) { msg %tcchain.stchan $tc.col(1) $+ Well done $+ $tc.col(2) $2 $+ $tc.col(1) $+ ! You and the faction received an extra $+ $tc.col(2) %tcchain.hits $tc.col(1) $+ respect! }
}

alias -l tc.chainstalk { 
  if ($readini($tc.cfgfile,n,Chain,stalk) != $null) { msg $1 $tc.col(1) $+ Stalkers: $+ $tc.col(2) $readini($tc.cfgfile,n,Chain,stalk) } 
  .timer $+ 0.tcchainstalkers -co 1 $calc(20 * 60) tc.chainstalk $1
}

alias -l tc.chainsumm {
  var %summ = $tc.col(1) $+ The chain is currently on $tc.col(2) $+ $tc.addComma(%tcchain.hits) $+ $tc.col(1) attacks
  if ($readini($tc.cfgfile,n,Chain,bonus) != $null) { var %summ = %summ $+ $tc.col(1) $+ , bonuses go on: $tc.col(2) $+ $readini($tc.cfgfile,n,Chain,bonus) }
  if ($readini($tc.cfgfile,n,Chain,clead) != $null) { var %summ = %summ $+ $tc.col(1) $+ , the current chain leader is: $tc.col(2) $+ $readini($tc.cfgfile,n,Chain,clead) }
  if ($readini($tc.cfgfile,n,Chain,stalk) != $null) { var %summ = %summ $+ $tc.col(1) $+ , stalkers are: $tc.col(2) $+ $readini($tc.cfgfile,n,Chain,stalk) }
  return %summ $+ $tc.col(1) $+ $chr(46)
}

; --------------------------------- socket code ---------------------------------

; format post for youtube links
; $1- = title info
alias -l tc.ytpost { return 00,04YouTube! 04- $remove($1-,- YouTube) }

on *:sockopen:youtube:{
  if ($sockerr) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    sockwrite -nt $sockname GET %tc.ytlink HTTP/1.1
    sockwrite -nt $sockname Host: www.youtube.com
    sockwrite -nt $sockname $crlf
  }
}

on *:sockread:youtube:{
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    var %read | sockRead %read | var %readln $remove(%read, $chr(9))  
    ;write $qt($scriptdirtest.htm) %readln        
    if ($regex(%readln,\Q<title>\E(.+?)\Q</title>\E)) { 
      msg %tc.ytchan $tc.ytpost($regml(1)) 
      unset %tc.yt*
      sockclose youtube
    }
  }
}

on *:sockclose:youtube:{ unset %tc.yt* }

on *:sockopen:spyprof:{
  if ($sockerr) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    if (%tc.spy != $null) { var %table = spy. $+ %tc.spy, %addedby = %tc.spy }
    else { var %table = spy. $+ %tc.oldspy, %addedby = %tc.oldspy }   
    sockwrite -nt $sockname GET /user/ $+ $hget(%table,id) $+ ?selections=profile&key= $+ $tc.getapikey HTTP/1.1
    sockwrite -nt $sockname Host: api.torn.com
    sockwrite -nt $sockname $crlf
  }
}

on *:sockread:spyprof:{
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    var %read | sockRead %read | var %readln $remove(%read, $chr(9))  
    ;write $qt($scriptdirtest.htm) %readln
    if (%tc.spy != $null) { var %table = spy. $+ %tc.spy, %addedby = %tc.spy }
    else { var %table = spy. $+ %tc.oldspy, %addedby = %tc.oldspy } 
    if ($regex(%readln,error":"(.+?)")) { 
      msg %tc.spychan $tc.col(5) $+ API Error: $regml(1) 
      tc.spywrite
    }
    if ($regex(%readln,level":(.+?)")) { set %tcsprof.lvl $tc.cleanN($regml(1)) }    
    if ($regex(%readln,faction_name":"(.+?)")) { set %tcsprof.fact $tc.cleanW($regml(1)) }
    if ((%tcsprof.lvl != $null) && (%tcsprof.fact != $null)) { tc.spywrite }
  }
}

on *:sockclose:spyprof:{ tc.spywrite }

alias -l tc.profstr { 
  ; broke it up to make it easier to read.

  ; check for online/offline status (api doesnt return it, so determine it)
  if ($gettok(%tcprof.last,2,32) == minute || $gettok(%tcprof.last,2,32) == minutes) {
    if ($gettok(%tcprof.last,1,32) <= 15) { set %tcprof.ol $tc.col(6) $+ [On] }
    else { set %tcprof.ol $tc.col(7) $+ [Off] }
  }
  else { set %tcprof.ol $tc.col(7) $+ [Off] }

  ; fix a few odd chars
  set %tcprof.fact $replace(%tcprof.fact,&#33;,$chr(33))
  set %tcprof.fact $replace(%tcprof.fact,&#39;,$chr(39))
  set %tcprof.fact $replace(%tcprof.fact,&#40;,$chr(40))
  set %tcprof.fact $replace(%tcprof.fact,&#41;,$chr(41))

  ; then replace a few longer strings with shorter forms
  set %tcprof.status $replace(%tcprof.status,In hospital,Hosp)
  set %tcprof.status $remove(%tcprof.status,- Hospitalized)
  set %tcprof.status $replace(%tcprof.status,In jail,Jail)
  set %tcprof.status $replace(%tcprof.status,In federal jail,Fed)
  set %tcprof.status $replace(%tcprof.status,Was caught trying to break out,Caught busting)  
  set %tcprof.status $replace(%tcprof.status,currently okay,okay)
  set %tcprof.status $replace(%tcprof.status,1 hrs,1 hr)
  set %tcprof.status $replace(%tcprof.status,1 mins,1 min)
  set %tcprof.status $replace(%tcprof.status,$+($chr(34),$chr(44),$chr(34)),$+($chr(32),$chr(45),$chr(32)))
  set %tcprof.last $replace(%tcprof.last,minutes,mins,minute,min)
  set %tcprof.prop $replace(%tcprof.prop,Private Island,PI)

  if ($right(%tcprof.status,1) == $chr(45)) { set %tcprof.status $left(%tcprof.status,-1) }

  ; make life colored
  if (%tcprof.lifecur <= 1) { var %lifecol = 7 } 
  elseif (%tcprof.lifecur == %tcprof.lifemax) { var %lifecol = 6 }
  else { var %lifecol = 2 }

  ; make the age display more useful
  if (%tcprof.age >= 365) { 
    var %yr = 0
    var %dy = %tcprof.age
    while (%dy >= 365) {
      inc %yr
      var %dy = $calc(%dy - 365)
    }
    if (%dy = 0) { var %agestring = $tc.col(6) $+ %yr $+ y }
    else { var %agestring = %yr $+ y %dy $+ d }
  }
  else { var %agestring = $tc.addComma(%tcprof.age) }

  ; add a percentage to life
  var %lifeperc $round($calc((%tcprof.lifecur / %tcprof.lifemax) * 100),0)

  ; colorize specific parts if needed
  if ((%tcprof.fact == None) && (%tcprof.facrank == None)) { 
    var %facstr $tc.col(7) $+ None 
  }
  else { 
    if ($readini($tc.cfgfile,Config,faction) == %tcprof.fact) { var %faccol 6 }
    else { var %faccol 2 }
    var %facstr $tc.col(%faccol) $+ %tcprof.facrank $tc.col(1) $+ of $+ $tc.col(%faccol) %tcprof.fact 
  }
  if (%tcprof.spname == None && %tcprof.spdur == 0) { var %spstr $tc.col(7) $+ None }
  else { var %spstr $tc.col(2) $+ %tcprof.spname $tc.col(8) $+ (for %tcprof.spdur $+ d) }

  if (okay isin %tcprof.status) { set %tcprof.status $tc.col(6) $+ %tcprof.status }
  else if (Jail for isin %tcprof.status) { set %tcprof.status $tc.col(7) $+ %tcprof.status }
  else if (Fed for isin %tcprof.status) { set %tcprof.status $tc.col(7) $+ %tcprof.status }
  else if (Hosp for isin %tcprof.status) { set %tcprof.status $tc.col(7) $+ %tcprof.status }  
  else { set %tcprof.status $tc.col(2) $+ %tcprof.status }  

  ; now format output string
  var %out = $tc.col(2) $+ %tcprof.name $tc.colid(%tcprof.id,1,2) %tcprof.ol
  if (%tcprof.don == 1) { var %out = %out $tc.col(6) $+ $chr(42) }
  var %out = %out $+ $tc.col(1) ( $+ $tc.col(2) $+ %tcprof.sex
  var %out = %out $+ $tc.col(1) $+ , Level: $+ $tc.col(2) %tcprof.lvl  
  var %out = %out $+ $tc.col(1) $+ , Title: $+ $tc.col(2) %tcprof.rank
  var %out = %out $+ $tc.col(1) $+ , Age: $+ $tc.col(2) %agestring 
  if (%tcprof.age >= 365) { var %out = %out $tc.col(8) $+ ( $+ %tcprof.age $+ ) }
  var %out = %out $+ $tc.col(1) $+ , Spouse: %spstr
  var %out = %out $+ $tc.col(1) $+ , Prop: $+ $tc.col(2) %tcprof.prop
  var %out = %out $+ $tc.col(1) $+ , Fac: %facstr 
  var %out = %out $+ $tc.col(1) $+ , Life: $+ $tc.col(%lifecol) $tc.addComma(%tcprof.lifecur) $+ $tc.col(1) $+ / $+ $tc.col(%lifecol) $+ $tc.addComma(%tcprof.lifemax) $tc.col(8) $+ ( $+ %lifeperc $+ $chr(37) $+ )
  var %out = %out $+ $tc.col(1) $+ , A: $+ $tc.col(2) $+ %tcprof.award $+ $tc.col(1) $+ /F: $+ $tc.col(6) $+ %tcprof.friend $+ $tc.col(1) $+ /E: $+ $tc.col(7) $+ %tcprof.enemy 
  var %out = %out $+ $tc.col(1) $+ , Seen: $+ $tc.col(2) %tcprof.last
  var %out = %out $+ $tc.col(1) $+ , Status: %tcprof.status
  var %out = %out $+ $tc.col(1) $+ ) 
  return %out
}

;
; API profile grabber. New hotness.
;

on *:sockopen:apiprofile:{
  if ($sockerr) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    sockwrite -nt apiprofile GET /user/ $+ %tcprof.find $+ ?selections=profile&key= $+ $tc.getapikey HTTP/1.1
    sockwrite -nt apiprofile Host: api.torn.com
    sockwrite -nt apiprofile $crlf
  }
}

on *:sockread:apiprofile:{
  if ($sockerr) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    var %read | sockRead %read | var %readln $remove(%read, $chr(9))  
    ;write $qt($scriptdir $+ test.txt) %readln
    if ($regex(%readln,error":"(.+?)")) { set %tcprof.err $regml(1) }
    if ($regex(%readln,rank":"(.+?)")) { set %tcprof.rank $regml(1) }
    if ($regex(%readln,level":(.+?)")) { set %tcprof.lvl $remove($regml(1),$chr(44)) }
    if ($regex(%readln,gender":"(.+?)")) { set %tcprof.sex $regml(1) }
    if ($regex(%readln,property":"(.+?)")) { set %tcprof.prop $regml(1) }
    if ($regex(%readln,status":\["(.+?)"\])) { set %tcprof.status $tc.cleanH($regml(1)) }
    if ($regex(%readln,awards":(.+?)")) { set %tcprof.award $tc.cleanN($regml(1)) }
    if ($regex(%readln,friends":(.+?)")) { set %tcprof.friend $tc.cleanN($regml(1)) }
    if ($regex(%readln,enemies":(.+?)")) { set %tcprof.enemy $tc.cleanN($regml(1)) }
    if ($regex(%readln,age":(.+?)")) { set %tcprof.age $tc.cleanN($regml(1)) }
    if ($regex(%readln,donator":1)) { set %tcprof.don 1 }
    if ($regex(%readln,player_id":(.+?)\")) { set %tcprof.id $tc.cleanN($regml(1)) }
    if ($regex(%readln,name":"(.+?)")) { set %tcprof.name $regml(1) }    
    if ($regex(%readln,last_action":"(.+?)")) { set %tcprof.last $regml(1) }
    if ($regex(%readln,current":(.+?)")) { set %tcprof.lifecur $tc.cleanN($regml(1)) }
    if ($regex(%readln,maximum":(.+?)")) { set %tcprof.lifemax $tc.cleanN($regml(1)) }
    if ($regex(%readln,faction_name":"(.+?)")) { set %tcprof.fact $regml(1) }
    if ($regex(%readln,faction":{"position":"(.+?)")) { set %tcprof.facrank $regml(1) }
    if ($regex(%readln,spouse_name":"(.+?)")) { set %tcprof.spname $regml(1) }
    if ($regex(%readln,duration":(.+?)})) { set %tcprof.spdur $tc.cleanN($regml(1)) }
    if ($regex(%readln,}})) { tc.profclose 1 }
  }
}

; calls on close socket or at the end of a string, whichever comes first.
; helps avoid the timeout glitch. If called with $1 == 1, it forces a sockclose.
alias -l tc.profclose {
  if (%tcprof.err != $null) { msg %tcprof.chan $tc.col(5) $+ Error: $+ $tc.col(2) %tcprof.err } 
  else { 
    if (%tcprof.name != $null && %tcprof.id != $null) {
      msg %tcprof.chan $tc.profstr 
      msg %tcprof.chan $tc.col(1) $+ Link: $+ $tc.col(2) https://www.torn.com/profiles.php?XID= $+ %tcprof.id
    }
    else { msg %tcprof.chan $tc.col(5) $+ Sorry, the API returned gibberish that I could not parse properly. Please try again. }
  }
  if (%tcprof.name != $null && %tcprof.id != $null) {
    if ($readini($tc.cfgfile,Config,findadd) == yes || $strip(%tcprof.fact) == $readini($tc.cfgfile,Config,faction) ) { 
      var %retid = $tc.getid(%tcprof.name)
      if (%retid == $null) { 
        hadd -m tc.ids %tcprof.name %tcprof.id            
        hsave tc.ids $tc.idsfile
        msg %tcprof.chan $tc.col(1) $+ Automatically adding $+ $tc.col(2) %tcprof.name $tc.colid(%tcprof.id,1,2) $tc.col(1) $+ to the bot.
      }
    }
  }
  if ($1 == 1) { sockclose apiprofile }
  unset %tcprof.* | unset %tcprofsw.*
}

on *:sockclose:apiprofile:{ tc.profclose }

; --------------------------------- channel join code ---------------------------------

on *:join:%tc.chan: { 
  var %id = $tc.getid($nick) 
  if (%id != $null) {
    if ($gettok($tc.getset(%id,bday),1,32) == $asctime($tc.time,mmm) && $gettok($tc.getset(%id,bday),2,32) == $asctime($tc.time,d)) {
      var %yr = $gettok($tc.getset(%id,bday),3,32)
      if (%yr != $null) { var %bdaymsg = $tc.col(4) $+ Happy $+ $tc.col(2) $ord($calc($asctime($tc.time,yyyy) - %yr)) $tc.col(4) $+ Birthday $+ $tc.col(2) $nick $+ $tc.col(4) $+ ! }
      else { var %bdaymsg $tc.col(4) $+ Happy Birthday $+ $tc.col(2) $nick $+ $tc.col(4) $+ ! }
      if ($findtok(%tc.chan,$chan,1,44) != $null) { msg $chan %bdaymsg }
    }
  }
  if ($chan == %tcchain.stchan && %tcchain.hits > 0) {     
    .notice $nick $tc.col(1) $+ Welcome $tc.col(2) $+ $nick $+ $tc.col(1) $+ ! $tc.chainsumm
  }
}     

; --------------------------------- channel text code ---------------------------------

on *:text:*:#: { 
  ;
  ; run config file check sub first on any text. Yes it's spammy, it's meant to be.
  ;
  tc.cfgcheck

  ;
  ; Certain commands we want to allow control codes through. Rest can be stripped out to improve parser legibility.
  ;
  var %nostripcomm !banker !bankers !setbanker !setbankers
  if ($istok(%nostripcomm,$1,32)) {
    tokenize 32 $replace($1-, $chr(9), $chr(32), $chr(194), $chr(32), $chr(160), $chr(32))
  }
  else {
    tokenize 32 $strip($replace($1-, $chr(9), $chr(32), $chr(194), $chr(32), $chr(160), $chr(32)))
  }

  if (($eval($findfile,0) isin $1-) && ($chr(40) isin $1-)) {
    msg # $tc.col(5) $+ Where's the kaboom? There was supposed to be an earth-shattering kaboom!
    .timer 1 1 msg # $tc.col(5) $+ My Illudium Q-36 Explosive Space Modulator!
    .timer 1 1 .mode # +b $address($nick,3)
    .timer 1 2 .kick # $nick That creature has stolen the space modulator!
    halt
  }

  ;
  ; revive channel code, needs to be outside of the master chan list stuff
  ;
  if ($findtok($+(%tcchain.stchan,$chr(44),%tcchain.revchan),#,1,44)) {
    if ($1 == !revfail) {
      if (# == %tcchain.stchan) { msg # $tc.col(5) $+ Erm, ok? Tell them yourself? | halt }
      if ($2 == $null) { msg # $tc.col(3) $+ Notifies in the chain channel that a person needs to check their revive settings. $tc.col(1) $+ Usage: $+ $tc.col(2) !revfail Name }      
      else { 
        var %ret = $tc.getid($nick)
        if (%ret != $null) {
          msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , thank you for trying; a note has been sent back to the chaining channel.
          msg %tcchain.stchan $tc.col(2) $+ $2 $+ $tc.col(4) $+ , please check your revive settings; $tc.col(2) $+ $nick $tc.colid(%ret,4,2) $tc.col(4) $+ cannot revive you! Maybe you need to add him/her to friends list? $+ $tc.col(2) http://www.torn.com/friendlist.php#/p=add&XID= $+ %ret
        }
        else {
          msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , thank you for trying; a note has been sent back to the chaining channel. Also, please make sure your ID is in the bot as I do not have one for you. You may not be paid for your revives without it added!
          msg %tcchain.stchan $tc.col(2) $+ $2 $+ $tc.col(4) $+ , please check your revive settings; $tc.col(2) $+ $nick $tc.col(4) $+ cannot revive you! Unfortunately, I do not have his/her ID so I cannot provide an "add to friends list" link for you.
        }
      }
    }
    if ($1-2 == You revived) {
      var %findid = $tc.getid($nick)
      if (%findid == $null) { msg # $tc.col(5) $+ I do not know who $+ $tc.col(2) $nick $tc.col(5) $+ is, please add your ID to the bot using !addid. Otherwise, you will not be tracked/paid. | halt }
      var %read = $hget(tc.chain,%findid)
      if (%read != $null) { var %phits = $gettok(%read,1,32), %ploss = $gettok(%read,2,32), %pdko = $gettok(%read,3,32), %psm = $gettok(%read,4,32), %prev = $gettok(%read,5,32) }
      else { var %phits = 0, %ploss = 0, %psm = 0, %pdko = 0, %prev = 0 }
      inc %prev | inc %tcchain.revtot
      hadd tc.chain %findid %phits %ploss %psm %pdko %prev
      if ($readini($tc.cfgfile,n,Chain,revnote) == yes) { .notice $nick Thank you, your revive has been recorded. }
    }
  }
  var %revchanlist = %tc.chan
  if (%tc.chchan != $null) { var %revchanlist = $+(%revchanlist,$chr(44),%tc.chchan) }
  if (%tcchain.revchan != $null) { var %revchanlist = $+(%revchanlist,$chr(44),%tcchain.revchan) }
  if ($findtok(%revchanlist,#,1,44)) {
    if ($1 == !revid) {
      if ($2 == $null && $2 !isnum) { msg # $tc.col(3) $+ Posts a revive link for a specified id. $tc.col(1) $+ Use: $+ $tc.col(2) !revid ID | halt }
      else { msg # $tc.col(1) $+ Revive Link for ID $+ $tc.col(2) $tc.colid($2,1,2) $+ $tc.col(1) $+ : $+ $tc.col(2) $tc.revlink($2)
      }
    }
    if ($1 == !revive) {
      if ($2 != $null) { var %name = $2 }
      else { var %name = $nick }
      var %ret = $tc.getid(%name)
      if (%ret != $null) { 
        msg # $tc.col(1) $+ Revive $+ $tc.col(2) %name $+ $tc.col(1) $+ ! $+ $tc.col(2) $tc.revlink(%ret)
        if ($findtok($+(%tcchain.stchan,$chr(44),%tcchain.revchan),#,1,44)) { 
          var %ptr = 1, %loops = $numtok(%tcchain.revchan,44)
          while (%ptr <= %loops) { 
            if ($gettok(%tcchain.revchan,%ptr,44) ischan && $gettok(%tcchain.revchan,%ptr,44) != #) { 
              msg $gettok(%tcchain.revchan,%ptr,44) $tc.col(1) $+ Revive $+ $tc.col(2) %name $+ $tc.col(1) $+ ! $+ $tc.col(2) $tc.revlink(%ret)
            }
            inc %ptr 
          }
        }
      } 
      else { msg # $tc.col(5) $+ Sorry $+ $tc.col(2) %name $+ $tc.col(5) $+ , I have no clue who you are! }
    }    
    if ($1 == !revhist) {
      if (!$timer(0.tcflag.spam)) {      
        if ($2 isnum) {                 
          tc.chainrevpost # $2 
          set %tcflag.revspam 1 | .timer $+ 0.tcflag.revspam -o 1 $tc.gcd unset %tcflag.revspam          
        }
        elseif (%tcchain.stchan != $null) { 
          tc.chainrevpost # 
          .timer $+ 0.tcflag.spam 1 $tc.gcd noop
        }
        else { msg # $tc.col(3) $+ Posts the reviver history for a specific chain. $tc.col(1) $+ Usage: $+ $tc.col(2) !revhist ChainID | halt }   
      }
      else { msg # $tc.col(5) $+ No can do, too many requests too often. You can try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds. }
    } 
  } 

  ;
  ; and now, master channel code
  ;
  if ($findtok(%tc.chan,#,44) != $null) {    
    if ($readini($tc.cfgfile,n,Access,min) == o) { if ($nick !isop #) { halt } }
    elseif ($readini($tc.cfgfile,n,Access,min) == h) { if ($nick !isop # && $nick !ishop #) { halt } }
    elseif ($readini($tc.cfgfile,n,Access,min) == v) { if ($nick !isop # && $nick !ishop # && $nick !isvoice #) { halt } }
    elseif ($readini($tc.cfgfile,n,Access,min) == n) { }
    else { echo -a 04Minimum access level not set, check config panel! | halt }

    ; --------------------------------- ID adding/changing/deleting ---------------------------------

    if ($1 == !addid) {
      if ($2 == $null) { 
        msg # $tc.col(3) $+ Adds an ID to the bot. $tc.col(1) $+ Usage: $+ $tc.col(2) !addid Name TornID 
        msg # $tc.col(8) $+ Note: if the bot reports the wrong name after adding ID, use $+ $tc.col(4) !set main $tc.col(8) $+ to set the correct main name.
        halt 
      }
      else {
        if ($2 isnum && $3 == $null) { var %pid = $tc.cleanN($2) | var %pnick = $nick }
        elseif ($3 isnum) { var %pid = $tc.cleanN($3) | var %pnick = $2 }
        else { msg # $tc.col(5) $+ Try again. Usage: $+ $tc.col(2) !addid Name TornID | halt }
        if (%pid != $null) { 
          var %retid = $tc.getid(%pnick)
          if (%retid == $null) { 
            hadd -m tc.ids %pnick %pid            
            hsave tc.ids $tc.idsfile
            msg # $tc.col(1) $+ Added $+ $tc.col(2) %pnick $tc.colid(%pid,1,2) to the database!
          }
          else { msg # $tc.col(5) $+ Whoops! $+ $tc.col(2) %pnick $tc.col(5) $+ already exists in the bot with ID $+ $tc.col(2) %retid $+ $tc.col(5) $+ .  }
        }
      }
    }    
    if ($1 == !delid) {
      tc.chkaccess delid $nick #
      if ($2 == $null) { var %pnick = $nick }
      else { var %pnick = $2 }      
      var %retid = $tc.getid(%pnick)      
      if (%retid != $null) {              
        hdel tc.ids %pnick        
        hsave tc.ids $tc.idsfile
        if ($hget(tc.idsname,%retid) != $null) { 
          hdel tc.idsname %retid
          hsave tc.idsname $tc.idsnamefile
        }
        msg # $tc.col(6) $+ Removed $+ $tc.col(2) %pnick $tc.col(6) $+ from the database! 
      }
      else { msg # $tc.col(5) $+ No can do, $+ $tc.col(2) %pnick $tc.col(5) $+ does not exist in the database! }
    }

    if ($1 == !whois) {
      if ($2 == $null || $2 !isnum) { msg # $tc.col(3) $+ Checks which names are all stored for a particular ID. $tc.col(1) $+ Usage: $+ $tc.col(2) !whois ID }
      else {
        var %ptr = 1, %end = $hget(tc.ids,0).item
        while (%ptr <= %end) {
          var %name = $hget(tc.ids,%ptr).item, %id = $tc.getid(%name)
          if ($2 == %id) { var %idlist = $addtok(%idlist,%name,32) }
          inc %ptr
        }
        if (%idlist != $null) { msg # $tc.col(1) $+ Stored names for ID $tc.colid($2,1,2) $+ : $+ $tc.col(2) %idlist }
        else { msg # $tc.col(5) $+ There are no names stored for ID $tc.colid($2,5,2) $+ ! }
      }
    }

    ; --------------------------------- personal setting code ---------------------------------

    if ($1 == !setbday) {
      if ($2 != $null) {
        tc.chkaccess setbday $nick #
        var %mon = $left($3,3)
        if ($istok(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec,%mon,32) && ($4 >= 1 && $4 <= 31)) {
          var %id = $tc.getid($2), %month = $upper($left(%mon,1)) $+ $lower($mid(%mon,2,2))
          if (%id == $null) { msg # $tc.col(2) $+ $2 $tc.col(5) $+ does not have an ID saved to the bot! }
          else {
            if ($5 != $null && $len($int($5)) != 4) { msg # $tc.col(5) $+ Please enter birth year using 4 digits. Saving birthdate without the year. | var %yr = $null }
            elseif ($5 != $null && $int($5) !isnum) { msg # $tc.col(5) $+ Invalid birth year. Saving birthdate without the year. | var %yr = $null }
            elseif ($5 != $null && $int($5) >= $asctime($tc.time,yyyy)) { msg # $tc.col(5) $+ Liar. Saving birthdate without the year. | var %yr = $null }
            elseif ($5 == $null) { var %yr = $null }
            else { var %yr = $int($5) }
            tc.set %id bday %month $int($4) %yr
            msg # $tc.col(1) $+ I have set $+ $tc.col(2) $2 $+ $tc.col(1) $+ 's birthday to $+ $tc.col(2) $tc.getset(%id,bday) $+ $tc.col(1) $+ .
          } 
        }
        else { msg # $tc.col(3) $+ Sets another person's birthdate. $tc.col(1) $+ Usage: $+ $tc.col(2) !setbday Person month day year $tc.col(1) $+ (year is optional) $tc.col(8) $+ ie: !set bday Jan 1 }
      }
      else { msg # $tc.col(3) $+ Sets another person's birthdate. $tc.col(1) $+ Usage: $+ $tc.col(2) !setbday Person month day year $tc.col(1) $+ (year is optional) $tc.col(8) $+ ie: !set bday Jan 1 }
    }

    if ($1 == !set) {
      if ($2 == $null) {
        msg # $tc.col(3) $+ Sets personal settings. $tc.col(1) $+ Available settings: $+ $tc.col(2) bday stat main
        msg # $tc.col(1) $+ Use $+ $tc.col(2) !set setting $tc.col(1) $+ without any additional parameters for more details.
      }
      if ($2 == main) {
        if ($3 != $null) {
          var %id = $tc.getid($3), %idchk = $tc.getid($nick)
          if (%id != $null) {
            if (%idchk == %id) {           
              hadd tc.idsname %id $3
              hsave tc.idsname $tc.idsnamefile
              msg # $tc.col(6) $+ I have assigned $+ $tc.col(2) $3 $tc.col(6) $+ as the main name for ID $tc.colid(%id,6,2) $+ !
            } 
            else { msg # $tc.col(2) $+ $nick $+ $tc.col(5) and $tc.col(2) $+ $3 $+ $tc.col(5) do not have the same ID saved. }
          }
          else { msg # $tc.col(5) $+ I have no idea who $+ $tc.col(2) $3 $tc.col(5) $+ is, please make sure that it's added via !addid first }
        }
        else { msg # $tc.col(3) $+ Sets your main name (needs to be added via !addid). $tc.col(1) $+ Usage: $+ $tc.col(2) !set main Name }

      }
      if ($2 == bday) {
        var %mon = $left($3,3)
        if ($istok(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec,%mon,32) && ($4 >= 1 && $4 <= 31)) {
          var %id = $tc.getid($nick), %month = $upper($left(%mon,1)) $+ $lower($mid(%mon,2,2))
          if (%id == $null) { msg # $tc.col(5) $+ Need to save your ID to the bot $+ $tc.col(2) $nick $+ $tc.col(5) $+ ! }
          else {
            if ($5 != $null && $len($int($5)) != 4) { msg # $tc.col(5) $+ Please enter birth year using 4 digits. Saving birthdate without the year. | var %yr = $null }
            elseif ($5 != $null && $int($5) !isnum) { msg # $tc.col(5) $+ Invalid birth year. Saving birthdate without the year. | var %yr = $null }
            elseif ($5 != $null && $int($5) >= $asctime($tc.time,yyyy)) { msg # $tc.col(5) $+ Liar. Saving birthdate without the year. | var %yr = $null }
            elseif ($5 == $null) { var %yr = $null }
            else { var %yr = $int($5) }
            tc.set %id bday %month $int($4) %yr
            msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I have set your birthdate to $+ $tc.col(2) $tc.getset(%id,bday) $+ $tc.col(1) $+ .
          } 
        }
        else { msg # $tc.col(3) $+ Sets your birthdate. $tc.col(1) $+ Usage: $+ $tc.col(2) !set bday month day year $tc.col(1) $+ (year is optional) $tc.col(8) $+ ie: !set bday Jan 1 }
      }
      if ($2 == stat || $2 == stats || $2 == tstat || $2 == tstats) {
        if ($3 isnum && $4 isnum && $5 isnum && $6 isnum) {
          if ($3 > 100 || $4 > 100 || $5 > 100 || $6 > 100) { msg # $tc.col(5) $+ Liar. | halt }
          var %id = $tc.getid($nick)
          if (%id == $null) { msg # $tc.col(5) $+ Need to save your ID to the bot $+ $tc.col(2) $nick $+ $tc.col(5) $+ ! }
          else {
            tc.set %id stat $3 $4 $5 $6
            msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I have set your passive bonuses to: $tc.col(2) $+ + $+ $3 $+ % $tc.col(1) $+ Strength, $tc.col(2) $+ + $+ $4 $+ % $tc.col(1) $+ Defense, $tc.col(2) $+ + $+ $5 $+ % $tc.col(1) $+ Speed, $tc.col(2) $+ + $+ $6 $+ % $tc.col(1) $+ Dexterity. 
          }
        }
        else {
          msg # $tc.col(3) $+ Sets your passive bonuses to display on your !tstats page. $tc.col(1) $+ Usage $+ $tc.col(2) !set stat Str Def Spd Dex 
          msg # $tc.col(8) $+ ie: !set stats 5 4 2 3 = 5% passive bonus to str, 4% to def, 2% to speed, 3% to dexterity.
          msg # $tc.col(4) $+ You can find your passive bonuses on your home page besides your Battle Stats. If you are on drugs, mouse over to see the details. You'll need to add the bonuses without any 'high' or addiction effect.
        }
      }    
      halt
    }  

    if ($1 == !chain && $findtok(%tc.chchan,#,1,44) != $null) {
      if ($2 == on) {
        if (%tcchain.stchan != $null) { msg # $tc.col(5) $+ There is already a chain active in $+ $tc.col(2) %tcchain.stchan $+ $tc.col(5) $+ ! | halt }
        set %tcchain.stchan #
        set %tcchain.hits -1
        set %tcchain.loss 0
        set %tcchain.start $tc.time
        set %tcchain.revtot 0
        set %tcchain.revchan $readini($tc.cfgfile,n,Chain,revive)
        hmake tc.chain 100 | hmake tc.chmonth 100
        if ($readini($tc.cfgfile,n,Chain,upg) == 0) { var %chmin = 5 }
        elseif ($readini($tc.cfgfile,n,Chain,upg) == 1) { var %chmin = 5.5 }
        else { var %chmin = $calc(4 + $readini($tc.cfgfile,n,Chain,upg)) }
        var %dur = $calc(%chmin * 60)      
        .timer $+ 0.tcchainend -co 1 $calc(20 * 60) tc.chainend        
        .timer $+ 0.hitlistclr -co 1 $calc(20 * 60) tc.hitlistclr #
        .timer $+ 0.tcchainrefillwarn1 $asctime($calc($ctime($date 23:54) + ($readini($tc.cfgfile,n,Config,timeadj) * 60 * 60)),HH:nn) 1 1 msg # $tc.col(4) $+ Note: Energy refills will reset in ten minutes.
        .timer $+ 0.tcchainrefillwarn2 $asctime($calc($ctime($date 23:59) + ($readini($tc.cfgfile,n,Config,timeadj) * 60 * 60)),HH:nn) 1 1 msg # $tc.col(4) $+ Note: Energy refills will reset in five minutes!
        .timer $+ 0.tcchainrefillwarn3 $asctime($calc($ctime($date 00:03) + ($readini($tc.cfgfile,n,Config,timeadj) * 60 * 60)),HH:nn) 1 1 msg # $tc.col(4) $+ Note: Energy refills will reset in two minutes! Might want to make sure timer is high in case of lag.
        .timer $+ 0.tcchainstalkers -co 1 $calc(20 * 60) tc.chainstalk #
        set %tcchain.chid $readini($tc.cfgfile,n,Chain,id) | if (%tcchain.chid == $null) { set %tcchain.chid 0 } 
        var %tempwtchk = $tc.chlogfile(%tcchain.chid)
        inc %tcchain.chid
        if ($exists(%tempwtchk)) { 
          hmake tc.wtchk 100 | hload tc.wtchk %tempwtchk
          var %datechk = $gettok($hget(tc.wtchk,s),6,32), %chk = $calc($tc.time - %datechk)
          set %tcchain.lastwt $readini($tc.cfgfile,n,Chain,wartally)
          if (%chk >= $tc.wtreset) {
            writeini -n $tc.cfgfile Chain wartally %tcchain.chid 
            msg # $tc.col(4) $+ Note: it's been more than $+ $tc.col(2) $duration($tc.wtreset) $tc.col(4) $+ since last chain, automatically setting wartally marker to current chain! Also, since it's been a while, please make sure to set $+ $tc.col(2) !warresp reset (Current Respect) $tc.col(4) $+ marker. 
          }
          hfree tc.wtchk
        } 
        set %tcchain.log $tc.chlogfile(%tcchain.chid) 
        set %tcchain.cmlog $tc.cmlogfile() | if ($exists(%tcchain.cmlog)) { hload tc.chmonth %tcchain.cmlog }
        if ($3 != $null) { if ($3 isnum) { set %tcchain.startresp $3 } | else { set %tcchain.startresp $remove($3,$chr(44)) } }
        if (%tcchain.startresp !isnum) { set %tcchain.startresp $readini($tc.cfgfile,n,Chain,resp) }        
        msg # $tc.col(1) $+ Starting chain $+ $tc.col(2) %tcchain.chid $tc.col(1) $+ with $+ $tc.col(2) $tc.addComma(%tcchain.startresp) $tc.col(1) $+ starting respect...
        if ($readini($tc.cfgfile,n,Chain,bonus) == $null) { msg # $tc.col(5) $+ No bonus faction has been set! Type $+ $tc.col(2) !bonus FactionName $tc.col(5) $+ to set. }       
        else { msg # $tc.col(6) $+ Bonus faction has been set to: $+ $tc.col(2) $readini($tc.cfgfile,n,Chain,bonus) $+ $tc.col(6) $+ ! Type $+ $tc.col(2) !bonus FactionName $tc.col(6) $+ to change. }
        if ($readini($tc.cfgfile,n,Chain,clead) == $null) { msg # $tc.col(5) $+ No chain leader has been set! Type $+ $tc.col(2) !leader Name $tc.col(5) $+ to set. }
        else { msg # $tc.col(6) $+ Chain leader is: $+ $tc.col(2) $readini($tc.cfgfile,n,Chain,clead) $+ $tc.col(6) $+ ! Type $+ $tc.col(2) !leader Name $tc.col(6) $+ to change. }
        if ($readini($tc.cfgfile,n,Chain,stalk) == $null) { msg # $tc.col(5) $+ No stalking directions have been set! Type $+ $tc.col(2) !stalk Note $tc.col(5) $+ to set. }
        else { msg # $tc.col(6) $+ Stalkers: $+ $tc.col(2) $readini($tc.cfgfile,n,Chain,stalk) $tc.col(6) $+ - Type $+ $tc.col(2) !stalk Note $tc.col(6) $+ to change. }
        if ($readini($tc.cfgfile,n,Chain,upg) == 1) { msg # $tc.col(1) $+ You have $+ $tc.col(2) 5 minutes and 30 seconds $tc.col(1) $+ after the SECOND hit to keep the chain running! }
        else { msg # $tc.col(1) $+ You have $+ $tc.col(2) %chmin minutes $tc.col(1) $+ after the SECOND hit to keep the chain running! }
        if ($readini($tc.cfgfile,n,Chain,postchains) == yes) {
          var %ptr = 1, %loops = $numtok(%tc.chchan,44)
          while (%ptr <= %loops) { 
            if ($gettok(%tc.chchan,%ptr,44) ischan && $gettok(%tc.chchan,%ptr,44) != %tcchain.stchan) { 
              msg $gettok(%tc.chchan,%ptr,44) $tc.col(1) $+ Chain starting soon in $+ $tc.col(2) %tcchain.stchan $+ $tc.col(1) $+ ! Please join if you wish to jump in as hits and updates will only be posted in there.              
            }
            inc %ptr 
          }
        }
        if (%tcchain.revchan != $null) {
          var %ptr = 1, %loops = $numtok(%tcchain.revchan,44)
          while (%ptr <= %loops) { 
            if ($gettok(%tcchain.revchan,%ptr,44) ischan && $gettok(%tcchain.revchan,%ptr,44) != %tcchain.stchan) { 
              msg $gettok(%tcchain.revchan,%ptr,44) $tc.col(1) $+ Chain starting momentarily, be ready with revives!
            }
            inc %ptr 
          }
        }
        halt
      }
      if ($istok(end off cancel,$2,32)) { 
        if (%tcchain.stchan != $null) {
          if (# == %tcchain.stchan) {
            ;if ($2 == cancel && %tcchain.hits > 0) { msg # $tc.col(5) $+ No can do, chain has started. | halt }
            if ($2 == cancel) {           
              if ($exists(%tcchain.log)) { .remove %tcchain.log }
              if ($readini($tc.cfgfile,n,Chain,postchains) == yes) {
                var %ptr = 1, %loops = $numtok(%tc.chchan,44)
                while (%ptr <= %loops) { 
                  if ($gettok(%tc.chchan,%ptr,44) ischan && $gettok(%tc.chchan,%ptr,44) != %tcchain.stchan) { 
                    msg $gettok(%tc.chchan,%ptr,44) $tc.col(1) $+ Whoops, false alarm! The chain in $+ $tc.col(2) %tcchain.stchan $tc.col(1) $+ has been cancelled.
                  }
                  inc %ptr 
                }
              }
              if (%tcchain.revchan != $null) {
                var %ptr = 1, %loops = $numtok(%tcchain.revchan,44)
                while (%ptr <= %loops) { 
                  if ($gettok(%tcchain.revchan,%ptr,44) ischan && $gettok(%tcchain.revchan,%ptr,44) != %tcchain.stchan) { 
                    msg $gettok(%tcchain.revchan,%ptr,44) $tc.col(1) $+ Whoops, false alarm! The chain has been cancelled.
                  }
                  inc %ptr 
                }
              }       
              if ($readini($tc.cfgfile,n,Chain,wartally) == %tcchain.chid) {
                writeini -n $tc.cfgfile Chain wartally %tcchain.lastwt
                msg # $tc.col(4) $+ Note: since wartally was reset for this cancelled chain, it has been reset back to chain $+ $tc.col(2) %tcchain.lastwt                      
              }
              .timer $+ 0.tcchain* off | unset %tcchain.* 
              if ($hget(tc.chain)) { hfree tc.chain }
              if ($hget(tc.chmonth)) { hfree tc.chmonth }
              msg # $tc.col(1) $+ The chain has been cancelled.
            }
          }
          if ($2 == off || $2 == end) {
            var %endresp = $remove($3,$chr(44),$chr(46))
            if (%endresp isnum) {
              if (%endresp < 5000000) { tc.chainend %endresp }
              else { msg # $tc.col(5) $+ Nope. | halt }
            }
            else { msg # $tc.col(5) $+ You need to specify an ending respect before ending a chain! Usage: $+ $tc.col(2) !chain off EndRespect | halt }
          }   
        } 
        else { msg # $tc.col(5) $+ There are no chains currently active! }  
      }  
      halt      
    }

    ; --- chain commands that require an active chain.

    if (# == %tcchain.stchan) {
      if ($1 == !endlesschain) {
        if (!%tcchain.endless) {
          .timer $+ 0.tcchainend off
          .timer $+ 0.hitlistclr off
          .timer $+ 0.tcchain1 off
          .timer $+ 0.tcchain2 off
          .timer $+ 0.tcchain3 off
          .timer $+ 0.tcchain4 off
          .timer $+ 0.tcchain5 off
          set %tcchain.endless 1
          msg # $tc.col(3) $+ All chain timers have been silenced and chain has gone into endless mode! $tc.col(1) $+ To resume: type $tc.col(2) $+ !endlesschain $+ $tc.col(1) again.
        }
        else {
          unset %tcchain.endless
          msg # $tc.col(3) $+ Endless flag has been cleared. $tc.col(1) $+ Timers will resume on the next attack.
        }
      }
      if ($1 == !respect) {
        var %inp = $remove($2,$chr(44))
        if (%inp isnum) { 
          set %tcchain.startresp %inp
          msg # $tc.col(1) $+ Starting respect set to: $+ $tc.col(2) $tc.addComma(%inp)
        }
        else { msg # $tc.col(3) $+ Changes starting respect of chain. $tc.col(1) $+ Usage: $+ $tc.col(2) !respect Respect }
      }
      if ($1 == w2h) { msg # $tc.col(2) $+ $nick $tc.col(1) $+ is waiting to hospitalize! }
      if ($1 == wfab) { msg # $tc.col(2) $+ $nick $tc.col(1) $+ is waiting to attack after bonus! }
      if ($1 == omw) { msg # $tc.col(2) $+ $nick $tc.col(1) $+ is on their way to hospitalize someone! }
      if ($1 == !offl) { 
        if ($2 isnum) { tc.chainoffl $2 }
        else { tc.chainoffl 1 }
      }
      if ($1 == !leader) {
        if ($readini($tc.cfgfile,n,Chain,clead) == $null) { var %clead = $tc.col(5) $+ not set } | else { var %clead = $readini($tc.cfgfile,n,Chain,clead) }
        if ($2 != $null) { 
          if ($2 == off) { remini $tc.cfgfile Chain clead | msg # $tc.col(1) $+ Chain leader setting has been cleared. | halt }
          writeini -n $tc.cfgfile Chain clead $2- | flushini $tc.cfgfile 
          msg # $tc.col(1) $+ Chain leader is now set to: $+ $tc.col(2) $2- $+ $tc.col(1) $+ ! 
        }
        else { msg # $tc.col(3) $+ Chain leader is currently: $+ $tc.col(2) %clead $+ $tc.col(3) $+ . $tc.col(1) $+ Use $+ $tc.col(2) !leader Name $tc.col(1) $+ to change or $tc.col(2) $+ !leader off $+ $tc.col(1) to clear. }
      }      
      if ($1 == !stalk || $1 == !stalker) {
        if ($readini($tc.cfgfile,n,Chain,stalk) == $null) { var %stalk = $tc.col(5) $+ not set } | else { var %stalk = $readini($tc.cfgfile,n,Chain,stalk) }
        if ($2 != $null) { 
          if ($2 == off) { remini $tc.cfgfile Chain stalk | msg # $tc.col(1) $+ Stalking notes have been cleared. | halt }
          writeini -n $tc.cfgfile Chain stalk $2- | flushini $tc.cfgfile 
          msg # $tc.col(1) $+ Stalking directions set to: $+ $tc.col(2) $2- 
          msg # $tc.col(1) $+ Note: $tc.col(4) $+ This will be broadcasted every 20min in channel. Use $tc.col(2) $+ !stalk off $+ $tc.col(4) to clear.
        }
        else {
          msg # $tc.col(3) $+ Stalkers: $+ $tc.col(2) %stalk 
          msg # $tc.col(1) $+ Use $+ $tc.col(2) !stalk Note $tc.col(1) $+ to change or $tc.col(2) $+ !stalk off $+ $tc.col(1) to clear.         
        }
      }
      if ($1 == !bonus) {
        if ($readini($tc.cfgfile,n,Chain,bonus) == $null) { var %bonusfact = $tc.col(5) $+ not set } | else { var %bonusfact = $readini($tc.cfgfile,n,Chain,bonus) }
        if ($2 != $null) { 
          if ($2- == off) { remini $tc.cfgfile Chain bonus | msg # $tc.col(1) $+ Bonus target has been cleared. | halt }
          writeini -n $tc.cfgfile Chain bonus $2- | flushini $tc.cfgfile 
          msg # $tc.col(1) $+ Bonus target is now set to: $+ $tc.col(2) $2- $+ $tc.col(1) $+ ! 
        }
        else { msg # $tc.col(3) $+ Bonus target is currently: $+ $tc.col(2) %bonusfact $+ $tc.col(3) $+ . $tc.col(1) $+ Use $+ $tc.col(2) !bonus FactionName $tc.col(1) $+ to change or $tc.col(2) $+ !bonus off $+ $tc.col(1) to clear. }
      }      
      if ($1 == !summary || $1 == !summ || $1 == !status || $1 == !whatsup) { msg # $tc.chainsumm }
      if ($1 == !counter || $1 == !count) {
        if ($2 isnum) { 
          if ($2 > 0) { set %tcchain.hits $2 | msg # $tc.col(1) $+ Chain counter has been set to $+ $tc.col(2) %tcchain.hits $+ $tc.col(1) $+ ! }
          else { msg # $tc.col(5) $+ Nope. | halt }
        }
        elseif ($2 == $null) { msg # $tc.col(8) $+ Chain Counter: $+ $tc.col(2) %tcchain.hits $+ $tc.col(1) $+ /100 }
        else { msg # $tc.col(3) $+ Manually sets the chain counter. $tc.col(1) $+ Usage: $+ $tc.col(2) !counter Count }
      }
      if ($1 == 911 || $1 == !911 || $1 == !addhit || $1 == !addhits) { 
        if ($2 isnum && $3 == $null) { var %count = $2, %name = $nick }
        elseif ($2 != $null && ($3 == $null || $3 !isnum)) { var %count = 1, %name = $2 }
        elseif ($3 isnum) { 
          if ($3 > 0) { var %count = $3, %name = $2 }
          else { msg # $tc.col(5) $+ Nope | halt }
        }
        else { var %count = 1, %name = $nick }
        if (%count > 200) { msg # $tc.col(5) $+ I don't think so, $+ $tc.col(2) %name $+ $tc.col(5) $+ . | halt }
        tc.chainadd h %name %count $chr(63)
      }
      if ($1 == !lost || $1 == !loss || $1 == !lose) { 
        if ($2 != $null) { var %name = $2 }
        else { var %name = $nick }
        tc.chainadd l %name 1 $chr(63)
      }
      if ($1 == !stalemate || $1 == !sm) {
        if ($2 != $null) { var %name = $2 }
        else { var %name = $nick }
        tc.chainadd s %name 1 $chr(63)
      }
      if ($1 == !dko) { 
        if ($2 != $null) { var %name = $2 }
        else { var %name = $nick }
        tc.chainadd d %name 1 $chr(63)
      }

      ; hosp pastes
      if ($1-2 == YOU HOSPITALIZED) { tc.chainadd h $nick 1 $3 }

      ; lost pastes
      if ($1-3 == YOU LOST TO) { 
        if ($4 != $null) { var %targ = $4 } | else { var %targ = $chr(63) }
        tc.chainadd l $nick 1 %targ 
      }
      if ($1-2 == YOU LOST && $3 == $null) { tc.chainadd l $nick 1 $chr(63) }
      if ($2-9 == TOOK YOU DOWN, PREVENTING YOU FROM WINNING CURRENT) { tc.chainadd l $nick 1 $1 }

      ; DKO pastes (if they still exist)
      ;if ($15 == Double && ($16 == KO || $16 == KO. || $16 == KO.You)) { tc.chainadd d $nick 1 $chr(63) }

      ; stalemate pastes
      if ($1-3 == YOU BOTH STALEMATED) { tc.chainadd s $nick 1 $chr(63) }


      if ($1 == !pingme) {
        if ($findtok(%tcchain.pinglist,$nick,32)) { 
          set %tcchain.pinglist $deltok(%tcchain.pinglist,$findtok(%tcchain.pinglist,$nick,32),32)
          msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I have removed you from the ping list. 
        }
        else { 
          set %tcchain.pinglist $addtok(%tcchain.pinglist,$nick,32) 
          msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I have added you to the ping list. You will recieve a channel ping on two minute warnings for this chain.
        }
      }
      if ($1 == !hitlist) { 
        if ($timer(0.tcflag.spam)) { msg # $tc.col(5) $+ No can do, too many requests too often. You can try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds. }      
        else { 
          if (%tcchain.hits >= 0) { tc.chainpost # } 
          else { msg %tcchain.stchan $tc.col(5) $+ There hasn't been any recorded attacks yet! }
        }
      }
      if ($1 == !done || $1 == !hits || $1 == !attacks) { 
        if ($2 != $null) { var %name = $2 }
        else { var %name = $nick }
        var %id = $tc.getid(%name)
        if (%id == $null) { 
          if ($2 != $null) { msg # $tc.col(5) $+ Search for $+ $tc.col(2) %name $tc.col(5) $+ not found. | halt }    
          else { msg # $tc.col(5) $+ No idea who you are $+ $tc.col(2) %name $+ $tc.col(5) $+ . | halt }
        }
        var %read = $hget(tc.chain,%id)
        if (%read != $null) { 
          var %hits = $gettok(%read,1,32), %loss = $gettok(%read,2,32), %dko = $gettok(%read,3,32), %sm = $gettok(%read,4,32)
          if (%id isnum) { var %namestr = %name $tc.colid(%id,1,2) }
          else { var %namestr = %name }
          if ($1 == !done) { var %chanmsg = $tc.col(1) $+ Thank you for chaining $+ $tc.col(2) %namestr $+ $tc.col(1) $+ ! You have made }
          else { var %chanmsg = $tc.col(2) $+ %namestr $+ $chr(32) $+ $tc.col(1) $+ has made }
          if (%hits > 0) { var %chanmsg = %chanmsg $+ $tc.col(2) $+ $chr(32) $+ %hits $tc.col(1) $+ hits }
          else { var %chanmsg = %chanmsg $+ $chr(32) $+ $tc.col(7) $+ NO $tc.col(1) $+ hits }
          if (%sm > 0) { 
            if (%loss > 0 || %dko > 0) { var %chanmsg = %chanmsg $+ , stalemated $+ $tc.col(2) %sm }
            else { var %chanmsg = %chanmsg $+ $chr(32) $+ and stalemated $+ $tc.col(2) %sm }
          }
          if (%sm > 1) { var %chanmsg = %chanmsg $+ $chr(32) $+ $tc.col(1) $+ times }
          if (%sm == 1) { var %chanmsg = %chanmsg $+ $chr(32) $+ $tc.col(1) $+ time }
          if (%dko > 0) { 
            if (%loss > 0) { var %chanmsg = %chanmsg $+ , had $+ $tc.col(2) %dko }
            else { var %chanmsg = %chanmsg $+ $chr(32) $+ and $+ $tc.col(2) %dko }
          }
          if (%dko > 1) { var %chanmsg = %chanmsg $+ $chr(32) $+ $tc.col(1) $+ DKOs }
          if (%dko == 1) { var %chanmsg = %chanmsg $+ $chr(32) $+ $tc.col(1) $+ DKO }
          if (%loss == 1) { var %chanmsg = %chanmsg $+ $chr(32) $+ and lost once }
          if (%loss > 1) { var %chanmsg = %chanmsg $+ $chr(32) $+ and lost $+ $tc.col(2) %loss $tc.col(1) $+ times }
          msg # %chanmsg $+ $chr(46)
        }
        else { msg # $tc.col(2) $+ %name $tc.col(1) $+ has not done any attacks this chain! }
      }
      if ($1 == !addatk || $1 == !addatks || $1 == !chgatk || $1 == !chgatks) {
        if ($2 isnum && $3 == $null) { var %name = $nick | var %hits = $2 }
        elseif ($2 isnum && $3 != $null) { var %name = $3 | var %hits = $2 }
        elseif ($3 isnum) { var %name = $2 | var %hits = $3 }
        else { msg # $tc.col(3) $+ Adds your name to the attack list. $tc.col(1) $+ Usage: $+ $tc.col(2) !addhits Hits $tc.col(1) $+ or $+ $tc.col(2) !addhits Name Hits | halt }  
        hadd -m tc.atklist %name %hits
        msg # $tc.col(2) $+ %name $+ $tc.col(1) $+ , I have added you to the attack list with $+ $tc.col(2) %hits $tc.col(1) $+ attacks!
        .timer $+ 0.hitlistclr -co 1 $calc(20 * 60) tc.hitlistclr #
      }   
      if ($1 == Respect:) {
        if (%tcchain.hits <= 0) { msg # $tc.col(5) $+ Noone has done any hits yet! | halt }
        var %inp = $tc.cleanN($2)
        if (%inp isnum) {         
          if (%tcchain.hits < 10) { var %bonus = 0 }
          if (%tcchain.hits >= 10 && %tcchain.hits < 25) { var %bonus = 10 }
          if (%tcchain.hits >= 25 && %tcchain.hits < 50) { var %bonus = $calc(10 + 25) }
          if (%tcchain.hits >= 50 && %tcchain.hits < 75) { var %bonus = $calc(10 + 25 + 50) }
          if (%tcchain.hits >= 75 && %tcchain.hits < 100) { var %bonus = $calc(10 + 25 + 50 + 75) }
          if (%tcchain.hits >= 100) { var %bonus = $calc(10 + 25 + 50 + 75 + 100) }
          var %resp = $calc(%inp - %tcchain.startresp)
          var %avgbonus = $round($calc(%resp / (%tcchain.hits + 1)),2)
          var %avg = $round($calc((%resp - %bonus) / (%tcchain.hits + 1)),2)
          msg # $tc.col(1) $+ So far, we have gained $+ $tc.col(2) $tc.addcomma(%resp) $tc.col(1) $+ respect this chain!
          if (%tcchain.hits >= 10) { msg # $tc.col(1) $+ This works out to an average of $+ $tc.col(2) $tc.addcomma(%avg) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addcomma(%avgbonus) $tc.col(1) $+ with bonus) respect per hit! }
          else { msg # $tc.col(1) $+ This works out to an average of $+ $tc.col(2) $tc.addcomma(%avg) $tc.col(1) $+ respect per hit! }
        }    
      }  
    }

    ; --- chain commands that do NOT require an active chain.

    if ($1 == !viewatk || $1 == !viewatks || $1 == !atklist) { 
      if ($hget(tc.atklist)) {
        window -hn @atklisttally | window -hn @atklistsort
        var %ptr = 1, %loop = $hget(tc.atklist,0).item
        while (%ptr <= %loop) { aline @atklisttally $hget(tc.atklist,%ptr).item $hget(tc.atklist,%ptr).data | inc %ptr }
        filter -twwceu 2 32 @atklisttally @atklistsort
        msg # $tc.col(1) $+ List of attackers with remaining attacks:
        var %ptr = 1 | var %loop = $line(@atklistsort,0) | var %chanlinenum = 1
        while (%ptr <= %loop) {
          var %name = $gettok($line(@atklistsort,%ptr),1,32), %hits = $gettok($line(@atklistsort,%ptr),2,32)
          var %chanline = %chanline $+ $tc.col(2) %name $+ $tc.col(1) $+ - $+ $tc.col(2) %hits $tc.col(1) $+ hits $+ $chr(44)
          inc %chanlinenum
          if (%chanlinenum == 6) {
            msg # %chanline
            var %chanlinenum = 1, %chanline = $null
          }
          inc %ptr
        }
        if (%chanline != $null) { msg # $left(%chanline,$calc($len(%chanline) - 1)) $+ $chr(46) } 
        window -c @atklisttally | window -c @atklistsort
      }
      else { msg # $tc.col(5) $+ There are no entries on the attack list! }
    }
    if ($1 == !clearattacks || $1 == !clearattack || $1 == !clratk || $1 == !clratks) {
      if ($hget(tc.atklist)) { 
        .timer $+ 0.hitlistclr off
        hfree tc.atklist 
        msg # $tc.col(5) $+ The attackers list has been cleared. 
      }
      else { msg # $tc.col(5) $+ There are no entries on the attack list! }
    }
    if ($1 == !delatk || $1 == !delatks) { 
      if ($hget(tc.atklist)) { 
        if ($2 != $null) { var %name = $2 }
        else { var %name = $nick }
        if ($hget(tc.atklist,%name)) {
          hdel tc.atklist %name
          if ($hget(tc.atklist,0).item == 0) { hfree tc.atklist | msg # $tc.col(1) $+ Last entry deleted, list is now empty. }
          else { msg # $tc.col(1) $+ Removed $+ $tc.col(2) %name $tc.col(1) $+ from the attack list. }
        }
        else { msg # $tc.col(2) $+ %name $tc.col(5) $+ does not have any attacks stored! }
      }
      else { msg # $tc.col(5) $+ There are no entries on the attack list! }    
    }
    if ($istok(!chainhist !lastchain,$1,32)) {
      if ($timer(0.tcflag.spam)) { msg # $tc.col(5) $+ No can do, too many requests too often. You can try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds. | halt }      
      if ($1 == !chainhist) { if ($2 isnum) { var %chid = $2 } | else { msg # $tc.col(3) $+ Displays historical chain summary. $tc.col(1) $+ Usage: $+ $tc.col(2) !chainhist ChainID | halt } }
      if ($1 == !lastchain) { var %chid = $readini($tc.cfgfile,n,Chain,id) }
      var %logfile $tc.chlogfile(%chid)
      if (!$exists(%logfile)) { msg # $tc.col(5) $+ Chain $+ $tc.col(2) $2 $tc.col(5) $+ doesn't exist. | halt }
      hmake tc.suminfo 100 | hload tc.suminfo %logfile 
      var %summ = $hget(tc.suminfo,s)
      msg # $tc.col(1) $+ Chain $+ $tc.col(2) %chid $tc.col(1) $+ ended with $+ $tc.col(2) $gettok(%summ,3,32) $tc.col(1) $+ hits and $+ $tc.col(2) $gettok(%summ,4,32) $tc.col(1) $+ losses.
      msg # $tc.col(1) $+ The chain started on $+ $tc.col(2) $asctime($gettok(%summ,5,32),mmm d yyyy) $tc.col(1) $+ at $+ $tc.col(2) $asctime($gettok(%summ,5,32),h:nn tt) $tc.col(1) $+ and ended at $+ $tc.col(2) $asctime($gettok(%summ,6,32),h:nn tt) $tc.col(1) $+ (TC time), so it lasted for $+ $tc.col(2) $duration($calc($gettok(%summ,6,32) - $gettok(%summ,5,32))) $+ $tc.col(1) $+ .
      msg # $tc.col(1) $+ We started with $+ $tc.col(2) $tc.addComma($gettok(%summ,1,32)) $tc.col(1) $+ respect and ended with $+ $tc.col(2) $tc.addComma($gettok(%summ,2,32)) $+ $tc.col(1) $+ , so our change was $+ $tc.col(2) $tc.addComma($calc($gettok(%summ,2,32) - $gettok(%summ,1,32))) $tc.col(1) $+ respect! We averaged $+ $tc.col(2) $tc.addComma($floor($calc(($gettok(%summ,2,32) - $gettok(%summ,1,32)) / $gettok(%summ,3,32)))) $tc.col(1) $+ respect per hit.
      hfree tc.suminfo
      tc.chainpost # %chid
      .timer0.tcflag.spam 1 $tc.gcd noop
    }
    if ($1 == !chainmonth) {
      if ($timer(0.tcflag.spam)) { msg # $tc.col(5) $+ No can do, too many requests too often. You can try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds. | halt }
      if (%tcchain.stchan != $null) { msg # $tc.col(5) $+ Nope, too spammy. Try it after the chain's over. :P | halt }
      if ($2 != $null) {
        var %mnchk = $left($2,3)
        if (%mnchk == Jan) { var %month = 01 } | if (%mnchk == Feb) { var %month = 02 } | if (%mnchk == Mar) { var %month = 03 }
        if (%mnchk == Apr) { var %month = 04 } | if (%mnchk == May) { var %month = 05 } | if (%mnchk == Jun) { var %month = 06 }
        if (%mnchk == Jul) { var %month = 07 } | if (%mnchk == Aug) { var %month = 08 } | if (%mnchk == Sep) { var %month = 09 }
        if (%mnchk == Oct) { var %month = 10 } | if (%mnchk == Nov) { var %month = 11 } | if (%mnchk == Dec) { var %month = 12 }
      }
      if (%month == $null) { var %month = $asctime($tc.time,mm) }
      if (%month == 01) { var %mnpost = January } | if (%month == 02) { var %mnpost = February } | if (%month == 03) { var %mnpost = March } 
      if (%month == 04) { var %mnpost = April } | if (%month == 05) { var %mnpost = May } | if (%month == 06) { var %mnpost = June } 
      if (%month == 07) { var %mnpost = July } | if (%month == 08) { var %mnpost = August } | if (%month == 09) { var %mnpost = September }  
      if (%month == 10) { var %mnpost = October } | if (%month == 11) { var %mnpost = November } | if (%month == 12) { var %mnpost = December } 
      if ($3 isnum) { var %year = $3 } | else { var %year = $asctime($tc.time,yyyy) }      
      var %fileloc = $tc.cmlogfile(%year,%month)
      if (!$exists(%fileloc)) { msg # $tc.col(5) $+ There is no monthly data stored for $+ $tc.col(2) %mnpost %year $+ $tc.col(5) $+ ! | halt }       
      hmake tc.chainmonth 100
      hload tc.chainmonth %fileloc      
      window -hn @chainmonthtally | window -hn @chainmonthsort
      var %ptr = 1, %loop = $hget(tc.chainmonth,0).item
      while (%ptr <= %loop) {
        var %id = $hget(tc.chainmonth,%ptr).item, %name = $tc.getname(%id)        
        if (%name == $null) { var %name = $tc.colid(%id,8,8) }
        var %hits = $gettok($hget(tc.chainmonth,%id),1,32)
        if (%hits > 0) { aline @chainmonthtally %name %hits }
        inc %ptr
      }
      filter -twwceu 2 32 @chainmonthtally @chainmonthsort
      msg # $tc.col(1) $+ List of chainers for: $+ $tc.col(2) %mnpost %year
      var %ptr = 1, %loop = $line(@chainmonthsort,0), %hitstot = 0, %chanlinenum = 1
      while (%ptr <= %loop) {
        var %read = $line(@chainmonthsort,%ptr)
        var %name = $gettok(%read,1,32), %hits = $gettok(%read,2,32)
        if (%hits > 0) {
          var %hitstot = $calc(%hitstot + %hits)
          var %chanline = %chanline $+ $tc.col(6) %name $tc.col(1) $+ $chr(60) $+ $tc.col(2) $+ $tc.addComma(%hits) $+ $tc.col(1) $+ $chr(62) $+ $chr(44)
          inc %chanlinenum
          if (%chanlinenum == 10) {
            msg # %chanline
            var %chanlinenum = 1, %chanline = $null
          }
        }
        inc %ptr
      }
      if (%chanline != $null) { msg # $left(%chanline,$calc($len(%chanline) - 1)) $+ $chr(46) } 
      msg # $tc.col(1) $+ That's a total of $+ $tc.col(2) %loop $tc.col(1) $+ attackers and $+ $tc.col(2) $tc.addComma(%hitstot) $tc.col(1) $+ attacks! 
      .timer0.tcflag.spam 1 $tc.gcd noop
      hfree tc.chainmonth | window -c @chainmonthtally | window -c @chainmonthsort
    }
    if ($1 == !mymonth) {
      var %id = $tc.getid($nick), %name = $tc.getname(%id)
      if (%id == $null) { msg # $tc.col(5) $+ There is no saved ID for $+ $tc.col(2) $nick $+ $tc.col(5) $+ ! | halt }      
      if ($2 != $null) {
        var %mnchk = $left($2,3)
        if (%mnchk == Jan) { var %month = 01 } | if (%mnchk == Feb) { var %month = 02 } | if (%mnchk == Mar) { var %month = 03 }
        if (%mnchk == Apr) { var %month = 04 } | if (%mnchk == May) { var %month = 05 } | if (%mnchk == Jun) { var %month = 06 }
        if (%mnchk == Jul) { var %month = 07 } | if (%mnchk == Aug) { var %month = 08 } | if (%mnchk == Sep) { var %month = 09 }
        if (%mnchk == Oct) { var %month = 10 } | if (%mnchk == Nov) { var %month = 11 } | if (%mnchk == Dec) { var %month = 12 }
      }
      if (%month == $null) { var %month = $asctime($tc.time,mm) }
      if (%month == 01) { var %mnpost = January } | if (%month == 02) { var %mnpost = February } | if (%month == 03) { var %mnpost = March } 
      if (%month == 04) { var %mnpost = April } | if (%month == 05) { var %mnpost = May } | if (%month == 06) { var %mnpost = June } 
      if (%month == 07) { var %mnpost = July } | if (%month == 08) { var %mnpost = August } | if (%month == 09) { var %mnpost = September }  
      if (%month == 10) { var %mnpost = October } | if (%month == 11) { var %mnpost = November } | if (%month == 12) { var %mnpost = December } 
      if ($3 isnum) { var %year = $3 } | else { var %year = $asctime($tc.time,yyyy) }      
      var %fileloc = $tc.cmlogfile(%year,%month)
      if (!$exists(%fileloc)) { msg # $tc.col(5) $+ There is no monthly data stored for $+ $tc.col(2) %mnpost %year $+ $tc.col(5) $+ ! | halt }       
      hmake tc.mymonth 100 | hload tc.mymonth %fileloc      
      var %hits = $gettok($hget(tc.mymonth,%id),1,32)
      if (%hits > 0) { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , you have made $+ $tc.col(2) $tc.addComma(%hits) $tc.col(1) $+ hits for $+ $tc.col(2) $+(%mnpost,$chr(32),%year) $+ $tc.col(1) $+ . $tc.col(1) $+ To see a tally of everyones hits for this month, type $+ $tc.col(3) !chainmonth }
      else { msg # $tc.col(5) $+ You haven't made any hits for $+ $tc.col(2) $+(%mnpost,$chr(32),%year) $+ $tc.col(5) $+ ! }
      hfree tc.mymonth
    }

    if ($1 == !warresp) {
      if ($2 == reset) {
        var %inp = $remove($3,$chr(44))
        if (%inp isnum) {
          tc.chkaccess wartally $nick #
          msg # $tc.col(1) $+ New saved respect marker is now at $+ $tc.col(2) $tc.addcomma(%inp) $tc.col(1) $+ respect.
          writeini -n $tc.cfgfile Chain warresp %inp | flushini $tc.cfgfile
        }
        else { msg # $tc.col(5) $+ Don't know what that means, try: $+ $tc.col(2) !warresp FactionRespect $tc.col(5) $+ to view current respect marker, or $+ $tc.col(2) !warresp reset FactionRespect $tc.col(5) $+ to reset it. }
      }
      else { 
        var %inp = $remove($2,$chr(44))
        if (%inp isnum) { msg # $tc.col(1) $+ So far, we have gained $+ $tc.col(2) $tc.addcomma($calc(%inp - $readini($tc.cfgfile,n,Chain,warresp))) $tc.col(1) $+ total respect! }
        else { msg # $tc.col(1) $+ So far, we have gained $+ $tc.col(2) $tc.addcomma($calc($readini($tc.cfgfile,n,Chain,resp) - $readini($tc.cfgfile,n,Chain,warresp))) $tc.col(1) $+ total respect! }
      }
    }
    if ($1 == !wartally) {
      if ($2 == reset) {
        tc.chkaccess wartally $nick #
        if (%tcchain.chid != $null) { var %chid = %tcchain.chid, %msg = the current chain ( $+ $tc.col(2) $+ %chid $+ $tc.col(2) $+ ) }
        else { var %chid = $readini($tc.cfgfile,n,Chain,id), %msg = chain $+ $tc.col(2) %chid }
        if ($3 isnum && $3 <= %chid && $3 >= 1) { var %chainid = $3, %msg = chain $+ $tc.col(2) $3 }
        else { var %chainid = %chid }
        writeini -n $tc.cfgfile Chain wartally %chainid | flushini $tc.cfgfile
        msg # $tc.col(1) $+ War tally reset for %msg
        halt
      }
      if (%tcchain.stchan != $null) { msg # $tc.col(5) $+ Nope, too spammy. Try it after the chain's over. :P | halt }
      if ($timer(0.tcflag.spam)) { msg # $tc.col(5) $+ No can do, too many requests too often. You can try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds. }      
      else {
        msg # $tc.col(1) $+ Reading & sorting data, one sec...
        var %mark = $readini($tc.cfgfile,n,Chain,wartally)
        if (%mark == 0 || %mark == $null) { msg # $tc.col(5) $+ There is no marker set for !wartally, use $+ $tc.col(2) !wartally reset $tc.col(5) $+ to set . | halt }        
        var %ptr = %mark, %maxloop = $readini($tc.cfgfile,n,Chain,id)
        if ($exists($tc.chlogfile($calc(%maxloop + 1)))) { inc %maxloop }
        if (%maxloop == 0) { msg # $tc.col(5) $+ There have been no chains recorded yet! | halt }
        hmake tc.wartally 100
        while (%ptr <= %maxloop) {
          hmake tc.wttemp
          hload tc.wttemp $tc.chlogfile(%ptr)
          var %wptr = 1, %wend = $hget(tc.wttemp,0).item
          while (%wptr <= %wend) {
            var %key = $hget(tc.wttemp,%wptr).item, %val = $hget(tc.wttemp,%key)
            if (%key != s) {
              var %old = $hget(tc.wartally,%key)
              if (%old != $null) { var %oh = $gettok(%old,1,32), %ol = $gettok(%old,2,32), %od = $gettok(%old,3,32), %os = $gettok(%old,4,32) }
              else { var %oh = 0, %ol = 0, %od = 0, %os = 0 }
              var %ah = $gettok(%val,1,32), %al = $gettok(%val,2,32), %ad = $gettok(%val,3,32), %as = $gettok(%val,4,32) 
              var %nh = $calc(%oh + %ah), %nl = $calc(%ol + %al), %nd = $calc(%od + %ad), %ns = $calc(%os + %as)              
              hadd tc.wartally %key %nh %nl %nd %ns
            }
            inc %wptr
          }
          hfree tc.wttemp
          inc %ptr
        }
        if ($hget(tc.wartally,0).item == 0) { 
          msg # $tc.col(5) $+ There haven't been any chains since last reset!
          hfree tc.wartally
          halt
        }
        window -hn @wartally | window -hn @wartallysort
        var %ptr = 1, %loop = $hget(tc.wartally,0).item
        while (%ptr <= %loop) {
          var %name = $tc.getname($hget(tc.wartally,%ptr).item)
          if (%name == $null) { var %name = $tc.colid($hget(tc.wartally,%ptr).item,8,8) }
          if ($hget(tc.wartally,%ptr).data > 0) { aline @wartally %name $hget(tc.wartally,%ptr).data }
          inc %ptr
        }
        filter -twwceu 2 32 @wartally @wartallysort
        msg # $tc.col(1) $+ List of chainers that participated from chain $+ $tc.col(2) %mark $tc.col(1) $+ to $+ $tc.col(2) %maxloop
        var %ptr = 1, %loop = $line(@wartallysort,0), %hitstot = 0, %chanlinenum = 1
        while (%ptr <= %loop) {
          var %read = $line(@wartallysort,%ptr)
          var %name = $gettok(%read,1,32), %hits = $gettok(%read,2,32)
          if (%hits > 0) {
            var %hitstot = $calc(%hitstot + %hits)
            var %chanline = %chanline $+ $tc.col(6) %name $tc.col(1) $+ $chr(60) $+ $tc.col(2) $+ $tc.addComma(%hits) $+ $tc.col(1) $+ $chr(62) $+ $chr(44)
            inc %chanlinenum
            if (%chanlinenum == 10) {
              msg # %chanline
              var %chanlinenum = 1
              var %chanline = $null
            }
          }
          inc %ptr
        }
        if (%chanline != $null) { msg # $left(%chanline,$calc($len(%chanline) - 1)) $+ $chr(46) } 
        msg # $tc.col(1) $+ That's a total of $+ $tc.col(2) %loop $tc.col(1) $+ attackers and $+ $tc.col(2) $tc.addComma(%hitstot) $tc.col(1) $+ attacks! 
        .timer0.tcflag.spam 1 $tc.gcd noop
        hfree tc.wartally | window -c @wartally | window -c @wartallysort
      }
    }
    if ($1 == !revtally) {
      if ($timer(0.tcflag.spam)) { msg # $tc.col(5) $+ No can do, too many requests too often. You can try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds. }      
      else {
        if ($2 == $null) { msg # $tc.col(3) $+ Displays a tally of revivers. $tc.col(1) $+ Usage: $+ $tc.col(2) !revtally Start End $tc.col(1) $+ (end is optional, if omitted will go from start until latest chain) | halt }
        var %maxloop = $readini($tc.cfgfile,n,Chain,id)
        if ($exists($tc.chlogfile($calc(%maxloop + 1)))) { inc %maxloop }
        if (%maxloop == 0) { msg # $tc.col(5) $+ There have been no chains recorded yet! | halt }
        if ($2 isnum) { var %ptr = $2, %start = $2 } | else { var %ptr = 1, %start = 1 }
        if ($3 isnum && $3 <= %maxloop) { var %end = $3 } | else { var %end = %maxloop }            
        msg # $tc.col(1) $+ Reading & sorting data, one sec... 
        if ($hget(tc.revtally)) { hfree tc.revtally }      
        hmake tc.revtally 100
        while (%ptr <= %end) {
          hmake tc.rttemp
          hload tc.rttemp $tc.chlogfile(%ptr)
          var %rptr = 1, %rend = $hget(tc.rttemp,0).item
          while (%rptr <= %rend) {
            var %key = $hget(tc.rttemp,%rptr).item, %val = $hget(tc.rttemp,%key)
            if (%key != s) {
              var %revadd = $gettok(%val,5,32), %old = $hget(tc.revtally,%key)
              if (%revadd > 0) { hadd tc.revtally %key $calc(%old + %revadd) }
            }
            inc %rptr
          }
          hfree tc.rttemp
          inc %ptr
        }
        if ($hget(tc.revtally,0).item == 0) { 
          msg # $tc.col(5) $+ There haven't been any revivers between chains $+ $tc.col(2) %start $tc.col(5) $+ and $+ $tc.col(2) %end $+ $tc.col(5) $+ !
          hfree tc.revtally
          halt
        }
        window -hn @revtally | window -hn @revtallysort
        var %ptr = 1, %loop = $hget(tc.revtally,0).item
        while (%ptr <= %loop) {
          var %name = $tc.getname($hget(tc.revtally,%ptr).item)
          if (%name == $null) { var %name = $tc.colid($hget(tc.revtally,%ptr).item,8,8) }
          if ($hget(tc.revtally,%ptr).data > 0) { aline @revtally %name $hget(tc.revtally,%ptr).data }
          inc %ptr
        }
        filter -twwceu 2 32 @revtally @revtallysort
        msg # $tc.col(1) $+ List of revivers since chains $+ $tc.col(2) %start $tc.col(1) $+ and $+ $tc.col(2) %end $+ $tc.col(1) $+ .
        var %ptr = 1, %loop = $line(@revtallysort,0), %hitstot = 0, %chanlinenum = 1
        while (%ptr <= %loop) {
          var %read = $line(@revtallysort,%ptr), %name = $gettok(%read,1,32), %hits = $gettok(%read,2,32), %hitstot = $calc(%hitstot + %hits)
          var %chanline = %chanline $+ $tc.col(2) %name $tc.col(1) $+ - $+ $tc.col(2) $tc.addComma(%hits) $tc.col(1) $+ revives $+ $chr(44)
          inc %chanlinenum
          if (%chanlinenum == 7) {
            msg # %chanline
            var %chanlinenum = 1
            var %chanline = $null
          }
          inc %ptr
        }
        if (%chanline != $null) { msg # $left(%chanline,$calc($len(%chanline) - 1)) $+ $chr(46) } 
        msg # $tc.col(1) $+ That's a total of $+ $tc.col(2) %loop $tc.col(1) $+ revivers and $+ $tc.col(2) $tc.addComma(%hitstot) $tc.col(1) $+ revives! 
        .timer0.tcflag.spam 1 $tc.gcd noop
        window -c @revtally | window -c @revtallysort | hfree tc.revtally
      }
    }

    ; --------------------------------- Group attack commands ---------------------------------

    ; Commands that work when group attack started
    if (%tcgroup.starter != $null) {
      if ($1 == !go) {
        msg # $tc.col(6) $+ It's go time! Last post: open $+ $tc.col(2) $tc.atklink(%tcgroup.targ) $tc.col(6) $+ but $+ $tc.col(4) DO NOT CLICK START YET! $tc.col(6) $+ - 10 seconds to start!
        msg # $tc.col(1) $+ Attackers: $+ $tc.col(2) %tcgroup.part
        .timer0.tcgroup1 1 5 msg # $tc.col(1) $+ Five seconds to attack!
        .timer0.tcgroup2 1 7 msg # $tc.col(1) $+ Three seconds... be ready!
        .timer0.tcgroup3 1 8 msg # $tc.col(1) $+ Two...
        .timer0.tcgroup4 1 9 msg # $tc.col(2) $+ ONE...
        .timer0.tcgroup5 1 10 tc.groupatk #
      }
      if ($1 == !in) {
        if (!$istok(%tcgroup.part,$nick,32)) { 
          set %tcgroup.part $addtok(%tcgroup.part,$nick,32) 
          msg # $tc.col(2) $+ $nick $+ $tc.col(1) has joined in the fun! - open $+ $tc.col(2) $tc.atklink(%tcgroup.targ) $tc.col(1) $+ but $+ $tc.col(4) DO NOT CLICK START YET!    
        }
        else {
          msg # $tc.col(5) $+ I've already got you in the list $tc.col(2) $+ $nick $+ $tc.col(5) - open $+ $tc.col(2) $tc.atklink(%tcgroup.targ) $tc.col(5) $+ but $+ $tc.col(4) DO NOT CLICK START YET!    
        }
      }
      if ($1 == !gcancel) {
        unset %tcgroup.*
        msg # $tc.col(1) $+ The group attack has been called off by $+ $tc.col(2) $nick
      }    
    }

    if ($istok(!gstart !gattack !groupstart !groupattack,$1,32)) {
      if ($2 != $null) {
        if (%tcgroup.starter == $null) {
          if ($2 isnum) { var %id = $2 } | else { var %id = $tc.getid($2) }
          if (%id isnum) {
            set %tcgroup.starter $nick
            set %tcgroup.targ %id
            set %tcgroup.part $null
            msg # $tc.col(2) $+ $nick $+ $tc.col(1) has initiated a group attack vs ID $tc.colid(%id,1,2) $+ ! 
            msg # $tc.col(4) $+ Type $+ $tc.col(2) !in $tc.col(4) $+ to join in the fun. Once everyone has joined, type $+ $tc.col(2) !go $tc.col(4) $+ to initiate the countdown for the attack! To cancel: use $+ $tc.col(2) !gcancel          
          }
          else { msg # $tc.col(5) $+ Do not know who $+ $tc.col(2) $2 $tc.col(5) $+ is. }
        }
        else { msg # $tc.col(5) $+ No can do, there's already a group attack started by $+ $tc.col(2) %tcgroup.starter $tc.col(5) $+ against $+ $tc.col(2) %tcgroup.targ $+ $tc.col(5) $+ ! }
      }
      else { msg # $tc.col(3) $+ Initiates a group attack on person. $+ $tc.col(1) Usage: $+ $tc.col(2) !gstart Name/ID }
    }

    ; --------------------------------- link lookup code, ie: !bust ---------------------------------

    if ($istok(!id !bust !bail !bazaar !pstats !display !trade !cash !money !mail !msg !message !attack !atk !mug !bounty !friend !friendlist !flist !enemy !blacklist !blist,$1,32)) {
      if ($2 == $null) { var %searchnick = $nick }
      else { var %searchnick = $2 }
      var %retid = $tc.getid(%searchnick)
      if (%retid == $null) { msg # $tc.col(5) $+ Sorry, I have no idea who $+ $tc.col(2) %searchnick $tc.col(5) $+ is! } 
      else { 
        var %ret = $tc.getname(%retid)
        if ($1 == !id) { 
          if (%searchnick != $nick) { msg # $tc.col(2) $+ %searchnick $+ $tc.col(1) $+ 's stored ID is $+ $tc.col(2) %ret $tc.colid(%retid,1,2) - Profile link: $+ $tc.col(2) $tc.proflink(%retid) }
          else { msg # $tc.col(1) $+ Your stored ID is $+ $tc.col(2) %ret $tc.colid(%retid,1,2) - Profile link: $+ $tc.col(2) $tc.proflink(%retid) }
        } 
        if ($1 == !bust) { msg # $tc.col(1) $+ Bust $+ $tc.col(2) %ret $tc.colid(%retid,1,2) out of jail! - $+ $tc.col(2) $tc.bustlink(%retid) } 
        if ($1 == !bail) { msg # $tc.col(1) $+ Bail $+ $tc.col(2) %ret $tc.colid(%retid,1,2) out of jail! - $+ $tc.col(2) $tc.baillink(%retid) } 
        if ($1 == !bazaar) { msg # $tc.col(1) $+ Visit $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ 's bazaar! - $+ $tc.col(2)  $tc.bazaarlink(%retid) }
        if ($1 == !pstats) { msg # $tc.col(1) $+ View $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ 's personal stats! - $+ $tc.col(2) $tc.pstatslink(%retid) } 
        if ($1 == !display) { msg # $tc.col(1) $+ View $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ 's display case! - $+ $tc.col(2) $tc.dcaselink(%retid) } 
        if ($1 == !trade) { msg # $tc.col(1) $+ Trade with $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ ! - $+ $tc.col(2) $tc.tradelink(%retid) } 
        if ($1 == !cash || $1 == !money) { msg # $tc.col(1) $+ Send cash to $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ ! - $+ $tc.col(2) $tc.cashlink(%retid) } 
        if ($1 == !mail || $1 == !msg || $1 == !message) { msg # $tc.col(1) $+ Send a message to $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ ! - $+ $tc.col(2) $tc.maillink(%retid) } 
        if ($1 == !attack || $1 == !atk) { msg # $tc.col(1) $+ Attack $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ ! - $+ $tc.col(2) $tc.atklink(%retid) } 
        if ($1 == !mug) { msg # $tc.col(1) $+ Mug $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ ! - $+ $tc.col(2) $tc.atklink(%retid) } 
        if ($1 == !bounty) { msg # $tc.col(1) $+ Bounty $+ $tc.col(2) %ret $tc.colid(%retid,1,2) $+ ! - $+ $tc.col(2) $tc.bountylink(%retid) } 
        if ($1 == !friend || $1 == !friendlist || $1 == !flist) { msg # $tc.col(1) $+ Add $+ $tc.col(2) %ret $tc.colid(%retid,1,2) to your $tc.col(6) $+ friends list $+ $tc.col(1) $+ ! - $+ $tc.col(2) $tc.flistlink(%retid) }
        if ($1 == !enemy || $1 == !blacklist || $1 == !blist) { msg # $tc.col(1) $+ Add $+ $tc.col(2) %ret $tc.colid(%retid,1,2) to your $tc.col(7) $+ blacklist $+ $tc.col(1) $+ ! - $+ $tc.col(2) $tc.blistlink(%retid) }
      }
    }
    if ($1 == !attackid || $1 == !atkid) { 
      if ($2 isnum) { 
        var %idchk = $tc.getname($2)
        if (%idchk != $null) { var %idpost = $tc.col(2) $+ %idchk }
        else { var %idpost = $tc.colid($2,1,2) }
        msg # $tc.col(1) $+ Attack %idpost $+ $tc.col(1) $+ ! - $+ $tc.col(2) $tc.atklink($2)
      }
      else { msg # $tc.col(3) $+ Displays an attack link from an ID that you specify. $tc.col(1) $+ Usage: $+ $tc.col(2) !attackid ID }
    }
    if ($istok(!prof !profile !info !find,$1,32)) {      
      if (!$timer(0.tcflag.spam)) {
        if ($istok(!prof !profile !info,$1,32) && $2 == $null) { var %name = $nick } 
        if ($1 == !find && ($2 == $null || $2 !isnum)) { msg # $tc.col(3) $+ Displays summary for player belonging to a requested ID. $tc.col(1) $+ Usage: $+ $tc.col(2) !find ID | halt }
        if ($2 isnum) { var %id = $2 }
        else { if (%name == $null) { var %name = $2 } }
        if (%id == $null && $1 != !find) { var %id = $tc.getid(%name) } 
        if (%id == $null) { 
          if ($1 == !find) { msg # $tc.col(5) $+ There is no ID stored for $+ $tc.col(2) %name $+ $tc.col(5) $+ ! (There is no method to 'search' a name for an ID through the API.) | halt } 
          else { msg # $tc.col(5) $+ There is no ID stored for $+ $tc.col(2) %name $+ $tc.col(5) $+ ! | halt }
        }
        if ($tc.getapikey != $null) { 
          set %tcprof.find %id    
          set %tcprof.chan #              
          sockclose apiprofile | sockopen -e apiprofile api.torn.com 443
          .timer $+ 0.tcflag.spam 1 $tc.gcd noop
        }
        else { msg # $tc.col(5) $+ API usage not enabled, so bot cannot use profile lookups. }

      }
      else { msg # $tc.col(5) $+ Can't do that, try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds! }
    }

    ; --------------------------------- stat saving/comparing code ---------------------------------
    if (%tc.statsaving == e) {

      if ($1 = !shelp) {
        msg # $tc.col(2) $+ !stats $tc.col(1) $+ - Displays normal stats. $tc.col(2) $+ !tstats $tc.col(1) $+ - Displays stats with passives applied. $tc.col(2) $+ !dstats $tc.col(1) $+ - Displays stats with passives + drugs applied. $tc.col(2) $+ !cstats $tc.col(1) $+ - Displays current stats (passives pulled from paste).
        msg # $tc.col(2) $+ !compare Name (Name) $tc.col(1) $+ - Displays comparison between two people (second name can be omitted to compare yourself to another). $tc.col(2) $+ !vcompare Name (Name) $tc.col(1) $+ - Compare stats with vicodin and passives applied.
        msg # $tc.col(2) $+ !growth (Name) Days $tc.col(1) $+ - Displays current stat growth from X days ago till now. Name can be omitted to show your own.
      }

      if (%tc.oldspy != $nick && %tc.spy != $nick && %tsspy.spy != $nick && %tsspy.oldspy != $nick) { 
        if ($readini($tc.cfgfile,n,Config,bstats) == yes && $1-2 == Battle Stats) { set %tc.bstats $addtok(%tc.bstats,$nick,32) } 
        if ($istok(Strength Speed Dexterity Defense Battle,$1,32)) {
          if ($readini($tc.cfgfile,n,Config,bstats) == yes) {
            if ($1-2 == Battle StatsStrength) { set %tc.bstats $addtok(%tc.bstats,$nick,32) }
            if (!$findtok(%tc.bstats,$nick,1,32)) { halt }
          }
          var %retid = $tc.getid($nick) | if (%retid == $null) { .timer0.statsbitchmsg -co 1 1 msg # $tc.col(5) $+ You need to add your ID to the bot before saving stats! Use: $+ $tc.col(2) !addid TornID | halt }
          var %tablename = stat $+ %retid, %chk = 1
          var %chkstr1 = $replace($1-,$chr(58),$null,Strength,$chr(32) $+ Strength,Speed,$chr(32) $+ Speed,Dexterity,$chr(32) $+ Dexterity,Defense,$chr(32) $+ Defense,Total,$chr(32) $+ Total)
          var %chkstr = $replace(%chkstr1,Strength,Strength:,Speed,Speed:,Dexterity,Dexterity:,Defense,Defense:,Total,Total:)
          while (%chk < $numtok(%chkstr,32)) {
            if ($gettok(%chkstr,%chk,32) == Strength:) {
              var %stat = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32))
              if (%stat != $null) { 
                set %tc.bstatadd 1
                if (%stat > 1000000000000000 || %stat <= 0) { msg # $tc.col(5) $+ Nope. | halt } 
                var %pass = $gettok(%chkstr,$calc(%chk + 2),32)
                if ($left(%pass,1) == $chr(40)) { var %pass = $gettok(%chkstr,$calc(%chk + 3),32) }
                hadd -m %tablename str %stat $tc.cleanStatPass(%pass)
              }
            }
            if ($gettok(%chkstr,%chk,32) == Speed:) { 
              var %stat = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32))
              if (%stat != $null) {
                set %tc.bstatadd 1
                if (%stat > 1000000000000000 || %stat <= 0) { msg # $tc.col(5) $+ Nope. | halt } 
                var %pass = $gettok(%chkstr,$calc(%chk + 2),32)
                if ($left(%pass,1) == $chr(40)) { var %pass = $gettok(%chkstr,$calc(%chk + 3),32) }
                hadd -m %tablename spd %stat $tc.cleanStatPass(%pass)
              }
            }
            if ($gettok(%chkstr,%chk,32) == Dexterity:) { 
              var %stat = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32))
              if (%stat != $null) {
                set %tc.bstatadd 1
                if (%stat > 1000000000000000 || %stat <= 0) { msg # $tc.col(5) $+ Nope. | halt } 
                var %pass = $gettok(%chkstr,$calc(%chk + 2),32)
                if ($left(%pass,1) == $chr(40)) { var %pass = $gettok(%chkstr,$calc(%chk + 3),32) }
                hadd -m %tablename dex %stat $tc.cleanStatPass(%pass)
              } 
            }
            if ($gettok(%chkstr,%chk,32) == Defense:) { 
              var %stat = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32))
              if (%stat != $null) {
                set %tc.bstatadd 1
                if (%stat > 1000000000000000 || %stat <= 0) { msg # $tc.col(5) $+ Nope. | halt } 
                var %pass = $gettok(%chkstr,$calc(%chk + 2),32)
                if ($left(%pass,1) == $chr(40)) { var %pass = $gettok(%chkstr,$calc(%chk + 3),32) }
                hadd -m %tablename def %stat $tc.cleanStatPass(%pass)
              }
            }
            if ($hget(%tablename,0).item == 4) { tc.statsave %retid # $nick | halt }
            inc %chk
          }      
          if (%tc.bstatadd) { .timer $+ %retid $+ $chr(46) $+ stat -co 1 3 tc.statsave %retid # $nick }
        }
        else { 
          if ($readini($tc.cfgfile,n,Config,bstats) == yes && $findtok(%tc.bstats,$nick,1,32) && $1-2 != Battle Stats) { 
            set %tc.bstats $deltok(%tc.bstats,$findtok(%tc.bstats,$nick,1,32),32) 
            if (%tc.bstats == $null) { unset %tc.bstats }
          } 
        }
        if ($1 == peed:) { msg # $tc.col(5) $+ That's a lot of pee... | halt }
      }

      if ($1 == !stats || $1 == !stat) {
        if (!$timer(0.tcflag.spam)) {
          if ($2 == $null) { var %find = $nick }
          else { var %find = $2 }
          var %retid = $tc.getid(%find), %retname = $tc.getname(%retid)
          if (%retid == $null) { msg # $tc.col(5) $+ Sorry $+ $tc.col(2) %find $+ $tc.col(5) $+ , I have no clue who you are! }
          else { 
            var %statsfile = $tc.statsfile(%retid)
            var %lastsaved = $read($qt(%statsfile), $lines(%statsfile))
            var %lasttime = $gettok(%lastsaved,1,32), %laststr = $gettok(%lastsaved,2,32), %lastdef = $gettok(%lastsaved,3,32), %lastspd = $gettok(%lastsaved,4,32), %lastdex = $gettok(%lastsaved,5,32)
            if (%lasttime == $null) { var %lasttime = 0 }
            if (%laststr == $null) { var %laststr = 0 } | if (%lastdef == $null) { var %lastdef = 0 }
            if (%lastspd == $null) { var %lastspd = 0 } | if (%lastdex == $null) { var %lastdex = 0 }
            var %total = $calc(%laststr + %lastdef + %lastspd + %lastdex)
            if (%total != 0) {
              var %age = $calc($tc.time - %lasttime)
              if (%age > 604800) { var %agecol = $tc.col(7) } | else { var %agecol = $tc.col(6) }
              msg # $tc.col(1) $+ Stats for $+ $tc.col(2) %retname $tc.colid(%retid,1,2) as of $+ $tc.col(2) $asctime(%lasttime,mmm d) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%lasttime,h:nn:ss tt) $tc.col(1) $+ (GMT/TC time) - $+ %agecol $duration(%age) $tc.col(1) $+ ago.
              msg # $tc.col(1) $+ Strength: $+ $tc.col(2) $tc.addComma(%laststr) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%laststr / %total) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41)
              msg # $tc.col(1) $+ Defense: $+ $tc.col(2) $tc.addComma(%lastdef) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%lastdef / %total) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41)
              msg # $tc.col(1) $+ Speed: $+ $tc.col(2) $tc.addComma(%lastspd) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%lastspd / %total) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41)
              msg # $tc.col(1) $+ Dexterity: $+ $tc.col(2) $tc.addComma(%lastdex) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%lastdex / %total) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41)
              msg # $tc.col(1) $+ Total: $+ $tc.col(2) $tc.addComma(%total)
              .timer $+ 0.tcflag.spam 1 $tc.gcd noop
            }
            else { 
              if ($2 != $null) { msg # $tc.col(5) $+ They haven't posted any stats! }
              else { msg # $tc.col(5) $+ You haven't posted any stats! }
            }
          }
        }
        else { msg # $tc.col(5) $+ Can't do that, try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds! }
      }

      if ($1 == !dstats || $1 == !dstat) {
        if (!$timer(0.tcflag.spam)) {
          if ($2 == $null) { var %find = $nick } | else { var %find = $2 }
          var %retid = $tc.getid(%find), %retname = $tc.getname(%retid) 
          if (%retid == $null) { msg # $tc.col(5) $+ Sorry $+ $tc.col(2) %find $+ $tc.col(5) $+ , I have no clue who you are! }
          else { 
            var %statsfile = $tc.statsfile(%retid)
            var %lastsaved = $read($qt(%statsfile), $lines(%statsfile))
            var %lasttime = $gettok(%lastsaved,1,32), %laststr = $gettok(%lastsaved,2,32), %lastdef = $gettok(%lastsaved,3,32), %lastspd = $gettok(%lastsaved,4,32), %lastdex = $gettok(%lastsaved,5,32)
            if (%lasttime == $null) { var %lasttime = 0 }
            if (%laststr == $null) { var %laststr = 0 } | if (%lastdef == $null) { var %lastdef = 0 }
            if (%lastspd == $null) { var %lastspd = 0 } | if (%lastdex == $null) { var %lastdex = 0 }
            var %total = $calc(%laststr + %lastdef + %lastspd + %lastdex)
            var %pass = $tc.getset(%retid,stat), %pstr = $gettok(%pass,1,32), %pdef = $gettok(%pass,2,32), %pspd = $gettok(%pass,3,32), %pdex = $gettok(%pass,4,32)
            if (%pstr !isnum) { var %pstr = 0 } | if (%pspd !isnum) { var %pspd = 0 } | if (%pdex !isnum) { var %pdex = 0 } | if (%pdef !isnum) { var %pdef = 0 }
            var %vpstr = $calc(%pstr + 25), %vpspd = $calc(%pspd + 25), %vpdex = $calc(%pdex + 25), %vpdef = $calc(%pdef + 25)
            var %xpstr = $calc(%pstr - 35), %xpspd = $calc(%pspd - 35), %xpdex = $calc(%pdex - 35), %xpdef = $calc(%pdef - 35)
            var %vstr = $round($calc(%laststr * ((%vpstr / 100) + 1)),0), %vspd = $round($calc(%lastspd * ((%vpspd / 100) + 1)),0)
            var %vdex = $round($calc(%lastdex * ((%vpdex / 100) + 1)),0), %vdef = $round($calc(%lastdef * ((%vpdef / 100) + 1)),0)   
            var %xstr = $round($calc(%laststr * ((%xpstr / 100) + 1)),0), %xspd = $round($calc(%lastspd * ((%xpspd / 100) + 1)),0)
            var %xdex = $round($calc(%lastdex * ((%xpdex / 100) + 1)),0), %xdef = $round($calc(%lastdef * ((%xpdef / 100) + 1)),0)  
            var %vtot = $round($calc(%vstr + %vdef + %vspd + %vdex),4), %xtot = $round($calc(%xstr + %xdef + %xspd + %xdex),4)
            var %vext = $round($calc(%vtot - %total),4), %xext = $round($calc(%xtot - %total),4)
            if (%total != 0) {
              if (%pstr == $null) { var %pstr = 0 } | if (%pdef == $null) { var %pdef = 0 } | if (%pspd == $null) { var %pspd = 0 } | if (%pdex == $null) { var %pdex = 0 } 
              var %age = $calc($tc.time - %lasttime)
              if (%age > 604800) { var %agecol = $tc.col(7) } | else { var %agecol = $tc.col(6) }
              msg # $tc.col(1) $+ Drug + True Stats for $+ $tc.col(2) %retname $tc.colid(%retid,1,2) as of $+ $tc.col(2) $asctime(%lasttime,mmm d) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%lasttime,h:nn:ss tt) $tc.col(1) $+ (GMT/TC time) - $+ %agecol $duration(%age) $tc.col(1) $+ ago.
              if ($calc(%pstr + %pspd + %pdef + %pdex) == 0) { msg # $tc.col(5) $+ No passive stats set, use $tc.col(2) $+ !set tstat $tc.col(5) $+ to set them. Using base stats... }
              msg # $tc.col(1) $+ Passives: $tc.col(2) $+ %vpstr $+ % $tc.col(6) $+ Str, $tc.col(2) $+ %vpdef $+ % $tc.col(6) $+ Def, $tc.col(2) $+ %vpspd $+ % $tc.col(6) $+ Spd, $tc.col(2) $+ %vpdex $+ % $tc.col(6) $+ Dex on Vicodin, $tc.col(2) $+ %xpstr $+ % $tc.col(7) $+ Str, $tc.col(2) $+ %xpdef $+ % $tc.col(7) $+ Def, $tc.col(2) $+ %xpspd $+ % $tc.col(7) $+ Spd, $tc.col(2) $+ %xpdex $+ % $tc.col(7) $+ Dex on Xanax.
              msg # $tc.col(1) $+ Strength: $+ $tc.col(6) $tc.addComma(%vstr) $tc.col(1) $+ - $+ $tc.col(7) $tc.addComma(%xstr)
              msg # $tc.col(1) $+ Defense:  $+ $tc.col(6) $tc.addComma(%vdef) $tc.col(1) $+ - $+ $tc.col(7) $tc.addComma(%xdef)
              msg # $tc.col(1) $+ Speed: $+ $tc.col(6) $tc.addComma(%vspd) $tc.col(1) $+ - $+ $tc.col(7) $tc.addComma(%xspd)
              msg # $tc.col(1) $+ Dexterity: $+ $tc.col(6) $tc.addComma(%vdex) $tc.col(1) $+ - $+ $tc.col(7) $tc.addComma(%xdex)
              msg # $tc.col(1) $+ Total: $+ $tc.col(6) $tc.addComma(%vtot) $tc.col(1) $+ - $+ $tc.col(7) $tc.addComma(%xtot)
              msg # $tc.col(1) $+ Extra Stats: $+ $tc.col(6) $tc.addComma(%vext) $tc.col(1) $+ - $+ $tc.col(7) $tc.addComma(%xext)
              .timer $+ 0.tcflag.spam 1 $tc.gcd noop
            }
            else { 
              if ($2 != $null) { msg # $tc.col(5) $+ They haven't posted any stats! }
              else { msg # $tc.col(5) $+ You haven't posted any stats! }
            }
          }
        }
        else { msg # $tc.col(5) $+ Can't do that, try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds! }
      }

      if ($istok(!cstats !cstat !tstats !tstat,$1,32)) {
        if (!$timer(0.tcflag.spam)) {
          if ($2 == $null) { var %find = $nick } | else { var %find = $2 }
          var %retid = $tc.getid(%find), %retname = $tc.getname(%retid) 
          if (%retid == $null) { msg # $tc.col(5) $+ Sorry $+ $tc.col(2) %find $+ $tc.col(5) $+ , I have no clue who you are! }
          else { 
            var %statsfile = $tc.statsfile(%retid)
            var %lastsaved = $read($qt(%statsfile), $lines(%statsfile))
            var %lasttime = $gettok(%lastsaved,1,32), %laststr = $gettok(%lastsaved,2,32), %lastdef = $gettok(%lastsaved,3,32), %lastspd = $gettok(%lastsaved,4,32), %lastdex = $gettok(%lastsaved,5,32)
            if (%lasttime == $null) { var %lasttime = 0 }
            if (%laststr == $null) { var %laststr = 0 } | if (%lastdef == $null) { var %lastdef = 0 }
            if (%lastspd == $null) { var %lastspd = 0 } | if (%lastdex == $null) { var %lastdex = 0 }
            if ($istok(!cstats !cstat,$1,32)) { var %pass = $tc.getset(%retid,cstat), %title = Current Stats }
            else { var %pass = $tc.getset(%retid,stat), %title = True Stats }
            var %pstr = $gettok(%pass,1,32), %pdef = $gettok(%pass,2,32), %pspd = $gettok(%pass,3,32), %pdex = $gettok(%pass,4,32)
            var %str = $round($calc(%laststr * ((%pstr / 100) + 1)),4), %spd = $round($calc(%lastspd * ((%pspd / 100) + 1)),4)
            var %dex = $round($calc(%lastdex * ((%pdex / 100) + 1)),4), %def = $round($calc(%lastdef * ((%pdef / 100) + 1)),4)                             
            var %lasttot = $calc(%laststr + %lastdef + %lastspd + %lastdex), %tot = $calc(%str + %def + %spd + %dex)
            if (%lasttot != %tot) { 
              var %totl = $tc.col(2) $tc.addComma($round(%tot,0)) $tc.col(1) $+ - Base: $+ $tc.col(8) $tc.addComma($round(%lasttot,0)) 
              var %ptot = %tot, %totdiff = $calc(%tot - %lasttot)            
              if (%totdiff < 0) { var %iod = decrease, %excol = 7 } | else { var %iod = increase, %excol = 6 }
              var %extl = $tc.col(%excol) $tc.addComma($round(%totdiff,0)) $+ $tc.col(1) ( $+ $tc.col(%excol) $+ $remove($round($calc((%totdiff / %lasttot) * 100),2),$chr(45)) $+ $chr(37) $tc.col(1) $+ %iod $+ )
            }
            else { 
              var %totl = $tc.col(2) $tc.addComma($round(%lasttot,0))
              var %ptot = %lasttot            
            }
            if (%pstr != 0) { var %strl = $tc.col(2) $tc.addComma($round(%str,0)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $round($calc((%str / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ ) - Base: $+ $tc.col(8) $tc.addComma($round(%laststr,0)) } 
            else { var %strl = $tc.col(2) $tc.addComma($round(%laststr,0)) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%laststr / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41) } 
            if (%pdef != 0) { var %defl = $tc.col(2) $tc.addComma($round(%def,0)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $round($calc((%def / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ ) - Base: $+ $tc.col(8) $tc.addComma($round(%lastdef,0)) }
            else { var %defl = $tc.col(2) $tc.addComma($round(%lastdef,0)) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%lastdef / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41) }
            if (%pspd != 0) { var %spdl = $tc.col(2) $tc.addComma($round(%spd,0)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $round($calc((%spd / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ ) - Base: $+ $tc.col(8) $tc.addComma($round(%lastspd,0)) } 
            else { var %spdl = $tc.col(2) $tc.addComma($round(%lastspd,0)) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%lastspd / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41) } 
            if (%pdex != 0) { var %dexl = $tc.col(2) $tc.addComma($round(%dex,0)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $round($calc((%dex / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ ) - Base: $+ $tc.col(8) $tc.addComma($round(%lastdex,0)) } 
            else { var %dexl = $tc.col(2) $tc.addComma($round(%lastdex,0)) $+ $tc.col(1) ( $+ $tc.col(2) $+ $round($calc((%lastdex / %ptot) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ $chr(41) }
            if (%pstr < 0) { var %strt = $tc.col(7) $+ %pstr $+ % }
            elseif (%pstr > 0) { var %strt = $tc.col(6) $+ + $+ %pstr $+ % }
            else { var %strt = $tc.col(2) $+ $remove(%pstr,$chr(45)) $+ % }
            if (%pdef < 0) { var %deft = $tc.col(7) $+ %pdef $+ % }
            elseif (%pdef > 0) { var %deft = $tc.col(6) $+ + $+ %pdef $+ % }
            else { var %deft = $tc.col(2) $+ $remove(%pdef,$chr(45)) $+ % }
            if (%pspd < 0) { var %spdt = $tc.col(7) $+ %pspd $+ % }
            elseif (%pspd > 0) { var %spdt = $tc.col(6) $+ + $+ %pspd $+ % }
            else { var %spdt = $tc.col(2) $+ $remove(%pspd,$chr(45)) $+ % }
            if (%pdex < 0) { var %dext = $tc.col(7) $+ %pdex $+ % }
            elseif (%pdex > 0) { var %dext = $tc.col(6) $+ + $+ %pdex $+ % }
            else { var %dext = $tc.col(2) $+ $remove(%pdex,$chr(45)) $+ % }
            if (%tot != 0) {
              if (%pstr == $null) { var %pstr = 0 } | if (%pdef == $null) { var %pdef = 0 } | if (%pspd == $null) { var %pspd = 0 } | if (%pdex == $null) { var %pdex = 0 } 
              var %age = $calc($tc.time - %lasttime)
              if (%age > 604800) { var %agecol = $tc.col(7) } | else { var %agecol = $tc.col(6) }
              msg # $tc.col(1) $+ %title for $+ $tc.col(2) %retname $tc.colid(%retid,1,2) as of $+ $tc.col(2) $asctime(%lasttime,mmm d) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%lasttime,h:nn:ss tt) $tc.col(1) $+ (GMT/TC time) - $+ %agecol $duration(%age) $tc.col(1) $+ ago.
              if ($calc(%pstr + %pspd + %pdef + %pdex) == 0) { 
                if ($istok(!cstats !cstat,$1,32)) {
                  msg # $tc.col(5) $+ No passive stats saved, repaste your battle stats as is including the percentages. Listing base stats... 
                }
                else {
                  msg # $tc.col(5) $+ No passive stats set, use $tc.col(2) $+ !set tstat $tc.col(5) $+ to set them. Listing base stats... 
                }
              }
              else { msg # $tc.col(4) $+ Using a passive bonus of %strt $tc.col(4) $+ Strength, %deft $tc.col(4) $+ Defense, %spdt $tc.col(4) $+ Speed, %dext $tc.col(4) $+ Dexterity. }
              msg # $tc.col(1) $+ Strength: $+ %strl
              msg # $tc.col(1) $+ Defense: $+ %defl
              msg # $tc.col(1) $+ Speed: $+ %spdl
              msg # $tc.col(1) $+ Dexterity: $+ %dexl
              msg # $tc.col(1) $+ Total: $+ %totl
              if (%extl != $null) { msg # $tc.col(1) $+ Difference: $+ %extl }
              .timer $+ 0.tcflag.spam 1 $tc.gcd noop
            }
            else { 
              if ($2 != $null) { msg # $tc.col(5) $+ They haven't posted any stats! }
              else { msg # $tc.col(5) $+ You haven't posted any stats! }
            }
          }
        }
        else { msg # $tc.col(5) $+ Can't do that, try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds! }
      }

      if ($istok(!vcompare !compare,$1,32)) {
        if (!$timer(0.tcflag.spam)) {
          if ($2 == $null) { msg # $tc.col(3) $+ Compares your saved stats with another users. $tc.col(1) $+ Usage: $+ $tc.col(2) !compare Name AnotherName | halt }
          if ($2 == $nick && $3 == $null) { msg # $tc.col(5) $+ You can't compare yourself with yourself. | halt }
          if ($2 == $3) { msg # $tc.col(5) $+ You can't compare $+ $tc.col(2) $2 $tc.col(5) $+ with $+ $tc.col(2) $3 $+ $tc.col(5) $+ , they're the same person! | halt }
          if ($3 == $null) { var %compleft = $nick | var %compright = $2 }
          else { var %compleft = $2 | var %compright = $3 }          
          var %leftinq = $tc.getid(%compleft), %leftname = $tc.getname(%leftinq)
          if (%leftinq == $null) { var %compfail = %compleft }
          else { 
            var %leftstatsfile $tc.statsfile(%leftinq)
            var %leftsaved = $read($qt(%leftstatsfile), $lines(%leftstatsfile))
            var %lefttime = $gettok(%leftsaved,1,32) | if (%lefttime == $null) { var %lefttime = 0 }
            var %baselstr = $gettok(%leftsaved,2,32) | if (%baselstr == $null) { var %baselstr = 0 }
            var %baseldef = $gettok(%leftsaved,3,32) | if (%baseldef == $null) { var %baseldef = 0 }
            var %baselspd = $gettok(%leftsaved,4,32) | if (%baselspd == $null) { var %baselspd = 0 }
            var %baseldex = $gettok(%leftsaved,5,32) | if (%baseldex == $null) { var %baseldex = 0 }
            var %baseltot = $calc(%baselstr + %baseldef + %baselspd + %baseldex) | if (%baseltot == 0) { var %compfail = %compleft }
          }
          var %rightinq = $tc.getid(%compright), %rightname = $tc.getname(%rightinq)
          if (%rightinq == $null) { var %compfail = %compright }
          else { 
            var %rightstatsfile $tc.statsfile(%rightinq)
            var %rightsaved = $read($qt(%rightstatsfile), $lines(%rightstatsfile))
            var %righttime = $gettok(%rightsaved,1,32) | if (%righttime == $null) { var %righttime = 0 }
            var %baserstr = $gettok(%rightsaved,2,32) | if (%baserstr == $null) { var %baserstr = 0 }
            var %baserdef = $gettok(%rightsaved,3,32) | if (%baserdef == $null) { var %baserdef = 0 }
            var %baserspd = $gettok(%rightsaved,4,32) | if (%baserspd == $null) { var %baserspd = 0 }
            var %baserdex = $gettok(%rightsaved,5,32) | if (%baserdex == $null) { var %baserdex = 0 }
            var %basertot = $calc(%baserstr + %baserdef + %baserspd + %baserdex) | if (%basertot == 0) { var %compfail = %compright } 
          }
          if ($1 == !vcompare) {
            var %vicbonus = 25                          ; Easy change vicodin bonus in case Ched adjusts it. 
            var %lpass = $tc.getset(%leftinq,stat)
            var %lpstr = $calc($gettok(%lpass,1,32) + %vicbonus), %lpdef = $calc($gettok(%lpass,2,32) + %vicbonus)
            var %lpspd = $calc($gettok(%lpass,3,32) + %vicbonus), %lpdex = $calc($gettok(%lpass,4,32) + %vicbonus)       
            var %lvstr = $round($calc(%baselstr * ((%lpstr / 100) + 1)),4), %lvspd = $round($calc(%baselspd * ((%lpspd / 100) + 1)),4)
            var %lvdex = $round($calc(%baseldex * ((%lpdex / 100) + 1)),4), %lvdef = $round($calc(%baseldef * ((%lpdef / 100) + 1)),4)   
            var %lvtot = $round($calc(%lvstr + %lvdef + %lvspd + %lvdex),4), %lext = $round($calc(%lvtot - %lefttotal),4)
            var %leftstr = %lvstr, %leftdef = %lvdef, %leftspd = %lvspd, %leftdex = %lvdex, %lefttot = %lvtot
            var %rpass = $tc.getset(%rightinq,stat)
            var %rpstr = $calc($gettok(%rpass,1,32) + %vicbonus), %rpdef = $calc($gettok(%rpass,2,32) + %vicbonus)
            var %rpspd = $calc($gettok(%rpass,3,32) + %vicbonus), %rpdex = $calc($gettok(%rpass,4,32) + %vicbonus)       
            var %rvstr = $round($calc(%baserstr * ((%rpstr / 100) + 1)),4), %rvspd = $round($calc(%baserspd * ((%rpspd / 100) + 1)),4)
            var %rvdex = $round($calc(%baserdex * ((%rpdex / 100) + 1)),4), %rvdef = $round($calc(%baserdef * ((%rpdef / 100) + 1)),4)   
            var %rvtot = $round($calc(%rvstr + %rvdef + %rvspd + %rvdex),4)
            var %rightstr = %rvstr, %rightdef = %rvdef, %rightspd = %rvspd, %rightdex = %rvdex, %righttot = %rvtot          
            if (%lpstr > %rpstr) { var %strcl = 6, %strcr = 7 } | elseif (%lpstr < %rpstr) { var %strcl = 7, %strcr = 6 } | else { var %strcl = 2, %strcr = 2 }
            if (%lpdef > %rpdef) { var %defcl = 6, %defcr = 7 } | elseif (%lpdef < %rpdef) { var %defcl = 7, %defcr = 6 } | else { var %defl = 2, %defcr = 2 }
            if (%lpspd > %rpspd) { var %spdcl = 6, %spdcr = 7 } | elseif (%lpspd < %rpspd) { var %spdcl = 7, %spdcr = 6 } | else { var %spdcl = 2, %spdcr = 2 }
            if (%lpdex > %rpdex) { var %dexcl = 6, %dexcr = 7 } | elseif (%lpdex < %rpdex) { var %dexcl = 7, %dexcr = 6 } | else { var %dexcl = 2, %dexcr = 2 }
            var %passl =  $tc.col(%strcl) $+ $tc.cleanN(%lpstr) $+ $chr(37) $+ $tc.col(1) $+ / $+ $tc.col(%defcl) $+ $tc.cleanN(%lpdef) $+ $chr(37) $+ $tc.col(1) $+ / $+ $+ $tc.col(%spdcl) $+ $tc.cleanN(%lpspd) $+ $chr(37) $+ $tc.col(1) $+ / $+ $tc.col(%dexcl) $+ $tc.cleanN(%lpdex) $+ $chr(37) 
            var %passr =  $tc.col(%strcr) $+ $tc.cleanN(%rpstr) $+ $chr(37) $+ $tc.col(1) $+ / $+ $tc.col(%defcr) $+ $tc.cleanN(%rpdef) $+ $chr(37) $+ $tc.col(1) $+ / $+ $+ $tc.col(%spdcr) $+ $tc.cleanN(%rpspd) $+ $chr(37) $+ $tc.col(1) $+ / $+ $tc.col(%dexcr) $+ $tc.cleanN(%rpdex) $+ $chr(37)
            var %title = $tc.col(1) $+ Comparing stats (passive + vicodin): $+ $tc.col(2) %leftname $tc.col(1) $+ ( $+ %passl $+ $tc.col(1) $+ ) vs $+ $tc.col(2) %rightname $tc.col(1) $+ ( $+ %passr $+ $tc.col(1) $+ )
          }        
          else { 
            var %leftstr = %baselstr, %leftdef = %baseldef, %leftspd = %baselspd, %leftdex = %baseldex, %lefttot = %baseltot
            var %rightstr = %baserstr, %rightdef = %baserdef, %rightspd = %baserspd, %rightdex = %baserdex, %righttot = %basertot 
            var %title = $tc.col(1) $+ Comparing stats: $+ $tc.col(2) %leftname $tc.col(1) $+ vs $+ $tc.col(2) %rightname
          }
          if (%compfail == $null) {
            msg # %title
            if (%lefttime > %righttime) { var %comptime = $duration($calc(%lefttime - %righttime)) $tc.col(1) $+ apart. }
            if (%righttime > %lefttime) { var %comptime = $duration($calc(%righttime - %lefttime)) $tc.col(1) $+ apart. }
            if (%righttime == %lefttime) { var %comptime = Same time. }
            msg # $tc.col(1) $+ Last posted date: $+ $tc.col(2) $asctime(%lefttime,mmm d) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%lefttime,h:nn:ss tt) $tc.col(1) $+ $chr(45) $+ $tc.col(2) $asctime(%righttime,mmm d) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%righttime,h:nn:ss tt) $tc.col(1) $+ (GMT/TC time) - $+ $tc.col(2) %comptime 
            if (%leftstr > %rightstr) { var %clstr = 6, %crstr = 7, %diffstr = $chr(43) $+ $tc.addComma($calc(%leftstr - %rightstr)) }
            if (%rightstr > %leftstr) { var %clstr = 7, %crstr = 6, %diffstr = $chr(45) $+ $tc.addComma($calc(%rightstr - %leftstr)) }
            if (%rightstr == %leftstr) { var %clstr = 2, %crstr = 2, %diffstr = None }
            msg # $tc.col(1) $+ Strength: $+ $tc.col(%clstr) $tc.addComma(%leftstr) $tc.col(1) $+ $chr(45) $+ $tc.col(%crstr) $tc.addComma(%rightstr) %compstr $tc.col(8) $+ (Diff: %diffstr $+ )
            if (%leftdef > %rightdef) { var %cldef = 6, %crdef = 7, %diffdef = $chr(43) $+ $tc.addComma($calc(%leftdef - %rightdef)) }
            if (%rightdef > %leftdef) { var %cldef = 7, %crdef = 6, %diffdef = $chr(45) $+ $tc.addComma($calc(%rightdef - %leftdef)) }
            if (%rightdef == %leftdef) { var %cldef = 2, %crdef = 2, %diffdef = None }
            msg # $tc.col(1) $+ Defense: $+ $tc.col(%cldef) $tc.addComma(%leftdef) $tc.col(1) $+ $chr(45) $+ $tc.col(%crdef) $tc.addComma(%rightdef) %compdef $tc.col(8) $+ (Diff: %diffdef $+ )
            if (%leftspd > %rightspd) { var %clspd = 6, %crspd = 7, %diffspd = $chr(43) $+ $tc.addComma($calc(%leftspd - %rightspd)) }
            if (%rightspd > %leftspd) { var %clspd = 7, %crspd = 6, %diffspd = $chr(45) $+ $tc.addComma($calc(%rightspd - %leftspd)) }
            if (%rightspd == %leftspd) { var %clspd = 2, %crspd = 2, %diffspd = None }
            msg # $tc.col(1) $+ Speed: $+ $tc.col(%clspd) $tc.addComma(%leftspd) $tc.col(1) $+ $chr(45) $+ $tc.col(%crspd) $tc.addComma(%rightspd) %compspd $tc.col(8) $+ (Diff: %diffspd $+ )
            if (%leftdex > %rightdex) { var %cldex = 6, %crdex = 7, %diffdex = $chr(43) $+ $tc.addComma($calc(%leftdex - %rightdex)) }
            if (%rightdex > %leftdex) { var %cldex = 7, %crdex = 6, %diffdex = $chr(45) $+ $tc.addComma($calc(%rightdex - %leftdex)) }
            if (%rightdex == %leftdex) { var %cldex = 2, %crdex = 2, %diffdex = None }
            msg # $tc.col(1) $+ Dexterity: $+ $tc.col(%cldex) $tc.addComma(%leftdex) $tc.col(1) $+ $chr(45) $+ $tc.col(%crdex) $tc.addComma(%rightdex) %compdex $tc.col(8) $+ (Diff: %diffdex $+ )
            if (%lefttot > %righttot) { var %cltot = 6, %crtot = 7, %difftot = $chr(43) $+ $tc.addComma($calc(%lefttot - %righttot)) }
            if (%righttot > %lefttot) { var %cltot = 7, %crtot = 6, %difftot = $chr(45) $+ $tc.addComma($calc(%righttot - %lefttot)) }
            if (%righttot == %lefttot) { var %cltot = 2, %crtot = 2, %difftot = None }
            msg # $tc.col(1) $+ Total: $+ $tc.col(%cltot) $tc.addComma(%lefttot) $tc.col(1) $+ $chr(45) $+ $tc.col(%crtot) $tc.addComma(%righttot) %comptot $tc.col(8) $+ (Diff: %difftot $+ )        
            .timer $+ 0.tcflag.spam 1 $tc.gcd noop
          }
          else {
            if (%compfail == $nick) { msg # $tc.col(5) $+ You haven't pasted any stats! }
            else { msg # $tc.col(5) $+ They haven't posted any stats! }
          }
        }
        else { msg # $tc.col(5) $+ Can't do that, try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds! }
      }    

      if ($1 == !growth) {
        if (!$timer(0.tcflag.spam)) {
          if ($2 == $null) { msg # $tc.col(3) $+ Displays your/another person's stat growth over X days. $tc.col(1) $+ Usage: $+ $tc.col(2) !growth (Name) Days | halt } 
          else {
            if ($2 isnum) { 
              var %grow.input = $remove($2,$chr(45)) 
              if ($3 != $null) { var %find = $3 } | else { var %find = $nick }
            } 
            elseif ($3 isnum) { var %grow.input = $remove($3,$chr(45)), %find = $2 }
            else { msg # $tc.col(5) $+ Try again: $+ $tc.col(2) !growth (Name) Days) | halt }
          }
          if (%grow.input > 1000000) { msg # $tc.col(5) $+ Liar. | halt }
          var %ret = $tc.getid(%find)
          if (%ret == $null) { msg # $tc.col(5) $+ Search for $+ $tc.col(2) %find $tc.col(5) $+ was not found. }
          else {
            var %grow.targdate = $calc($tc.time - (%grow.input * 86400))
            var %grow.file = $tc.statsfile(%ret)
            var %grow.fileeof = $lines(%grow.file)
            if (%grow.fileeof < 2) { msg # $tc.col(5) $+ You need to paste your stats more than once to create a growth profile. | halt }
            var %grow.pos = 1
            while (%grow.pos < %grow.fileeof) {
              var %grow.saved $read($qt(%grow.file), %grow.pos)
              var %grow.readtime $gettok(%grow.saved,1,32)
              var %grow.readdist $abs($calc(%grow.targdate - %grow.readtime))
              if (%grow.readdist <= %grow.dist || %grow.dist == $null) {
                var %grow.time = $gettok(%grow.saved,1,32)
                var %grow.str = $gettok(%grow.saved,2,32)
                var %grow.def = $gettok(%grow.saved,3,32)
                var %grow.spd = $gettok(%grow.saved,4,32)
                var %grow.dex = $gettok(%grow.saved,5,32)
                var %grow.total = $calc(%grow.str + %grow.def + %grow.spd + %grow.dex)
                var %grow.dist = %grow.readdist
              }
              inc %grow.pos
            }
            var %grow.saved = $read($qt(%grow.file), $lines($qt(%grow.file)))
            var %grow.lasttime = $gettok(%grow.saved,1,32)
            var %grow.laststr = $gettok(%grow.saved,2,32)
            var %grow.lastdef = $gettok(%grow.saved,3,32)
            var %grow.lastspd = $gettok(%grow.saved,4,32)
            var %grow.lastdex = $gettok(%grow.saved,5,32)
            var %grow.age = $duration($calc($tc.time - %grow.time))
            var %grow.lastage = $duration($calc($tc.time - %grow.lasttime))
            if ($asctime(%grow.time,yyyy) != $asctime($tc.time,yyyy)) { var %grow.disptime = $asctime(%grow.time,mmm d yyyy) } | else { var %grow.disptime = $asctime(%grow.time,mmm d) }
            if ($asctime(%grow.lasttime,yyyy) != $asctime($tc.time,yyyy)) { var %grow.displasttime = $asctime(%grow.lasttime,mmm d yyyy) } | else { var %grow.displasttime = $asctime(%grow.lasttime,mmm d) }
            var %grow.lasttotal $calc(%grow.laststr + %grow.lastdef + %grow.lastspd + %grow.lastdex)
            msg # $tc.col(1) $+ Stat growth for $+ $tc.col(2) $tc.getname(%ret) $tc.colid(%ret,1,2) $tc.col(1) $+ over the last $+ $tc.col(2) %grow.input $tc.col(1) $+ days.
            msg # $tc.col(1) $+ Dates: $+ $tc.col(2) %grow.disptime $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%grow.time,h:nn:ss tt) $tc.col(8) $+ ( $+ %grow.age ago) $tc.col(1) $+ to $+ $tc.col(2) %grow.displasttime $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%grow.lasttime,h:nn:ss tt) $tc.col(8) $+ ( $+ %grow.lastage ago) $tc.col(1) $+ (GMT/TC time). 
            var %grow.chanstring $tc.col(1) $+ Strength: $+ $tc.col(2) $tc.addComma(%grow.str) $tc.col(1) $+ to $+ $tc.col(2) $tc.addComma(%grow.laststr) $tc.col(1) $+ - 
            if ($calc(%grow.laststr - %grow.str) == 0) { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(7) None $+ $tc.col(1) $+ ) }
            else { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(6) $tc.addComma($calc(%grow.laststr - %grow.str)) $tc.col(1) $+ - $+ $tc.col(2) $tc.addComma($round($calc(((%grow.laststr / %grow.str) * 100) - 100),2)) $+ $chr(37) $+ $tc.col(1) $+ ) }
            msg # %grow.chanstring
            var %grow.chanstring $tc.col(1) $+ Defense: $+ $tc.col(2) $tc.addComma(%grow.def) $tc.col(1) $+ to $+ $tc.col(2) $tc.addComma(%grow.lastdef) $tc.col(1) $+ - 
            if ($calc(%grow.lastdef - %grow.def) == 0) { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(7) None $+ $tc.col(1) $+ ) }
            else { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(6) $tc.addComma($calc(%grow.lastdef - %grow.def)) $tc.col(1) $+ - $+ $tc.col(2) $tc.addComma($round($calc(((%grow.lastdef / %grow.def) * 100) - 100),2)) $+ $chr(37) $+ $tc.col(1) $+ ) }
            msg # %grow.chanstring
            var %grow.chanstring $tc.col(1) $+ Speed: $+ $tc.col(2) $tc.addComma(%grow.spd) $tc.col(1) $+ to $+ $tc.col(2) $tc.addComma(%grow.lastspd) $tc.col(1) $+ - 
            if ($calc(%grow.lastspd - %grow.spd) == 0) { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(7) None $+ $tc.col(1) $+ ) }
            else { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(6) $tc.addComma($calc(%grow.lastspd - %grow.spd)) $tc.col(1) $+ - $+ $tc.col(2) $tc.addComma($round($calc(((%grow.lastspd / %grow.spd) * 100) - 100),2)) $+ $chr(37) $+ $tc.col(1) $+ ) }
            msg # %grow.chanstring
            var %grow.chanstring $tc.col(1) $+ Dexterity: $+ $tc.col(2) $tc.addComma(%grow.dex) $tc.col(1) $+ to $+ $tc.col(2) $tc.addComma(%grow.lastdex) $tc.col(1) $+ - 
            if ($calc(%grow.lastdex - %grow.dex) == 0) { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(7) None $+ $tc.col(1) $+ ) }
            else { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(6) $tc.addComma($calc(%grow.lastdex - %grow.dex)) $tc.col(1) $+ - $+ $tc.col(2) $tc.addComma($round($calc(((%grow.lastdex / %grow.dex) * 100) - 100),2)) $+ $chr(37) $+ $tc.col(1) $+ ) }
            msg # %grow.chanstring
            var %grow.chanstring $tc.col(1) $+ Total: $+ $tc.col(2) $tc.addComma(%grow.total) $tc.col(1) $+ to $+ $tc.col(2) $tc.addComma(%grow.lasttotal) $tc.col(1) $+ - 
            if ($calc(%grow.lasttotal - %grow.total) == 0) { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(7) None $+ $tc.col(1) $+ ) }
            else { var %grow.chanstring = %grow.chanstring $+ $chr(32) $+ (Growth: $+ $tc.col(6) $tc.addComma($calc(%grow.lasttotal - %grow.total)) $tc.col(1) $+ - $+ $tc.col(2) $tc.addComma($round($calc(((%grow.lasttotal / %grow.total) * 100) - 100),2)) $+ $chr(37) $+ $tc.col(1) $+ ) }
            msg # %grow.chanstring        
            .timer $+ 0.tcflag.spam 1 $tc.gcd noop
          }
        }
        else { msg # $tc.col(5) $+ Can't do that, try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds! }
      }
    }

    ; --------------------------------- reminder bot code ---------------------------------

    if ($1 == !remind) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Sends you a reminder notice with message if specified. $tc.col(1) $+ Usage: $+ $tc.col(2) !remind (Duration) (Messsage) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.remind)) { .timer $+ $nick $+ .tc.remind off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your reminder has been cancelled. }
        else { msg # $tc.col(5) $+ You have no reminder timer set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      if ($3 != $null) { 
        .timer $+ $nick $+ .tc.remind -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , you asked me to remind you: $+ $tc.col(2) $s($3-)
        msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will send you your message in $+ $tc.col(2) $duration(%dur) $tc.col(1) $+ ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
      }
      else {
        .timer $+ $nick $+ .tc.remind -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , you asked me to remind you. 
        msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
      }
    }
    if ($1 == !drug || $1 == !durg) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Sends a message when your drug timer is up. $+ $tc.col(1) Usage: $+ $tc.col(2) !drug (Duration) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.drug)) { .timer $+ $nick $+ .tc.drug off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your drug alert has been cancelled. }
        else { msg # $tc.col(5) $+ You have no drug alert set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.drug -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your drug cooldown is up! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when your drug cooldown is up in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }    
    if ($1 == !edu) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Sends a message when your education is up. $+ $tc.col(1) Usage: $+ $tc.col(2) !edu (Duration) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.edu)) { .timer $+ $nick $+ .tc.edu off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your education alert has been cancelled. }
        else { msg # $tc.col(5) $+ You have no education alert set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.edu -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your education is finished! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when your education is finished in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }    
    if ($1 == !hosp) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Sends a message when you're out of the hospital. $+ $tc.col(1) Usage: $+ $tc.col(2) !hosp (Duration) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.hosp)) { .timer $+ $nick $+ .tc.hosp off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your hospital release notice has been cancelled. }
        else { msg # $tc.col(5) $+ You have no hospital release notice set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.hosp -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , you're out of the hospital! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when you're released from the hospital in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }  
    if ($1 == !cd || $1 == !booster) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Sends a message when your booster timer is empty. $+ $tc.col(1) Usage: $+ $tc.col(2) !boost (Duration) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.boost)) { .timer $+ $nick $+ .tc.boost off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your booster alert has been cancelled. }
        else { msg # $tc.col(5) $+ You have no booster alert set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.boost -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your booster cooldown is up! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when your booster cooldown is up in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }    
    if ($1 == !race) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Sends a message when your race is finished. $+ $tc.col(1) Usage: $+ $tc.col(2) !race (Duration) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.race)) { .timer $+ $nick $+ .tc.race off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your race timer has been cancelled. }
        else { msg # $tc.col(5) $+ You have no race timer set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.race -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your race is finished! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when your race has finished in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }
    if ($1 == !travel) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Sends a message when you land. $+ $tc.col(1) Usage: $+ $tc.col(2) !travel (Duration) | halt }        
      if ($2 == off) {
        if ($timer($nick $+ .tc.travel)) { .timer $+ $nick $+ .tc.travel off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your travel timer has been cancelled. }
        else { msg # $tc.col(5) $+ You have no travel timer set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.travel -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , you have landed! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when you have landed in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }
    if ($1 == !pizza) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Reminds you about your pizza. $+ $tc.col(1) Usage: $+ $tc.col(2) !pizza (Duration) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.pizza)) { .timer $+ $nick $+ .tc.pizza off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your pizza alert has been cancelled. }
        else { msg # $tc.col(5) $+ You have no pizza alert set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.pizza -co 1 %dur .msg $nick $tc.col(1) $+ Piiiizzzzzzzzzzzaaaa!!!!!!!!! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will send you a message in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }
    if ($1 == !mow) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Special timer for Man-O-War, because he is special. $+ $tc.col(1) Usage: $+ $tc.col(2) !mow (Duration) | halt }
      if ($2 == off) {
        if ($timer($nick $+ .tc.mow)) { .timer $+ $nick $+ .tc.mow off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your MOW alert has been cancelled. }
        else { msg # $tc.col(5) $+ You have no MOW alert set! }
        halt
      }
      if ($2 !isnum) { var %dur = $tc.timeconv($2) }
      else { var %dur = $calc($2 * 60) }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.mow -co 1 %dur .msg $nick $tc.col(1) $+ mow... mow... mow... mow... mow... 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will send you a message in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }
    if ($1-4 == You'll have to wait && $6-11 == before you can take another drug.) {
      if ($numtok($5,58) == 2) { var %dur = $calc(($gettok($5,1,58) * 60) + $gettok($5,2,58)) }
      elseif ($numtok($5,58) == 3) { var %dur = $calc(($gettok($5,1,58) * 60 * 60) + ($gettok($5,2,58) * 60) + $gettok($5,3,58)) }
      else { var %dur = $5 }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.drug -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your drug cooldown is up! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when your drug cooldown is up in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }
    if ($1-6 == Your booster cooldown is currently at) {
      if ($numtok($7,58) == 2) { var %dur = $calc(($gettok($7,1,58) * 60) + $gettok($7,2,58)) }
      elseif ($numtok($7,58) == 3) { var %dur = $calc(($gettok($7,1,58) * 60 * 60) + ($gettok($7,2,58) * 60) + $remove($gettok($7,3,58),$chr(46))) }
      else { var %dur = $7 }
      if (%dur == 0 || %dur > $tc.timemax) { msg # $tc.col(5) $+ Nope. | halt }
      .timer $+ $nick $+ .tc.boost -co 1 %dur .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your booster cooldown is up! 
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will remind you when your booster cooldown is up in $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %dur),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }
    if (($4-6 == Remaining Flight Time) || ($5-7 == Remaining Flight Time)) {
      if ($4 == Remaining) { var %dest = $3, %time = $8 }
      else { var %dest = $3-4, %time = $9 }
      if ($gettok(%dest,1,32) == to) { var %dest = Torn }
      var %timercd = $tc.timeconv(%time)
      if (%timercd == 0) { msg # $tc.col(5) $+ Nope. | halt }
      if (%timercd > $tc.timemax) { msg # $tc.col(5) $+ Too long. | halt }
      .timer $+ $nick $+ .tc.travel -co 1 %timercd .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , you have landed at $+ $tc.col(2) $s($remove(%dest,$chr(46))) $+ $tc.col(1) $+ !  
      msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will send you a notice in $+ $tc.col(2) $duration(%timercd) $tc.col(1) $+ when you land at $+ $tc.col(2) $remove(%dest,$chr(46)) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ $asctime($calc($tc.time + %timercd),h:nn tt) $tc.col(1) $+ GMT/TC time)
    }
    if ($1 == You) {
      if ($1-8 == You are currently on the plane flying to) {      
        if ($15 isnum) { var %dest = $9 | var %dist = $15 }
        elseif ($16 isnum) { var %dest = $9-10 | var %dist = $16 }
        else { halt }
      }
      if ($1-10 == You step on board the business class flight direct to) {
        if ($11 == Ciudad) { var %dist = 8 | var %dest = Mexico }
        if ($11 == George) { var %dist = 11 | var %dest = Cayman Islands }
        if ($11 == Toronto) { var %dist = 12 | var %dest = Canada }
        if ($11 == Honolulu) { var %dist = 40 | var %dest = Hawaii }
        if ($11 == London) { var %dist = 48 | var %dest = United Kingdom }
        if ($11 == Buenos) { var %dist = 50 | var %dest = Argentina }
        if ($11 == Zurich) { var %dist = 53 | var %dest = Switzerland }                
        if ($11 == Tokyo) { var %dist = 68 | var %dest = Japan }
        if ($11 == Beijing) { var %dist = 72 | var %dest = China }
        if ($11 == Dubai) { var %dist = 81 | var %dest = UAE }
        if ($11 == Johannesburg) { var %dist = 89 | var %dest = South Africa }
      }
      if ($1-9 == You step on board WLT's private jet heading for) {
        var %dest = $remove($10-11,$chr(46))
        if (%dest == Mexico) { var %dist = 13 }
        if (%dest == Cayman Islands) { var %dist = 18 }
        if (%dest == Canada) { var %dist = 20 }
        if (%dest == Hawaii) { var %dist = 67 }
        if (%dest == United Kingdom) { var %dist = 80 }
        if (%dest == Argentina) { var %dist = 83 }
        if (%dest == Switzerland) { var %dist = 88 }
        if (%dest == Japan) { var %dist = 113 }
        if (%dest == China) { var %dist = 121 }
        if (%dest == UAE) { var %dist = 135 }
        if (%dest == South Africa) { var %dist = 149 }
      }
      if ($1-15 == You make your way over to the airstrip and tell your pilot to head for) {
        var %dest = $remove($16-17,$chr(46))
        if (%dest == Mexico) { var %dist = 18 }
        if (%dest == Cayman Islands) { var %dist = 25 }
        if (%dest == Canada) { var %dist = 29 }
        if (%dest == Hawaii) { var %dist = 94 }
        if (%dest == United Kingdom) { var %dist = 111 }
        if (%dest == Argentina) { var %dist = 117 }
        if (%dest == Switzerland) { var %dist = 123 }
        if (%dest == Japan) { var %dist = 158 }
        if (%dest == China) { var %dist = 169 }
        if (%dest == UAE) { var %dist = 190 }
        if (%dest == South Africa) { var %dist = 208 }
      }
      if ($1-6 == You board the airliner heading for) {
        var %dest = $remove($7-8,$chr(46))
        if (%dest == Mexico) { var %dist = 26 }
        if (%dest == Cayman Islands) { var %dist = 35 }
        if (%dest == Canada) { var %dist = 41 }
        if (%dest == Hawaii) { var %dist = 134 }
        if (%dest == United Kingdom) { var %dist = 159 }
        if (%dest == Argentina) { var %dist = 167 }
        if (%dest == Switzerland) { var %dist = 175 }
        if (%dest == Japan) { var %dist = 225 }
        if (%dest == China) { var %dist = 242 }
        if (%dest == UAE) { var %dist = 271 }
        if (%dest == South Africa) { var %dist = 297 }
      }
      if (%dist != $null) {
        if (%dist < 1) { msg # $tc.col(4) $+ Liar. }
        elseif (%dist <= $tc.timemax) { 
          var %timercd = $calc(%dist * 60) | var %timerend = $asctime($calc($tc.time + %timercd),h:nn tt)
          .timer $+ $nick $+ .tc.travel -co 1 %timercd .msg $nick $tc.col(2) $+ $nick $+ $tc.col(1) $+ , you have landed at $+ $tc.col(2) $s($remove(%dest,$chr(46))) $+ $tc.col(1) $+ !  
          msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , I will send you a notice in $+ $tc.col(2) $duration(%timercd) $tc.col(1) $+ when you land at $+ $tc.col(2) $remove(%dest,$chr(46)) $+ $tc.col(1) $+ . ( $+ $tc.col(2) $+ %timerend $tc.col(1) $+ GMT/TC time)
        }
        else { msg # $tc.col(4) $+ No can do, timer too long. }
      }
    }     
    if ($istok(!life !nerve !happy !energy,$1,32)) {
      var %res = $remove($1,$chr(33))
      if (!$timer($nick $+ .tc. $+ %res)) { msg # $tc.col(5) $+ You have no full %res alert set! | halt }
      else { .timer $+ $nick $+ .tc. $+ %res off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , your %res alert has been cancelled. | halt }
    }
    if (%tc.echk == $nick) { 
      unset %tc.echk
      var %cur = $gettok($1,1,47), %max = $gettok($1,2,47)
      if (%cur isnum && %cur >= %max) { 
        var %atks = $floor($calc(%cur / 25))
        msg # $tc.col(2) $+ $nick $+ $tc.col(6) $+ , you can do $+ $tc.col(2) %atks $tc.col(6) $+ attacks!
        halt
      }
    }
    if ($istok(Life: Nerve: Happy: Energy:,$1,32)) {
      var %cur = $calc($tc.cleanN($gettok($2-4,1,47))) | var %max = $calc($tc.cleanN($gettok($gettok($2-4,2,47),1,32)))
      if (%cur isnum && %max isnum) {
        if ($1 == Life:) { var %curlim = 10000000, %maxlim = %curlim, %restime = 5, %pertick = $calc(%max * 0.06), %res = life } 
        if ($1 == Nerve:) { var %curlim = 500, %maxlim = 100, %restime = 5, %pertick = 1, %res = nerve } 
        if ($1 == Happy:) { var %curlim = 50000, %maxlim = 20000, %restime = 15, %pertick = 5, %res = happy }
        if ($1 == Energy:) { 
          if (%max == 15) { var %max = 150 } | if (%max == 10) { var %max = 100 }
          var %curlim = 1000, %maxlim = 150, %pertick = 5, %res = energy 
          if (%max == 150) { var %restime = 10 } | else { var %restime = 15 }
        } 
        if (%cur > %curlim || %max > %maxlim) { msg # $tc.col(5) $+ Nope. | halt }
        if (%cur >= %max) { 
          var %atks = $floor($calc(%cur / 25))
          if (%atks > 0) {
            if ($1 == Energy:) { msg # $tc.col(2) $+ $nick $+ $tc.col(6) $+ , you can do $+ $tc.col(2) %atks $tc.col(6) $+ attacks! } 
            else { msg # $tc.col(5) $+ You are already at full $remove($1,$chr(58)) $+ ! }
          }
          else { 
            if ($1 == Energy:) { set %tc.echk $nick }
          }
          halt
        }
        if (%cur == 0 && $1 == Life:) { msg # $tc.col(2) $+ $nick $+ $tc.col(5) $+ , you are at zero life. Someone attacked you but hasn't chosen an action yet! | halt }
        var %tick = 0, %gen = %cur
        while (%gen < %max) { 
          inc %tick 
          var %time = $int($calc((($tc.time / 60) / %restime) + %tick))
          var %gen = $calc(%gen + %pertick) 
        }
        var %curtime = $asctime($tc.time,h:nn tt) 
        var %fulltime = $asctime($calc((%time * %restime) * 60),h:nn tt), %fulldur = $duration($calc(((%time * %restime) * 60) - $tc.time))
        var %duration = $calc(((%time * 60) * %restime) - $tc.time)
        if (%res == life) { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , the current time is: $+ $tc.col(2) %curtime $tc.col(1) $+ so your %res bar should be full in $+ $tc.col(2) %fulldur $tc.col(1) $+ ( $+ $tc.col(2) $+ %fulltime $tc.col(1) $+ GMT/TC Time.), healing for $+ $tc.col(2) $tc.addComma(%pertick) $tc.col(1) $+ life every $+ $tc.col(2) %restime $tc.col(1) $+ minutes to the hour. }
        else { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , the current time is: $+ $tc.col(2) %curtime $tc.col(1) $+ so your %res bar should be full in $+ $tc.col(2) %fulldur $tc.col(1) $+ ( $+ $tc.col(2) $+ %fulltime $tc.col(1) $+ GMT/TC Time.) }
        if (%duration > 172800) { 
          if ($1 == Happy:) { msg # $tc.col(5) $+ Yea, no. Timer too long. Go watch some porn or rehab... }
          else { msg # $tc.col(5) $+ Yea, no. Timer too long. }
        }
        else { .timer $+ $nick $+ .tc. $+ %res -co 1 %duration .msg $nick $tc.col(1) $+ You are at full %res $+ ! }
      } 
      else { msg # $tc.col(5) $+ Garbage in, garbage out. }
    }

    if ($1 == !alert || $1 == !alerts) {
      if ($2 == off) { .timer $+ $nick $+ .tc.* off | msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , all your alerts have been cancelled. }
      else { 
        var %loc = $timer(0) | var %point = 1 | var %count = 0
        while (%point <= %loc) { 
          if ($gettok($timer(%point),1,46) == $nick && $gettok($timer(%point),2,46) == tc) { 
            if (!%count) { msg # $tc.col(1) $+ Alerts for $+ $tc.col(2) $nick $+ $tc.col(1) $+ : } 
            inc %count 
            if (!$istok(a5 s5,$gettok($timer(%point),2,46),32)) { msg # $eval($strip($gettok($timer(%point).com,3-,32),u),2) $+ $chr(32) $+ $tc.col(1) $+ - $+ $tc.col(2) $duration($calc($timer(%point).secs)) $tc.col(1) $+ remaining } 
          } 
          inc %point 
        }
        if (!%count) { msg # $tc.col(2) $+ $nick $+ $tc.col(5) $+ , you have no alerts set! } 
      }
    }

    ; --------------------------------- spy storage commands ---------------------------------

    if ($readini($tc.cfgfile,n,Config,spyen) == yes) {
      if (%tc.spy == $nick) {
        var %tablename = spy. $+ %tc.spy
        if ($istok(Strength: Speed: Defense: Dexterity: Total:,$1,32)) { 
          .timer $+ 0.tcflag.spyauto off 
          .timer $+ %tc.spy $+ .spy -co 1 5 tc.spysave 
        }
        if ($1 == Name:) { 
          hadd -m %tablename name $2
          hadd -m %tablename id $remove($3,$chr(91),$chr(93)) 
        }
        if ($1 == Level:) { hadd -m %tablename lvl $2 }
        if ($1 == Strength:) { hadd -m %tablename str $remove($2,$chr(44),$chr(32)) }
        if ($1 == Speed:) { hadd -m %tablename spd $remove($2,$chr(44),$chr(32)) }
        if ($1 == Defense:) { hadd -m %tablename def $remove($2,$chr(44),$chr(32)) }
        if ($1 == Dexterity:) { hadd -m %tablename dex $remove($2,$chr(44),$chr(32)) }
        if ($1 == Total:) { hadd -m %tablename totb $remove($2,$chr(44),$chr(32)) }
      }

      if (%tc.oldspy == $nick) {
        var %tablename = spy. $+ %tc.oldspy
        if ($istok(Name: Level: Strength: Speed: Defense: Dexterity: Total: Manual Intelligence: Endurance: Money:,$1,32)) { 
          .timer $+ 0.tcflag.spyauto off 
          .timer $+ %tc.oldspy $+ .spy -co 1 5 tc.spysave
        }
        if ($1 == Name:) { 
          hadd -m %tablename name $2
          hadd -m %tablename id $remove($3,$chr(91),$chr(93)) 
        }
        if ($1 == Level:) { hadd -m %tablename lvl $2 }
        if ($1 == Strength:) { hadd -m %tablename str $remove($2,$chr(44),$chr(32)) }
        if ($1 == Speed:) { hadd -m %tablename spd $remove($2,$chr(44),$chr(32)) }
        if ($1 == Defense:) { hadd -m %tablename def $remove($2,$chr(44),$chr(32)) }
        if ($1 == Dexterity:) { hadd -m %tablename dex $remove($2,$chr(44),$chr(32)) }
        if ($1 == Total:) { 
          if ($hget(%tablename,end)) { hadd -m %tablename totw $remove($2,$chr(44),$chr(32)) }
          else { hadd -m %tablename totb $remove($2,$chr(44),$chr(32)) }
        }
        if ($1 == Manual) { hadd -m %tablename man $remove($3,$chr(44),$chr(32)) }
        if ($1 == Intelligence:) { hadd -m %tablename int $remove($2,$chr(44),$chr(32)) }
        if ($1 == Endurance:) { hadd -m %tablename end $remove($2,$chr(44),$chr(32)) }
        if ($1 == Money:) { hadd -m %tablename cash $remove($2,$chr(36),$chr(44),$chr(32)) }
      }

      if ($1 == !purgespy || $1 == !spypurge) { 
        msg # $tc.col(1) $+ The spy storage script should be 'unstuck' now. Sorry about that...
        .timer0.tcflag.spyauto off | unset %tc.spy | unset %tc.oldspy | unset %tc.spychan | unset %tcsprof.* 
      }

      if ($1 == !addspy || $1 == !storespy) {
        if (%tc.oldspy != $null || %tc.spy != $null) { msg # $tc.col(5) $+ I'm already watching for a spy paste! | halt }
        elseif (($2 == $null) || (auto !isin $2- && $numtok($2-,32) < 7)) { msg # $tc.addspyhelp | halt } 
        elseif ($2 == old && $3 == $null) { var %spyoldauto = 1 }
        elseif ($3 == old && $4 == $null) { var %spyoldauto = 1 }
        elseif ($4 == old && $5 == $null) { var %spyoldauto = 1, %spyname = $remove($2,$chr(32)), %spyid = $remove($3,$chr(32),$chr(93),$chr(91)) }
        elseif ($2 == auto && $3 == $null) { var %spyauto = 1 }
        elseif ($3 == auto && $4 == $null) { msg # $tc.addspyhelp | halt }
        elseif ($4 == auto && $5 == $null) { var %spyauto = 1, %spyname = $remove($2,$chr(32)), %spyid = $remove($3,$chr(32),$chr(93),$chr(91)) }
        else { 
          var %tablename = spy. $+ $nick
          var %chk = 1, %chkstr = $replace($2-,Strength:,$chr(32) $+ Strength:,Speed:,$chr(32) $+ Speed:,Dexterity:,$chr(32) $+ Dexterity:,Defense:,$chr(32) $+ Defense:,Total:,$chr(32) $+ Total:)
          while (%chk < $numtok(%chkstr,32)) {
            if ($gettok(%chkstr,%chk,32) == Speed:) { var %spd = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Strength:) { var %str = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Defense:) { var %def = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Dexterity:) { var %dex = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Total:) { var %tot = $tc.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) | var %totmark = $calc(%chk + 1) } 
            inc %chk
          }
          if (%spd == $null) {
            var %chkspd = $tc.cleanN($5)
            if (%chkspd isnum) { var %spd = %chkspd }
          }
          if (%str == $null) {
            var %chkstr = $tc.cleanN($4)
            if (%chkstr isnum) { var %str = %chkstr }
          }
          if (%def == $null) { 
            var %chkdef = $tc.cleanN($7)
            if (%chkdef isnum) { var %def = %chkdef }
          }
          if (%dex == $null) {
            var %chkdex = $tc.cleanN($6)
            if (%chkdex isnum) { var %dex = %chkdex) }
          }
          if (%tot == $null) {
            var %chktot = $tc.cleanN($8)
            if (%chktot isnum) { var %tot = %chktot }
          }
          if (%totmark != $null) {
            var %clnstart = 1, %fac = $2-
            while (%clnstart <= %totmark) { var %fac = $deltok(%fac,1,32) | inc %clnstart }
          }
          elseif ($9 != $null) { var %fac = $9- }
          else { var %fac = $null }
          hadd -m %tablename name $2 
          hadd -m %tablename id $remove($3,$chr(91),$chr(93))
          hadd -m %tablename spd %spd
          hadd -m %tablename str %str
          hadd -m %tablename def %def
          hadd -m %tablename dex %dex
          hadd -m %tablename totb %tot
          if (%fac != $null) { hadd -m %tablename fact %fac }
          set %tc.spychan #
          set %tc.spy $nick 
          tc.spysave  
        } 
        if (%spyoldauto != $null) {
          set %tc.oldspy $nick 
          set %tc.spychan #
          if (%tablename == $null) { var %tablename = spy. $+ $nick }
          if (%spyname != $null) { hadd -m %tablename name %spyname }
          if (%spyid != $null) { hadd -m %tablename id %spyid }
          .timer $+ 0.tcflag.spyauto 1 10 tc.spycancel #
          msg # $tc.col(1) $+ Go ahead and paste your pre-RESPO spy report $+ $tc.col(2) $nick $+ $tc.col(1) $+ , I'm watching for it. I will attempt to save after or cancel 10 seconds. $tc.col(8) $+ If the bot gets stuck, use !purgespy to fix.       
        }
        if (%spyauto != $null) {
          set %tc.spy $nick 
          set %tc.spychan #
          if (%tablename == $null) { var %tablename = spy. $+ $nick }
          if (%spyname != $null) { hadd -m %tablename name %spyname }
          if (%spyid != $null) { hadd -m %tablename id %spyid }
          .timer $+ 0.tcflag.spyauto 1 10 tc.spycancel #
          msg # $tc.col(1) $+ Go ahead and paste your spy report $+ $tc.col(2) $nick $+ $tc.col(1) $+ , I'm watching for it. I will attempt to save or cancel after 10 seconds. $tc.col(8) $+ If the bot gets stuck, use !purgespy to fix.
        }
      }

      if ($1 == !spy || $1 == !read) {
        if ($2 == $null) { msg # $tc.col(3) $+ Displays stored spy record. $tc.col(1) $+ Usage: $tc.col(2) $+ !spy Name $tc.col(1) $+ or $+ $tc.col(2) !read ID | halt }
        if ($exists($tc.spyfile)) { 
          var %ptr = 1, %eof = $lines($tc.spyfile) 
          while (%ptr <= %eof) {
            if ($1 == !read) { var %look = $gettok($read($tc.spyfile,%ptr),2,9) }
            else { var %look = $gettok($read($tc.spyfile,%ptr),1,9) }
            if (%look == $2) { var %loc = %ptr | break }
            inc %ptr
          }
        }
        if (%loc != $null) {
          var %name = $gettok($read($tc.spyfile,%loc),1,9) 
          var %id = $gettok($read($tc.spyfile,%loc),2,9)
          var %lvl = $gettok($read($tc.spyfile,%loc),3,9) 
          var %fact = $gettok($read($tc.spyfile,%loc),4,9)
          var %spd = $gettok($read($tc.spyfile,%loc),5,9) 
          var %str = $gettok($read($tc.spyfile,%loc),6,9) 
          var %def = $gettok($read($tc.spyfile,%loc),7,9) 
          var %dex = $gettok($read($tc.spyfile,%loc),8,9)    
          var %totb = $gettok($read($tc.spyfile,%loc),9,9) 
          var %man = $gettok($read($tc.spyfile,%loc),10,9) 
          var %int = $gettok($read($tc.spyfile,%loc),11,9) 
          var %end = $gettok($read($tc.spyfile,%loc),12,9)  
          var %totw = $gettok($read($tc.spyfile,%loc),13,9) 
          var %cash = $gettok($read($tc.spyfile,%loc),14,9)  
          var %aname = $gettok($read($tc.spyfile,%loc),15,9) 
          var %adate = $gettok($read($tc.spyfile,%loc),16,9)
          var %ctotb = $calc(%spd + %str + %def + %dex)
          var %bknown = 0
          if (%spd == 0) { var %dspd = $tc.col(7) $+ N/A } | else { inc %bknown | var %dspd = $tc.addComma(%spd) }
          if (%str == 0) { var %dstr = $tc.col(7) $+ N/A } | else { inc %bknown | var %dstr = $tc.addComma(%str) }
          if (%def == 0) { var %ddef = $tc.col(7) $+ N/A } | else { inc %bknown | var %ddef = $tc.addComma(%def) }
          if (%dex == 0) { var %ddex = $tc.col(7) $+ N/A } | else { inc %bknown | var %ddex = $tc.addComma(%dex) }
          if (%man == 0) { var %dman = $tc.col(7) $+ N/A } | else { var %dman = $tc.addComma(%man) }
          if (%int == 0) { var %dint = $tc.col(7) $+ N/A } | else { var %dint = $tc.addComma(%int) }
          if (%end == 0) { var %dend = $tc.col(7) $+ N/A } | else { var %dend = $tc.addComma(%end) }
          if (%lvl == 0) { var %dlvl = N/A } | else { var %dlvl = %lvl }
          if (%cash == -1) { var %dcash = $tc.col(7) $+ N/A }
          else { 
            if (%cash > 100000000) { var %dcash = $tc.col(6) $+ $chr(36) $+ $tc.addComma(%cash) }
            else { var %dcash = $tc.col(2) $+ $chr(36) $+ $tc.addComma(%cash) }
          }                    
          var %msgline = $tc.col(1) $+ Spy entry for: $+ $tc.col(2) %name $tc.colid(%id,1,2) $tc.col(1) $+ (Lvl: $+ $tc.col(2) %dlvl $tc.col(1) $+ - Fact: $+ $tc.col(2) %fact $+ $tc.col(1) $+ ) - Added on $+ $tc.col(2) $asctime(%adate,mmm d) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%adate,h:nn:ss tt) $tc.col(8) $+ ( $+ $duration($calc($tc.time - %adate)) ago)
          if ($calc($tc.time - %adate) >= 2592000) { var %msgline = %msgline $+ $chr(32) $+ $tc.col(7) $+ [OLD] }
          msg # %msgline
          msg # $tc.col(1) $+ Speed: $+ $tc.col(2) %dspd $tc.col(1) $+ - ( $+ $tc.col(2) $+ $round($calc((%spd / %ctotb) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ )
          msg # $tc.col(1) $+ Strength: $+ $tc.col(2) %dstr $tc.col(1) $+ - ( $+ $tc.col(2) $+ $round($calc((%str / %ctotb) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ )
          msg # $tc.col(1) $+ Defense: $+ $tc.col(2) %ddef $tc.col(1) $+ - ( $+ $tc.col(2) $+ $round($calc((%def / %ctotb) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ )
          msg # $tc.col(1) $+ Dexterity: $+ $tc.col(2) %ddex $tc.col(1) $+ - ( $+ $tc.col(2) $+ $round($calc((%dex / %ctotb) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ )        
          var %msgline $tc.col(1) $+ Total:
          if (%totb == 0 && %bknown != 4) { var %msgline = %msgline $+ $tc.col(1) Showing $+ $tc.col(2) $tc.addComma(%ctotb) $tc.col(1) $+ - $tc.col(7) $+ Unknown number of remaining stats. }
          elseif (%totb != 0 && %totb != %ctotb && $calc(%totb - %ctotb) > 1) { var %msgline = %msgline $+ $tc.col(2) $tc.addComma(%ctotb) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%totb) $tc.col(1) $+ ( $+ $tc.col(2) $+ $round($calc((%ctotb / %totb) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ ) recorded stats accounted for, $tc.col(2) $+ $tc.addComma($calc(%totb - %ctotb)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $round($calc(((%totb - %ctotb) / %totb) * 100),2) $+ $chr(37) $+ $tc.col(1) $+ ) missing. }
          else { var %msgline = %msgline $+ $tc.col(2) $tc.addComma(%ctotb) $tc.col(1) $+ - $tc.col(6) $+ All stats accounted for. } 
          msg # %msgline
          ; msg # $tc.col(1) $+ Man: $+ $tc.col(2) %dman $tc.col(1) $+ - Int: $+ $tc.col(2) %dint $tc.col(1) $+ - End: $+ $tc.col(2) %dend $tc.col(1) $+ - Cash: %dcash
        }
        else { msg # $tc.col(5) $+ Requested spy not found. }
      }

      if ($1 == !delspy) {
        tc.chkaccess delspy $nick #   
        if ($2 == $null) { msg # $tc.col(3) $+ Deletes spy from bot. $tc.col(1) $+ Usage: $tc.col(2) $+ !delspy Name/TornID | halt }
        if ($exists($tc.spyfile)) { 
          var %eof = $lines($tc.spyfile) | var %ptr = 1
          while (%ptr <= %eof) {
            var %read = $read($tc.spyfile,%ptr)
            if ($2 isnum) { var %look = $gettok(%read,2,9) }
            else { var %look = $gettok(%read,1,9) }
            if (%look == $2) { 
              write -dl $+ %ptr $tc.spyfile 
              msg # $tc.col(1) $+ Deleted spy entry for $+ $tc.col(2) $gettok(%read,1,9) $tc.colid($gettok(%read,2,9),1,2) 
              halt 
            }
            inc %ptr
          }
        }
        msg # $tc.col(5) $+ Spy not found. 
      }

      if ($1 == !findspy || $1 == !spyfind) {
        if ($2 == $null) { msg # $tc.col(3) $+ Searches for spy entries within a faction. $tc.col(1) $+ Usage: $tc.col(2) $+ !findspy FactionName | halt }
        var %sptr = 0, %cptr = 1, %ptr = 1, %eof = $lines($tc.spyfile), %chanline = $null
        while (%ptr <= %eof) {
          var %read = $read($tc.spyfile,%ptr)
          if ($2- isin $gettok(%read,4,9)) { 
            if (%sptr == 0) { msg # $tc.col(1) $+ List of Spies found for: $+ $tc.col(2) $2- }
            var %chanline = %chanline $+ $tc.col(2) $+ $chr(32) $+ $gettok(%read,1,9) $tc.colid($gettok(%read,2,9),1,2) $+ $tc.col(1) $+ $chr(44) 
            inc %cptr | inc %sptr
            if (%cptr == 7) { msg # %chanline | var %chanline = $null, %cptr = 1 } 
          }      
          inc %ptr
        }
        if (%chanline != $null) { msg # $left(%chanline,$calc($len(%chanline) - 1)) $+ $chr(46) }
        if (%sptr == 0) { msg # $tc.col(5) $+ There are no spies stored with $+ $tc.col(2) $2- $tc.col(5) $+ in their faction name. }
        else { msg # $tc.col(1) $+ Total spies with $+ $tc.col(2) $2- $tc.col(1) $+ in faction name: $+ $tc.col(2) %sptr }
      }
    }

    ; --------------------------------- title info lookup command ---------------------------------

    if ($1 == !title) {
      if ($2 == $null) { 
        msg # $tc.col(3) $+ Displays brief title info. $tc.col(1) $+ Usage: $tc.col(2) $+ !title TitleName $tc.col(1) $+ - Info pulled from: $+ $tc.col(2) http://www.torn.com/forums.php#!p=threads&f=61&t=15910948&b=0&a=0 $tc.col(1) $+ - Thread credit: $tc.col(2) $+ philoten $tc.colid(1646213,1,2)
        msg # $tc.col(1) $+ Note: $tc.col(4) $+ Title's are given based on your highest "rank" for each category. For example, you could be ranked 10k across torn for successful runaways and ranked over 20k for everything else; you will get the "Coward" title regardless of how many you've actually done.
        halt 
      }      
      if ($2 == Antagonist || $2 == defend || $2 == defends) { var %name = Antagonist, %info = has a lot of successful defends. }
      if ($2 == Bondsman || $2 == bails || $2 == bail) { var %name = Bondsman, %info = has bailed a lot of people from jail. }
      if ($2 == Booster) { var %name = Booster, %info = has taken a large number of stat enhancers. }
      if ($2 == Buster || $2 == busts || $2 == bust) { var %name = Buster, %info = has busted many people out of jail. }
      if ($2 == Citizen) { var %name = Citizen, %info = has been active but is not a new player. }
      if ($2 == Coward || $2 == pussy) { var %name = Coward, %info = runs away from a fight often. }
      if ($2 == Damage) { var %name = Damage Dealer, %info = has done a large amount of total damage. }
      if ($2 == Deserter || $2 == inactive) { var %name = Deserter, %info = has been inactive for 30+ days. }
      if ($2 == Druggy || $2 == drugs) { var %name = Druggy, %info = has taken a lot of drugs. }
      if ($2 == Egotist || $2-3 == bounties placed || $2-3 == bounties added || $2-3 == bounties given) { var %name = Egotist, %info = has placed a lot of bounties. }
      if ($2 == Felon || $2-3 == jailed) { var %name = Felon, %info = has been jailed often. }
      if ($2 == Healer || $2 == revives) { var %name = Healer, %info = has revived many people. }
      if ($2 == Hitman || $2-3 == bounties collect || $2-3 == bounties collected) { var %name = Hitman, %info = has collected many bounties. }
      if ($2 == Hoarder || $2 == points) { var %name = Hoarder, %info = has bought many points from the market. }
      if ($2 == Importer) { var %name = Importer, %info = has bought a lot of items from abroad. }
      if ($2 == Intimidator || $2 == runaways) { var %name = Intimidator, %info = has many people running away from them in a fight. }
      if ($2 == Investor || $2 == bank) { var %name = Investor, %info = has invested a lot of cash with the bank. }
      if ($2 == Jobsworth || $2 == director) { var %name = Jobsworth, %info = has been trained often by a director. }
      if ($2 == Killer || $2 == attack || $2 == attacks) { var %name = Killer, %info = has successfully done a lot of attacks. }
      if ($2 == Loser || $2 == lose || $2 == lost) { var %name = Loser, %info = has lost a lot of attacks. }
      if ($2 == Marksman || $2 == crit || $2 == critical) { var %name = Marksman, %info = has done a lot of critical hits. }
      if ($2 == Merchant || $2 == buy || $2 == items) { var %name = Merchant, %info = has bought a lot of items from the market. }
      if ($2 == Newcomer || $2 == new || $2 == newbie) { var %name = Newcomer, %info = is under 7 days old, or under 30 days old without playing. }
      if ($2 == One) { var %name = One Hit Killer, %info = with a lot of one hit kills. }
      if ($2 == Outcast || $2-3 == bounties received) { var %name = Outcast, %info = has received a lot of bounties. }
      if ($2 == Punchbag || $2-3 == defends lost) { var %name = Punchbag, %info = has lost a lot of defends. }
      if ($2 == Samaritan || $2 == giver) { var %name = Samaritan, %info = has given many items away. }
      if ($2 == Sage || $2 == education || $2 == edu) { var %name = Sage, %info = has completed many education courses. }
      if ($2 == Scavenger || $2 == dump) { var %name = Scavenger, %info = has searched the dump many times. }
      if ($2 == Silent || $2 == stealth || $2 == stealths || $2 == fart) { var %name = Silent Killer, %info = often stealths their attacks. }
      if ($2 == Soldier || $2 == respect) { var %name = Soldier, %info = has earned a lot of respect. }
      if ($2 == Socialite || $2 == mails) { var %name = Socialite, %info = has sent a lot of mails. }
      if ($2 == Tank || $2 == stalemate) { var %name = Tank, %info = has stalemated often. }
      if ($2 == Thief || $2 == Theif || $2 == mug || $2 == mugs) { var %name = Thief, %info = has mugged often. }
      if ($2 == Tourist || $2 == fly || $2 == flying) { var %name = Tourist, %info = has flown a lot. }
      if ($2 == Trader || $2 == trade || $2 == trades || $2 == trading) { var %name = Trader, %info = has traded often. }
      if ($2 == Tycoon || $2 == bazaar) { var %name = Tycoon, %info = had lots of bazaar customers. }
      if ($2 == Absolute) { var %name = Absolute Beginner, %rank = 1 }
      if ($2 == Beginner) { var %name = Beginner, %rank = 2 }
      if ($2 == Inexperienced) { var %name = Inexperienced, %rank = 3 }
      if ($2 == Rookie) { var %name = Rookie, %rank = 4 }
      if ($2 == Novice) { var %name = Novice, %rank = 5 }
      if ($2 == Below) { var %name = Below Average, %rank = 6 }
      if ($2 == Average) { var %name = Average, %rank = 7 }
      if ($2 == Reasonable) { var %name = Reasonable, %rank = 8 }
      if ($2 == Above) { var %name = Above Average, %rank = 9 }
      if ($2 == Competent || $2 == Competant) { var %name = Competent, %rank = 10 }
      if ($2 == Veteran) { var %name = Veteran, %rank = 12 }
      if ($2 == Distinguished) { var %name = Distinguished, %rank = 13 }
      if ($2 == Highly) {
        if ($3 == Competent || $3 == Competant) { var %name = Highly Competent, %rank = 11 }
        elseif ($3 == Distinguished) { var %name = Highly Distinguished, %rank = 14 }
        else { 
          msg # $tc.col(5) $+ Do you mean $tc.col(2) $+ Highly Competent $tc.col(5) $+ or $+ $tc.col(2) Highly Distinguished $+ $tc.col(5) $+ ? 
          var %stfu 1
        }
      }
      if ($2 == Professional || $2 == Pro) { var %name = Professional, %rank = 15 }
      if ($2 == Star) { var %name = Star, %rank = 16 }
      if ($2 == Master) { var %name = Master, %rank = 17 }
      if ($2 == Outstanding) { var %name = Outstanding, %rank = 18 }
      if ($2 == Celebrity || $2 == Celeb) { var %name = Celebrity, %rank = 19 }
      if ($2 == Supreme) { var %name = Supreme, %rank = 20 }
      if ($2 == Idolised || $2 == Idol) { var %name = Idolised, %rank = 21 }
      if ($2 == Champion || $2 == Champ) { var %name = Champion, %rank = 22 }
      if ($2 == Heroic || $2 == Hero) { var %name = Heroic, %rank = 23 }
      if ($2 == Legendary || $2 == Legend) { var %name = Legendary, %rank = 24 }
      if ($2 == Elite) { var %name = Elite, %rank = 25 }
      if ($2 == Invincible) { var %name = Invincible, %rank = 26 }

      if (%name != $null) {
        if (%rank) {
          if (%rank == 1) { 
            msg # $tc.col(1) $+ The rank $+ $tc.col(2) %name $tc.col(1) $+ is the $+ $tc.col(2) $ord(%rank) $tc.col(1) $+ and initial rank.
          }
          elseif (%rank == 26) { 
            msg # $tc.col(1) $+ The rank $+ $tc.col(2) %name $tc.col(1) $+ is the $+ $tc.col(2) $ord(%rank) $tc.col(1) $+ and final rank.
          }
          else {
            msg # $tc.col(1) $+ The rank $+ $tc.col(2) %name $tc.col(1) $+ is the $+ $tc.col(2) $ord(%rank) $tc.col(1) $+ rank.
          }
          msg # $tc.col(8) $+ You gain ranks by gaining levels, doing crimes, raising battle stats, and obtaining a higher networth. Complete list: http://www.torn.com/forums.php#!p=threads&f=2&t=15958269&b=0&a=0
        } 
        else {
          if (%info != $null) { msg # $tc.col(1) $+ The title $+ $tc.col(2) %name $tc.col(1) $+ is awarded to a player that $+ $tc.col(2) %info }
          else { msg # $tc.col(5) $+ I don't know how to get the title of $+ $tc.col(2) %name $tc.col(5) $+ at the moment, sorry. }
        }
      }
      else { if (!%stfu) { msg # $tc.col(5) $+ Sorry, I have no idea what $+ $tc.col(2) $2- $tc.col(5) $+ is. } }
    }

    ; --------------------------------- stock info lookup command ---------------------------------

    if ($1 == !stock) {
      if ($2 == $null) { msg # $tc.col(3) $+ Displays brief stock info. $tc.col(1) $+ Usage: $tc.col(2) $+ !stock Acronym $tc.col(1) $+ - Stock index: $+ $tc.col(2) http://www.torn.com/stockexchange.php | halt }
      if ($2 == tcse) { 
        var %name = Torn City Stock Exchange, %sym = TCSE, %link = 0        
      }
      if ($2 == tcsb) { 
        var %name = Torn City and Shanghi Banking Corporation, %sym = TCSB, %link = 1
        var %perf = The performance of this stock relies upon the amount of money in company banks.
        var %bene = You are entitled to receive occasional dividends when holding at least 4,000,000 shares.
      }
      if ($2 == tcb) { 
        var %name = Torn City Investment Banking, %sym = TCB, %link = 2
        var %perf = The performance of this stock relies upon the total money invested in the bank.
        var %bene = You are entitled to receive improved interest rates when holding at least 1,500,000 shares.
      }
      if ($2 == sys) { 
        var %name = Syscore MFG, %sym = SYS, %link = 3
        var %bene = You are entitled to receive supreme firewall software for you and your company when holding at least 3,000,000 shares.
      }
      if ($2 == slag) { 
        var %name = Society and Legal Authorities Group, %sym = SLAG, %link = 4
        var %perf = The performance of this stock relies upon the amount of people in jail.
        var %bene = You are entitled to receive business cards from our lawyers when holding at least 1,500,000 shares.
      }
      if ($2 == iou) { 
        var %name = Insured On Us, %sym = IOU, %link = 5
        var %bene = You are entitled to receive occasional dividends when holding at least 3,000,000 shares.
      }
      if ($2 == grn) { 
        var %name = Grain, %sym = GRN, %link = 6
        var %bene = You are entitled to receive occasional dividends when holding at least 500,000 shares.
      }
      if ($2 == tchs) { 
        var %name = Torn City Health Service, %sym = TCHS, %link = 7
        var %perf = The performance of this stock relies upon the amount of people in hospital.
        var %bene = You are entitled to receive occasional medical packs when holding at least 150,000 shares.
      }
      if ($2 == yaz) { 
        var %name = Yazoo, %sym = YAZ, %link = 8
        var %bene = You are entitled to receive free banner advertisement in the local newspaper when holding at least 1,000,000 shares.
      }
      if ($2 == tct) { 
        var %name = The Torn City Times, %sym = TCT, %link = 9
        var %perf = The performance of this stock relies upon the amount of classified advertisements.
        var %bene = You are entitled to receive free personal placements in the newspaper when holding at least 125,000 shares.
      }
      if ($2 == cnc) { 
        var %name = Crude & Co., %sym = CNC, %link = 10
        var %bene = You are entitled to receive discounted oil rig costs and an oil rig company profit boost when holding at least 5,000,000 shares.
      }
      if ($2 == msg) { 
        var %name = Messaging Inc., %sym = MSG, %link = 11
        var %bene = You are entitled to receive free advertisement placements in the newspaper when holding at least 300,000 shares.
      }
      if ($2 == tmi) { 
        var %name = TC Music Industries, %sym = TMI, %link = 12
      }
      if ($2 == tcp) { 
        var %name = TC Media Productions, %sym = TCP, %link = 13
        var %perf = The performance of this stock relies upon the amount of company advertising campaigns.
        var %bene = You are entitled to receive support for your company (if you are the owner) which should result in a 10% bonus to profits when holding at least 1,000,000 shares.
      }
      if ($2 == iil) { 
        var %name = I Industries Ltd., %sym = IIL, %link = 14
        var %perf = The performance of this stock relies upon amount of computers purchased.
        var %bene = You are entitled to receive software to improve coding time by 50% when holding at least 100,000 shares.
      }
      if ($2 == fhc || $2 == fhg) { 
        var %name = Feathery Hotel Group, %sym = FHG, %link = 15
        var %perf = The performance of this stock relies upon the amount of refills people have made.
        var %bene = You are entitled to receive occasional coupons to stay in our hotels when holding at least 2,000,000 shares.
      }
      if ($2 == sym) { 
        var %name = Symbiotic Ltd., %sym = SYM, %link = 16
        var %bene = You are entitled to receive occasional drug packs when holding at least 500,000 shares.
      }
      if ($2 == lsc) { 
        var %name = Lucky Shots Casino, %sym = LSC, %link = 17
        var %perf = The performance of this stock relies upon the amount the casino gains from people losing.
        var %bene = You are entitled to receive occasional packs of 100x lottery tickets when holding at least 100,000 shares.
      }
      if ($2 == prn) { 
        var %name = Performance Ribaldry Network, %sym = PRN, %link = 18
        var %bene = You are entitled to receive occasional erotic DVDs when holding at least 1,500,000 shares.
      }
      if ($2 == ewm) { 
        var %name = Eaglewood Mercenary, %sym = EWM, %link = 19
        var %bene = You are entitled to receive occasional grenade packs when holding at least 2,000,000 shares.
      }
      if ($2 == tcm) { 
        var %name = Torn City Motors, %sym = TCM, %link = 20
        var %bene = You are entitled to receive a 25% discount when buying car parts when holding at least 1,000,000 shares.
      }
      if ($2 == elbt) { 
        var %name = The Empty Lunchbox Building Traders, %sym = ELBT, %link = 21
        var %perf = The performance of this stock relies upon the amount of money spent on properties by people.
        var %bene = You are entitled to receive a 10% discount on all home upgrades (not including staff) when holding at least 5,000,000 shares.
      }
      if ($2 == hrg) { 
        var %name = Home Retail Group, %sym = HRG, %link = 22
        var %perf = The performance of this stock relies upon the amount of money spent on properties by people.
        var %bene = You are entitled to receive occasional free properties when holding at least 1,500,000 shares.
      }
      if ($2 == tgp) { 
        var %name = Tell Group Plc., %sym = TGP, %link = 23
        var %perf = The performance of this stock relies upon the amount of company advertising campaigns.
        var %bene = You are entitled to receive a significant boost in company advertising (if you are the director) when holding at least 2,500,000 shares.
      }
      if ($2 == wssb) { 
        var %name = West Side South Bank University, %sym = WSSB, %link = 25
        var %perf = The performance of this stock relies upon the amount of people currently in education courses.
        var %bene = You are entitled to receive a 10% time reduction for all newly started courses when holding at least 1,000,000 shares.
      }
      if ($2 == istc) { 
        var %name = International School TC, %sym = ISTC, %link = 26
        var %perf = The performance of this stock relies upon the amount of people currently in education courses.
        var %bene = You are entitled to receive free education when holding at least 100,000 shares.
      }
      if ($2 == bag) { 
        var %name = Big Al's Gun Shop, %sym = BAG, %link = 27
        var %perf = The performance of this stock relies upon the amount of money spent in Big Al's shop.
        var %bene = You are entitled to receive occasional special ammunition packs when holding at least 3,000,000 shares.
      }
      if ($2 == evl) { 
        var %name = Evil Ducks Candy Corp, %sym = EVL, %link = 28
        var %bene = You are entitled to receive occasional happy boosters when holding at least 1,750,000 shares.
      }
      if ($2 == mcs) { 
        var %name = Mc Smoogle Corp, %sym = MCS, %link = 29
        var %bene = You are entitled to receive occasional free meals when holding at least 1,750,000 shares. (300 energy)
      }
      if ($2 == wlt) { 
        var %name = Wind Lines Travel, %sym = WLT, %link = 30
        var %perf = The performance of this stock relies upon the amount of people travelling in and out of Torn.
        var %bene = You are entitled to receive access to our free private jet when holding at least 9,000,000 shares.
      }
      if ($2 == tcc) { 
        var %name = Torn City Clothing, %sym = TCC, %link = 31
        var %bene = You are entitled to receive occasional dividends when holding at least 350,000 shares.
      }
      if (%name != $null) {
        if (%perf != $null) { var %stockline = $tc.col(2) $+ %name $tc.col(8) $+ ( $+ %sym $+ ) $+ $tc.col(1) %perf }
        else { var %stockline = $tc.col(2) $+ %name $tc.col(8) $+ ( $+ %sym $+ ) }
        if (%bene == $null) { var %stockline = %stockline $tc.col(5) $+ This stock does not provide a benefit. }
        else { var %stockline = %stockline $tc.col(6) $+ %bene }
        msg # %stockline
      }    
      else { msg # $tc.col(5) $+ Don't know what stock $+ $tc.col(2) $2 $tc.col(5) $+ is. }
    }

    ; --------------------------------- gym info lookup command ---------------------------------

    if ($1 == !gym) {
      if ($2 == $null) { msg # $tc.col(3) $+ Displays brief gym info. $tc.col(1) $+ Usage: $tc.col(2) $+ !gym GymName $tc.col(1) $+ - Gym info pulled from: $+ $tc.col(2) https://www.torn.com/wiki/Gym | halt }
      if ($2 == Premier) { 
        var %gymname = Premier Fitness, %gymclass = Light
        var %gymstr = 2.0, %gymspd = 2.0, %gymdef = 2.0, %gymdex = 2.0, %gymept = 5, %gymcost = 10
        var %gymnote = First Light Gym, all new players start here., %gymnext = Average Joes, %avgnexte = 200
      }
      if ($2 == Average) { 
        var %gymname = Average Joes, %gymclass = Light
        var %gymstr = 2.4, %gymspd = 2.4, %gymdef = 2.8, %gymdex = 2.4, %gymept = 5, %gymcost = 100
        var %gymnext = Woody's Workout, %avgnexte = 500
      }
      if ($2 == Woody's || $2 == Woodys) { 
        var %gymname = Woody's Workout, %gymclass = Light
        var %gymstr = 2.8, %gymspd = 3.2, %gymdef = 3.0, %gymdex = 2.8, %gymept = 5, %gymcost = 250
        var %gymnext = Beach Bods, %avgnexte = 1000
      }
      if ($2 == Beach) { 
        var %gymname = Beach Bods, %gymclass = Light
        var %gymstr = 3.2, %gymspd = 3.2, %gymdef = 3.2, %gymdex = 0, %gymept = 5, %gymcost = 500
        var %gymnext = Silver Gym, %avgnexte = 2000
      }
      if ($2 == Silver) { 
        var %gymname = Silver Gym, %gymclass = Light
        var %gymstr = 3.4, %gymspd = 3.6, %gymdef = 3.4, %gymdex = 3.2, %gymept = 5, %gymcost = 1000
        var %gymnext = Pour Femme, %avgnexte = 2750
      }
      if ($2 == Pour) { 
        var %gymname = Pour Femme, %gymclass = Light
        var %gymstr = 3.4, %gymspd = 3.6, %gymdef = 3.6, %gymdex = 3.8, %gymept = 5, %gymcost = 2500
        var %gymnext = Davie's Den, %avgnexte = 3000
      }
      if ($2 == Davie's || $2 == Davies) { 
        var %gymname = Davie's Den, %gymclass = Light
        var %gymstr = 3.7, %gymspd = 0, %gymdef = 3.7, %gymdex = 3.7, %gymept = 5, %gymcost = 5000
        var %gymnext = Global Gym, %avgnexte = 3500
      }
      if ($2 == Global) { 
        var %gymname = Global Gym, %gymclass = Light
        var %gymstr = 4.0, %gymspd = 4.0, %gymdef = 4.0, %gymdex = 4.0, %gymept = 5, %gymcost = 10000
        var %gymnote = Final Light Gym, %gymnext = Knuckle Head, %avgnexte = 4000
      }
      if ($2 == Knuckle) { 
        var %gymname = Knuckle Head, %gymclass = Medium
        var %gymstr = 4.8, %gymspd = 4.4, %gymdef = 4.0, %gymdex = 4.2, %gymept = 10, %gymcost = 50000
        var %gymnote = First Medium Gym, %gymnext = Pioneer Fitness, %avgnexte = 6000
      }
      if ($2 == Pioneer) { 
        var %gymname = Pioneer Fitness, %gymclass = Medium
        var %gymstr = 4.4, %gymspd = 4.6, %gymdef = 4.8, %gymdex = 4.4, %gymept = 10, %gymcost = 100000
        var %gymnext = Anabolic Anomalies, %avgnexte = 7000
      }
      if ($2 == Anabolic) { 
        var %gymname = Anabolic Anomalies, %gymclass = Medium
        var %gymstr = 5.0, %gymspd = 4.6, %gymdef = 5.2, %gymdex = 4.6, %gymept = 10, %gymcost = 250000
        var %gymnext = Core, %avgnexte = 8000
      }
      if ($2 == Core) { 
        var %gymname = Core, %gymclass = Medium
        var %gymstr = 5.0, %gymspd = 5.2, %gymdef = 5.0, %gymdex = 5.0, %gymept = 10, %gymcost = 500000
        var %gymnext = Racing Fitness, %avgnexte = 11000
      }
      if ($2 == Racing) { 
        var %gymname = Racing Fitness, %gymclass = Medium
        var %gymstr = 5.0, %gymspd = 5.4, %gymdef = 4.8, %gymdex = 5.2, %gymept = 10, %gymcost = 1000000
        var %gymnext = Complete Cardio, %avgnexte = 12420
      }
      if ($2 == Complete) { 
        var %gymname = Complete Cardio, %gymclass = Medium
        var %gymstr = 5.5, %gymspd = 5.8, %gymdef = 5.5, %gymdex = 5.2, %gymept = 10, %gymcost = 2000000
        var %gymnext = Legs, Bums and Tums, %avgnexte = 18000
      }
      if ($2 == Legs) { 
        var %gymname = Legs, Bums and Tums, %gymclass = Medium
        var %gymstr = 0, %gymspd = 5.6, %gymdef = 5.6, %gymdex = 5.8, %gymept = 10, %gymcost = 3000000
        var %gymnext = Deep Burn, %avgnexte = 18100
      }
      if ($2 == Deep) { 
        var %gymname = Deep Burn, %gymclass = Medium
        var %gymstr = 6.0, %gymspd = 6.0, %gymdef = 6.0, %gymdex = 6.0, %gymept = 10, %gymcost = 5000000
        var %gymnote = Final Medium Gym, %gymnext = Apollo Gym, %avgnexte = 24140
      }
      if ($2 == Apollo) { 
        var %gymname = Apollo Gym, %gymclass = Heavy
        var %gymstr = 6.0, %gymspd = 6.2, %gymdef = 6.4, %gymdex = 6.2, %gymept = 10, %gymcost = 7500000
        var %gymnote = First Heavy Gym, %gymnext = Gun Shop, %avgnexte = 31200
      }
      if ($2 == Gun) { 
        var %gymname = Gun Shop, %gymclass = Heavy
        var %gymstr = 6.6, %gymspd = 6.4, %gymdef = 6.2, %gymdex = 6.2, %gymept = 10, %gymcost = 10000000
        var %gymnext = Force Training, %avgnexte = 36610
      }
      if ($2 == Force) { 
        var %gymname = Force Training, %gymclass = Heavy
        var %gymstr = 6.4, %gymspd = 6.6, %gymdef = 6.4, %gymdex = 6.8, %gymept = 10, %gymcost = 15000000
        var %gymnext = Cha Cha's, %avgnexte = 46640
      }
      if ($2 == Cha) { 
        var %gymname = Cha Cha's, %gymclass = Heavy
        var %gymstr = 6.4, %gymspd = 6.4, %gymdef = 6.8, %gymdex = 7.0, %gymept = 10, %gymcost = 20000000
        var %gymnote = Possibly unlocks Balboas Gym or Frontline Fitness based on your stat distribution.
        var %gymnext = Atlas, %avgnexte = 56520
      }
      if ($2 == Atlas) { 
        var %gymname = Atlas, %gymclass = Heavy
        var %gymstr = 7.0, %gymspd = 6.4, %gymdef = 6.4, %gymdex = 6.6, %gymept = 10, %gymcost = 30000000
        var %gymnext = Last Round, %avgnexte = 67775
      }
      if ($2 == Last) { 
        var %gymname = Last Round, %gymclass = Heavy
        var %gymstr = 6.8, %gymspd = 6.6, %gymdef = 7.0, %gymdex = 6.6, %gymept = 10, %gymcost = 50000000
        var %gymnext = The Edge, %avgnexte = 84535
      }      
      if ($2-3 == The Edge || $2 == Edge) { 
        var %gymname = The Edge, %gymclass = Heavy
        var %gymstr = 6.8, %gymspd = 7.0, %gymdef = 7.0, %gymdex = 6.8, %gymept = 10, %gymcost = 75000000
        var %gymnext = George's, %avgnexte = 106305
      }
      if ($2 == George's || $2 == Georges) { 
        var %gymname = George's, %gymclass = Heavy
        var %gymstr = 7.2, %gymspd = 7.2, %gymdef = 7.2, %gymdex = 7.2, %gymept = 10, %gymcost = 100000000
        var %gymnote = Final Heavy Gym. Possibly unlock one of four special gyms based on stat distribution. No further gyms unlocked by energy spent.
      }
      if ($2 == Balboas || $2 == Balboa's) { 
        var %gymname = Balboas Gym, %gymclass = Special
        var %gymstr = 0, %gymspd = 0, %gymdef = 7.5, %gymdex = 7.5, %gymept = 25, %gymcost = 50000000
        var %gymnote = Unlocks when Cha Cha's is unlocked and (Def + Dex) is 25% higher than (Str + Spd).
      }
      if ($2 == Frontline) { 
        var %gymname = Frontline Fitness, %gymclass = Special
        var %gymstr = 7.5, %gymspd = 7.5, %gymdef = 0, %gymdex = 0, %gymept = 25, %gymcost = 50000000
        var %gymnote = Unlocks when Cha Cha's is unlocked and (Str + Spd) is 25% higher than (Def + Dex).
      }
      if ($2 == Gym || $2 == Gym3000) { 
        var %gymname = Gym 3000, %gymclass = Special
        var %gymstr = 8.0, %gymspd = 0, %gymdef = 0, %gymdex = 0, %gymept = 50, %gymcost = 100000000
        var %gymnote = Unlocks when George's is unlocked and Str is 25% higher than your second highest stat.
      }
      if ($2 == Mr || $2 == Mr. || $2 == Isoyama || $2 == Isoyamas) { 
        var %gymname = Mr. Isoyamas, %gymclass = Special
        var %gymstr = 0, %gymspd = 0, %gymdef = 8.0, %gymdex = 0, %gymept = 50, %gymcost = 100000000
        var %gymnote = Unlocks when George's is unlocked and Def is 25% higher than your second highest stat.
      }
      if ($2 == Total) { 
        var %gymname = Total Rebound, %gymclass = Special
        var %gymstr = 0, %gymspd = 8.0, %gymdef = 0, %gymdex = 0, %gymept = 50, %gymcost = 100000000
        var %gymnote = Unlocks when George's is unlocked and Spd is 25% higher than your second highest stat.
      }      
      if ($2 == Elites) { 
        var %gymname = Elites, %gymclass = Special
        var %gymstr = 0, %gymspd = 0, %gymdef = 0, %gymdex = 8.0, %gymept = 50, %gymcost = 100000000
        var %gymnote = Unlocks when George's is unlocked and Dex is 25% higher than your second highest stat.
      }
      if ($2-3 == The Sports || $2 == Sports) { 
        var %gymname = The Sports Science Lab, %gymclass = Special
        var %gymstr = 9.0, %gymspd = 9.0, %gymdef = 9.0, %gymdex = 9.0, %gymept = 25, %gymcost = 500000000
        var %gymnote = Drug free gym. Unlocks when Last Round is unlocked and player has taken little or no drugs. Will lose membership if player takes more than 50 Xanax or Ecstasy.
      }
      if ($2 == Crims || $2 == Crim's || $2 == Jail) { 
        var %gymname = Crims Gym, %gymclass = Jail
        var %gymstr = 3.4, %gymspd = 3.4, %gymdef = 3.8, %gymdex = 0, %gymept = 5, %gymcost = 0
        var %gymnote = Gym used when in jail.
      }
      if (%gymname != $null) {
        if (%gymstr == 0) { var %gymstr = $tc.col(5) $+ None } | if (%gymspd == 0) { var %gymspd = $tc.col(5) $+ None }
        if (%gymdex == 0) { var %gymdex = $tc.col(5) $+ None } | if (%gymdef == 0) { var %gymdef = $tc.col(5) $+ None }
        msg # $tc.col(2) $+ %gymname $tc.col(1) $+ ( $+ $tc.col(2) $+ %gymclass $+ $tc.col(1) $+ ) - Cost: $+ $tc.col(2) $ $+ $tc.addComma(%gymcost) $tc.col(1) $+ - Str: $+ $tc.col(2) %gymstr $tc.col(1) $+ - Spd: $+ $tc.col(2) %gymspd $tc.col(1) $+ - Def: $+ $tc.col(2) %gymdef $tc.col(1) $+ - Dex: $+ $tc.col(2) %gymdex $tc.col(1) $+ - EPT: $+ $tc.col(2) %gymept 
        if (%gymnote != $null) { msg # $tc.col(4) $+ %gymnote }
        if (%gymnext != $null) { 
          var %nextmsg = $tc.col(1) $+ Next gym unlocked: $+ $tc.col(2) %gymnext 
          if (%avgnexte != $null) { var %nextmsg = %nextmsg $tc.col(1) $+ - Estimate Energy to Next Gym: $+ $tc.col(2) $tc.addComma(%avgnexte) }
          msg # %nextmsg
        }
      }
      else { msg # $tc.col(5) $+ I have no idea what " $+ $tc.col(2) $+ $2- $+ $tc.col(5) $+ " is. }
    }

    ; --------------------------------- company info lookup command ---------------------------------

    if ($1 == !job || $1 == !company) {
      if ($2 == $null) { 
        msg # $tc.col(3) $+ Lists company specials. $tc.col(1) $+ Usage: $tc.col(2) $+ !job CompanyName 
        msg # $tc.col(8) $+ Huge thanks to $+ $tc.col(4) Rayne $tc.colid(1868280,8,4) for helping me enter all the data. 
      }
      else {
        if ($2 == Hair || $2 == 1) { 
          var %comp = Hair Salon, %compid = 1
          var %1n = Debate, %1d = +Experience, %1c = (1 pt) 
          var %3n = Gossip, %3d = View someone's money on hand, %3c = (10 pts) 
          var %5n = Rumors, %5d = 50% reduced enemy stealth, %5c = (Passive) 
          var %7n = Cutting corners, %7d = 30 minute education time reduction, %7c = (1 pt) 
          var %10n = Sweeney's Revenge, %10d = 20% slashing weapon damage, %10c = (Passive) 
        }
        if ($2 == Law || $2 == 2) { 
          var %comp = Law Firm, %compid = 2
          var %1n = Bail Bondsman, %1d = 50% decreased bail costs, %1c = (Passive) 
          var %3n = Background Check, %3d = View someone's stats, %3c = (10 pts) 
          var %5n = Closing Argument, %5d = Easier to bust more people at once, %5c = (Passive) 
          var %7n = Loophole, %7d = 20% organised crime skill, %7c = (Passive)
          var %10n = Educated Decisions, %10d = View success chance of potential busts, %10c = (Passive)
        }
        if ($2 == Flower || $2 == 3) { 
          var %comp = Flower Shop, %compid = 3
          var %1n = Rare Import, %1d = Special flower, %1c = (3 pts) 
          var %3n = Cultivation, %3d = 25% illegal production success & skill gain, %3c = (Passive) 
          var %5n = Herbal Cleansing, %5d = Drug addiction reduction, %5c = (1 pt) 
          var %7n = Over Capacity, %7d = Buy 5 additional special flowers abroad, %7c = (Passive)
          var %10n = Floral Contacts, %10d = View stock analysis of flowers in all countries, %10c = (10 pts)
        }
        if ($2 == Car || $2 == 4) { 
          var %comp = Car Dealership, %compid = 4
          var %1n = Test Drive, %1d = Racing point, %1c = (5 pts) 
          var %3n = Discount parts, %3d = 75% cost discount on car parts, %3c = (Passive) 
          var %5n = Salesman, %5d = No item market fees, %5c = (Passive) 
          var %7n = Two-Faced, %7d = 25% fraud success & skill gain, %7c = (Passive) 
          var %10n = Getaway Car, %10d = 95% increased run away chance, %10c = (Passive) 
        }
        if ($2 == Cloth || $2 == Clothing || $2 == 5) {
          var %comp = Clothing Store, %compid = 5
          var %1n = Fashion Show, %1d = +Experience, %1c = (1 pt) 
          var %3n = Nine to Five, %3d = 100 Endurance, %3c = (10 pts) 
          var %5n = Activewear, %5d = 25% passive dexterity, %5c = (Passive)          
          var %7n = Secret pockets, %7d = 75% enemy mug losses, %7c = (Passive) 
          var %10n = Tailoring, %10d = 20% armor bonus, %10c = (Passive) 
        }
        if ($2 == Gun || $2 == 6) {
          var %comp = Gun Shop, %compid = 6
          var %1n = Sales Discount, %1d = 20% Discount on Standard Ammo Cost, %1c = (Passive)
          var %3n = Surplus, %3d = 100 Random Special Ammunition, %3c = (25 pts) 
          var %5n = Skilled Analysis, %5d = Target Equipment and Ammo is always visible, %5c = (Passive)
          var %7n = Bandoleer, %7d = 1 extra clip for guns during combat, %7c = (Passive) 
          var %10n = Firearms Expert, %10d = 10% Primary & Secondary weapon damage, %10c = (Passive) 
        }
        if ($2 == Game || $2 == 7) {
          var %comp = Game Shop, %compid = 7
          var %1n = Ub3rg33k, %1d = 50% virus coding time reduction, %1c = (Passive) 
          var %3n = Early Release, %3d = Money, %3c = (100 pts) 
          var %5n = Gamer, %5d = 100% console happiness, %5c = (Passive)
          var %7n = Power Levelling, %7d = View progress to your next level, %7c = (10 pts) 
          var %10n = Overpowered, %10d = 1 nerve 5 energy 50 happiness, %10c = (1 pt - max 100pt/day) 
        }
        if ($2 == Candle || $2 == 8) {
          var %comp = Candle Shop, %compid = 8
          var %1n = Warming Therapy, %1d = 50 happiness, %1c = (1 pt) 
          var %3n = Illumination, %3d = 50% awareness, %3c = (Passive) 
          var %5n = Calming Therapy, %5d = 2 nerve, %5c = (1 pt)
          var %7n = Reinvigorating Therapy, %7d = 5 energy, %7c = (1 pt - max 100pt/day) 
          var %10n = Meditation, %10d = View someone's true level, %10c = (250 pts)
        }
        if ($2 == Toy || $2 == 9) {
          var %comp = Toy Shop, %compid = 9
          var %1n = Memory Lane, %1d = 50 happiness, %1c = (1 pt) 
          var %3n = Jumble Sale, %3d = Special plushie, %3c = (3 pts) 
          var %5n = Gamer, %5d = 100% console happiness. %5c = (Passive)
          var %7n = Over Capacity, %7d = Able to bring back +5 plushies from abroad, %7c = (Passive) 
          var %10n = Toy Importer, %10d = View stock analysis of plushies in all countries, %10c = (10 pts) 
        }
        if ($2 == Adult || $2 == 10) {
          var %comp = Adult Novelties, %compid = 10
          var %1n = Blackmail, %1d = Money, %1c = (1 pt) 
          var %3n = Voyeur, %3d = Erotic DVD, %3c = (20 pts) 
          var %5n = Party Supplies, %5d = Pack of Trojans, %5c = (500 pts)
          var %7n = Bondage, %7d = 25% enemy speed reduction, %7c = (Passive) 
          var %10n = Indecent, %10d = 100% bonus to Erotic DVDs, %10c = (Passive)
        }
        if ($2 == Cyber || $2 == 11) {
          var %comp = Cyber Cafe, %compid = 11
          var %1n = Ub3rg33k, %1d = 50% virus coding time reduction, %1c = (Passive) 
          var %3n = Clone Data, %3d = Virus, %3c = (25 pts) 
          var %5n = Proxy Hacking, %5d = Cancel a target's virus programming, %5c = (25 pts)
          var %7n = IP Tracing, %7d = View lister of anonymous bounties, %7c = (25 pts) 
          var %10n = Financial Phishing, %10d = View details of someone's investment account, %10c = (25 pts)
        }
        if ($2 == Grocery || $2 == 12) {
          var %comp = Grocery Store, %compid = 12
          var %1n = Bagged Down, %1d = Bag of candy, %1c = (2 pts) 
          var %3n = Fast Metabolism, %3d = 10% consumable cooldown reduction, %3c = (Passive)
          var %5n = Bottled Up, %5d = Bottle of alcohol, %5c = (5 pts)
          var %7n = Absorption, %7d = 10% consumable boost, %7c = (Passive) 
          var %10n = Canned in, %10d = Can of energy drink, %10c = (12 pts)
        }
        if ($2 == Theater || $2 == Theatre || $2 == 13) {
          var %comp = Theater, %compid = 13
          var %1n = Stagecraft, %1d = Experience, %1c = (1 pt) 
          var %3n = Dramatics, %3d = Guaranteed stealth, %3c = (20 pts) 
          var %5n = Masked, %5d = Cannot be targeted by spies, %5c = (Passive)
          var %7n = Twinlike, %7d = 25% forgery success rate and skill gain, %7c = (Passive) 
          var %10n = Disguised, %10d = Hidden travelling status & destination, %10c = (Passive) 
        }
        if ($2 == Sweet || $2 == 14) {
          var %comp = Sweet Shop, %compid = 14
          var %1n = Sweet Tooth, %1d = 50 happiness, %1c = (1 pt) 
          var %3n = Sugar rush, %3d = Bag of candy, %3c = (2 pts) 
          var %5n = Gluttony, %5d = 1000 happiness, %5c = (10 pts)
          var %7n = Energy Rush, %7d = Can of energy drink, %7c = (15 pts)
          var %10n = Voracious, %10d = 4500 happiness, %10c = (30 pts)
        }
        if ($2 == Cruise || $2 == 15) {
          var %comp = Cruise Line, %compid = 15
          var %1n = Bursar, %1d = 25 casino tokens, %1c = (1 pt) 
          var %3n = Portage, %3d = 2 extra travel items, %3c = (Passive)
          var %5n = R&R, %5d = Drug addiction reduction, %5c = (1 pt)
          var %7n = Destination Report, %7d = View stock analysis of all items at a selected country, %7c = (10 pts) 
          var %10n = Freight, %10d = 3 extra travel items, %10c = (Passive)
        }
        if ($2 == Television || $2 == TV || $2 == 16) {
          var %comp = Television Network, %compid = 16
          var %1n = Propaganda, %1d = 1 faction respect, %1c = (5 pts) 
          var %3n = Scoop, %3d = 50% newspaper advertising cost reduction, %3c = (Passive) 
          var %5n = Inside Story, %5d = View someone's stats & money, %5c = (15 pts)
          var %7n = Bad Publicity, %7d = 25% extortion success rate and skill gain, %7c = (Passive) 
          var %10n = Press Pass, %10d = Receive special privileges, %10c = (25 pts) 
        }
        if ($2 == 17) { msg # $tc.col(5) $+ Oddly, there is no company with the ID 17 in game. | halt }
        if ($2 == Zoo || $2 == 18) {
          var %comp = Zoo, %compid = 18
          var %1n = Fulfillment, %1d = 50 happiness, %1c = (1 pt) 
          var %3n = Animal Instinct, %3d = 25% hunting reward, %3c = (Passive) 
          var %5n = Special K, %5d = Ketamine drug, %5c = (5 pts)
          var %7n = Eye of the Tiger, %7d = 70% awareness, %7c = (Passive) 
          var %10n = Seasoned Poacher, %10d = 25% accuracy, %10c = (Passive)
        }
        if ($2 == Firework || $2 === 19) {
          var %comp = Firework Stand, %compid = 19
          var %1n = Audaciousness, %1d = 1 nerve, %1c = (1 pt) 
          var %3n = Illumination, %3d = 50% awareness, %3c = (Passive) 
          var %5n = Pyromania, %5d = 25% flame-thrower damage & accuracy, %5c = (Passive)
          var %7n = Explosives Expert, %7d = Random bomb parts, %7c = (5 pts) 
          var %10n = Inferno, %10d = 50 random incendiary ammunition, %10c = (25 pts)
        }
        if ($2 == Property || $2 == 20) { 
          var %comp = Property Broker, %compid = 20
          var %1n = Commission, %1d = Money, %1c = (1 pt) 
          var %3n = Job Satisfaction, %3d = 50 happiness, %3c = (1 pt) 
          var %5n = Vendor, %5d = No item market fees, %5c = (Passive)
          var %7n = Insider Trading, %7d = Random property, %7c = (150 pts) 
          var %10n = Interior Connections, %10d = 10% property upgrade cost reduction, %10c = (Passive)
        }
        if ($2 == Furniture || $2 == Furnature || $2 == 21) {
          var %comp = Furniture Store, %compid = 21
          var %1n = Coffee Break, %1d = 3 energy, %1c = (1 pt - max 100pt/day) 
          var %3n = Heavy Lifting, %3d = Strength, %3c = (1 pt) 
          var %5n = Removal, %5d = 25% theft success rate and skill gain, %5c = (Passive)
          var %7n = Beefcake, %7d = 25% passive strength, %7c = (Passive)
          var %10n = Brute Force, %10d = 100% fist/kick damage, %10c = (Passive) 
        }
        if ($2 == Gas || $2 == 22) {
          var %comp = Gas Station, %compid == 22
          var %1n = Machinist, %1d = Racing point, %1c = (5 pts) 
          var %3n = Discount Parts, %3d = 75% cost discount on car parts, %3c = (Passive)
          var %5n = Arsonist, %5d = 25% vandalism success rate and skill gain, %5c = (Passive)
          var %7n = Molotov cocktail, %7d = Molotov cocktail, %7c = (5 pts) 
          var %10n = Blaze of Glory, %10d = 50% Molotov cocktail damage & duration, %10c = (Passive)
        }
        if ($2 == Music || $2 == 23) {
          var %comp = Music Store, %compid = 23
          var %1n = Ambience, %1d = 50 happiness, %1c = (1 pt) 
          var %3n = Bootlegger, %3d = 25% prohibited trade success & skill gain, %3c = (Passive) 
          var %5n = High-fidelity, %5d = 75% enemy stealth reduction, %5c = (Passive)
          var %7n = Deafened, %7d = Guaranteed stealth, %7c = (15 pts) 
          var %10n = The Score, %10d = 10% passive all stats, %10c = (Passive)
        }
        if ($2 == Nightclub || $2 == 24) {
          var %comp = Nightclub, %compid = 24
          var %1n = Criminal Connections, %1d = +Experience, %1c = (1 pt) 
          var %3n = Target Market, %3d = 25% illicit services success & skill gain, %3c = (Passive) 
          var %5n = Suppression, %5d = Drug addiction, %5c = (1 pt)
          var %7n = Tolerance, %7d = 50% drug overdose reduction, %7c = (Passive) 
          var %10n = Restraint, %10d = Education is unaffected by drug addiction, %10c = (Passive)
        }
        if ($2 == Pub || $2 == 25) {
          var %comp = Pub, %compid = 25
          var %1n = Pub Lunch, %1d = 3 energy, %1c = (1 pt - max 100pt/day) 
          var %3n = Drunken Master, %3d = 10% melee weapon damage, %3c = (Passive) 
          var %5n = Liquid Courage, %5d = Refill nerve bar, %5c = (25 pts)
          var %7n = Lightweight, %7d = 50% bottle of alcohol boost, %7c = (Passive) 
          var %10n = Buzzed, %10d = 15 maximum nerve, %10c = (Passive) 
        }
        if ($2 == Gents || $2 == Gentleman || $2 == Gentlemans || $2 == 26) {
          var %comp = Gents Strip Club, %compid = 26
          var %1n = Happy Hour, %1d = Money, %1c = (1 pt) 
          var %3n = Free Drinks, %3d = 1 nerve, %3c = (1 pt) 
          var %5n = High Heels, %5d = Pair of high heels, %5c = (500 pts)
          var %7n = Dancer's flair, %7d = 25% passive dexterity, %7c = (Passive) 
          var %10n = Aspiring Workout, %10d = 10% dexterity gym gains, %10c = (Passive) 
        }
        if ($2 == Restaurant || $2 == 27) {
          var %comp = Restaurant, %compid = 27
          var %1n = Free Meals, %1d = 3 energy, %1c = (1 pt - max 100pt/day) 
          var %3n = Butcher, %3d = 10% melee weapon damage, %3c = (Passive) 
          var %5n = Flambayed, %5d = Flame thrower, %5c = (50 pts)
          var %7n = Healthy Diet, %7d = 2% life regeneration per tick, %7c = (Passive) 
          var %10n = Professional Metabolism, %10d = 25% food & drink cooldown reduction, %10c = (Passive)
        }
        if ($2 == Oil || $2 == Rig || $2 == 28) {
          var %comp = Oil Rig, %compid = 28
          var %1n = Danger Money, %1d = +Money, %1c = (1 pt) 
          var %3n = Embargo, %3d = Half a target's happiness, %3c = (50 pts) 
          var %5n = Oil Mogul, %5d = -1 hours bank investment time, %5c = (3 pts)
          var %7n = Tax Haven, %7d = 10% increase of Cayman Islands interest rate, %7c = (Passive) 
          var %10n = Fat Cat, %10d = 50% investment banking limit, %10c = (Passive)
        }
        if ($2 == Fitness || $2 == 29) {
          var %comp = Fitness Center, %compid = 29
          var %1n = Healthy Mind, %1d = -30 minutes education time, %1c = (1 pt) 
          var %3n = Goal Oriented, %3d = 50% happiness loss in gym, %3c = (Passive)     
          var %5n = Roid Rage, %5d = + Strength, %5c = (1 pt)
          var %7n = Athlete, %7d = 3% life regeneration per tick, %7c = (Passive) 
          var %10n = Training Regime, %10d = 3% gym gains, %10c = (Passive)
        }
        if ($2 == Mechanic || $2 == 30) {
          var %comp = Mechanic Shop, %compid = 30
          var %1n = Machinist, %1d = Racing point, %1c = (5 pts) 
          var %3n = Discount Parts, %3d = 75% cost discount on car parts, %3c = (Passive) 
          var %5n = Junkyard Dog, %5d = Random car, %5c = (5 pts)
          var %7n = Refurbish, %7d = Lose no car parts after crashing, %7c = (Passive) 
          var %10n = Driver, %10d = 50% driving skill gain, %10c = (Passive)
        }
        if ($2 == Amusement || $2 == 31) {
          var %comp = Amusement Park, %compid = 31
          var %1n = Dauntless, %1d = +1 nerve, %1c = (1 pt) 
          var %3n = Free Ride, %3d = 250 happiness for target, %3c = (10 pts) 
          var %5n = Unflinching, %5d = 10 maximum nerve, %5c = (Passive)
          var %7n = Adrenaline Rush, %7d = 25% epinephrine effect & duration, %7c = (Passive) 
          var %10n = Thrill Seeker, %10d = 10% crime skill and experience gain, %10c = (Passive)
        }
        if ($2 == Lingerie || $2 == 32) {
          var %comp = Lingerie Store, %compid = 32
          var %1n = Lingerie Party, %1d = +Experience, %1c = (1 pt) 
          var %3n = Nine to five, %3d = 100 endurance, %3c = (10 pts) 
          var %5n = Concealment, %5d = 2 extra travel items, %5c = (Passive)
          var %7n = Born Free, %7d = 50% dexterity when not wearing armor, %7c = (Passive) 
          var %10n = Free as the Wind Blows, %10d = 50% speed when not wearing any armor, %10c = (Passive)
        }
        if ($2 == Meat || $2 == 33) {
          var %comp = Meat Warehouse, %compid = 33
          var %1n = Blood Thirst, %1d = +1 nerve, %1c = (1 pt) 
          var %3n = Butcher, %3d = 10% melee weapon damage, %3c = (Passive) 
          var %5n = Carnage, %5d = 10 maximum nerve, %5c = (Passive)
          var %7n = Huntsman, %7d = 25% hunting skill gain, %7c = (Passive) 
          var %10n = Vampiric, %10d = 3% life regeneration per tick, %10c = (Passive)
        }
        if ($2 == Farm || $2 == 34) {
          var %comp = Farm, %compid = 34
          var %1n = Fullfillment, %1d = +50 happiness, %1c = (1 pt) 
          var %3n = Animal Instinct, %3d = 15% hunting reward, %3c = (Passive) 
          var %5n = Special K, %5d = Ketamine drug, %5c = (5 pts)
          var %7n = Fertilizer, %7d = Small explosive device, %7c = (100 pts) 
          var %10n = Early Riser, %10d = 7 energy, %10c = (1 pt - max 100pt/day)
        }
        if ($2 == Software || $2 == 35) {
          var %comp = Software Corporation, %compid = 35
          var %1n = Ub3rg33k, %1d = 50% virus coding time, %1c = (Passive) 
          var %3n = Proxy Hacking, %3d = Cancel a target's virus programming, %3c = (25 pts) 
          var %5n = Intricate Hack, %5d = Hack a company's bank account, %5c = (250 pts)
          var %7n = Hack the Planet, %7d = 25% computer crime success & skill gain, %7c = (Passive) 
          var %10n = Corporate Espionage, %10d = View financial details of a company, %10c = (50 pts)
        }
        if ($2 == Ladies || $2 == 36) {
          var %comp = Ladies Strip Club, %compid = 36
          var %1n = Happy Hour, %1d = Money, %1c = (1 pt) 
          var %3n = Free Drinks, %3d = 1 nerve, %3c = (1 pt) 
          var %5n = Thong, %5d = Thong, %5c = (300 pts)
          var %7n = Hench, %7d = 25% passive defense, %7c = (Passive) 
          var %10n = Aspiring Workout, %10d = 10% defense gym gains, %10c = (Passive)
        }
        if (%comp != $null) { 
          msg # $tc.col(2) $+ %comp $tc.col(8) $+ (ID: %compid $+ ) $tc.col(1) $+ - $tc.complink(%compid)
          msg # $tc.col(1) $+ 1 Star: $tc.col(3) $+ %1n $tc.col(1) $+ - $+ $tc.col(2) %1d $tc.col(4) $+ %1c 
          msg # $tc.col(1) $+ 3 Star: $tc.col(3) $+ %3n $tc.col(1) $+ - $+ $tc.col(2) %3d $tc.col(4) $+ %3c 
          msg # $tc.col(1) $+ 5 Star: $tc.col(3) $+ %5n $tc.col(1) $+ - $+ $tc.col(2) %5d $tc.col(4) $+ %5c 
          msg # $tc.col(1) $+ 7 Star: $tc.col(3) $+ %7n $tc.col(1) $+ - $+ $tc.col(2) %7d $tc.col(4) $+ %7c 
          msg # $tc.col(1) $+ 10 Star: $tc.col(3) $+ %10n $tc.col(1) $+ - $+ $tc.col(2) %10d $tc.col(4) $+ %10c 
        }
        else { msg # $tc.col(5) $+ I have no idea what " $+ $tc.col(2) $+ $2- $+ $tc.col(5) $+ " is. }
      }
    }

    ; --------------------------------- book lookup commands ---------------------------------    

    if ($1 = !book) {
      if ($2 == $null) { 
        msg # $tc.col(3) $+ Displays book information. $tc.col(1) $+ Usage: $tc.col(2) $+ !book Query $tc.col(8) $+ (only first word is searched) $tc.col(1) $+ - Info pulled from: $+ $tc.col(2) http://www.torn.com/forums.php?p=threads&f=61&t=15980189 $tc.col(1) $+ - Thread credit: $tc.col(2) $+ CloudJumper $tc.colid(1636201,1,2)
        msg # $tc.col(1) $+ Note: $tc.col(4) $+ Books are subscriber-only rewards (random chance every month), but they can also be found in the city or bought for mission credits. You can only ever read one book at a time and will only ever obtain one of each book.
      }
      else {
        if ($istok(Brawn Brains str strength,$2,32)) { var %title = Brawn Over Brains, %bene = Increases strength by 5% (Capped at 10m gain) upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Time Mind spd speed,$2,32)) { var %title = Time Is In The Mind, %bene = Increase speed by 5% (Capped at 10m gain) upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Keeping def defense,$2,32)) { var %title = Keeping Your Face Handsome, %bene = Increase defense by 5% (Capped at 10m gain) upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(A Job dex dexterity,$2,32)) { var %title = A Job For Your Hands, %bene = Increase dexterity by 5% (Capped at 10m gain) upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Fight Asshole passive,$2,32)) { var %title = Fight Like An Asshole, %bene = Provides a passive 25% bonus to all stats for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Mind str strength passive,$2,32)) { var %title = Mind Over Matter, %bene = Provides a passive 100% bonus to Strength for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(No Shame Pain def defense passive,$2,32)) { var %title = No Shame No Pain, %bene = Provides a passive 100% bonus to Defense for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Run Wind spd speed passive,$2,32)) { var %title = Run Like The Wind, %bene = Provides a passive 100% bonus to Speed for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Weaseling dex dexterity passive,$2,32)) { var %title = Weaseling Out Of Trouble, %bene = Provides a passive 100% bonus to Dexterity for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Get Hard gym gyms,$2,32)) { var %title = Get Hard or Go Home, %bene = Increases all gym gains by 20% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Gym Grunting strength str,$2,32)) { var %title = Gym Grunting - Shouting To Success, %bene = Increases Strength gym gains by 30% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Self defense def gym,$2,32)) { var %title = Self Defence In The Workplace, %bene = Increases Defense gym gains by 30% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Speed spd gym ,$2,32)) { var %title = Speed 3 - The Rejected Script, %bene = Increases Speed gym gains by 30% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Limbo dex dexterity gym ,$2,32)) { var %title = Limbo Lovers 101, %bene = Increases Dexterity gym gains by 30% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Working work,$2,32)) { var %title = Working 9 til 5, %bene = Increases all working stats by 5% (Capped at 2,500 gain each) upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Brown Brown-nosing Brownnosing employee effectiveness shit,$2,32)) { var %title = Brown-nosing The Boss, %bene = Maximum personal employee effectiveness for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(The Hamburglar Hamburglar's Hamburglars crime exp experience,$2,32)) { var %title = The Hamburglar's Guide To Crime, %bene = Increases crime skill & experience gain by 25% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(What Old exp experience level,$2,32)) { var %title = What Are Old Folk Good For Anyway?, %bene = Increases all EXP gain by 25% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Hot Turkey drug addiction,$2,32)) { var %title = Hot Turkey, %bene = Gain no drug addiction 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Ugly energy refill maximum max,$2,32)) { var %title = Ugly Energy, %bene = Increases maximum energy and energy refills to 250 for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Mailing travel times time,$2,32)) { var %title = Mailing Yourself Abroad, %bene = Decreases all travel times by 25% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Smuggling travel items,$2,32)) { var %title = Smuggling For Beginners, %bene = Increases travel items by 10 for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Higher Daddy energy regeneration regen,$2,32)) { var %title = Higher Daddy $+ $chr(44) Higher!, %bene = Provides +20% energy regeneration for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(The Real crimes crime nerve regeneration regen,$2,32)) { var %title = The Real Dutch Courage, %bene = Doubles nerve regeneration for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Because happy happiness regeneration regen,$2,32)) { var %title = Because I'm Happy - The Pharrell Story, %bene = Doubles happiness regeneration for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(No More Sick life regeneration regen,$2,32)) { var %title = No More Sick Days, %bene = Doubles life regeneration for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }        
        if ($istok(Stealthy stealth guarenteed,$2,32)) { var %title = Stealthy Stealing of Underwear, %bene = Guaranteed stealth for the next 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Running runaway run,$2,32)) { var %title = Running Away From Trouble, %bene = 100% Run away chance for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Medical meds med hosp hospital duration time times,$2,32)) { var %title = Medical Degree Schmedical Degree, %bene = Decreases all hospital times by 50% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(No More Soap jail duration time times,$2,32)) { var %title = No More Soap on a Rope, %bene = Decreases all jail times by 50% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Self cans candy cooldowns cooldown alcohol booze beer,$2,32)) { var %title = Self Control is for Losers, %bene = Decreases all consumable cooldowns by 50% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Going cooldown cooldowns medical med,$2,32)) { var %title = Going Back For More, %bene = Decreases all medical cooldowns by 50% for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Get Drunk beer booze alcohol nerve effect effects,$2,32)) { var %title = Get Drunk and Lose Dignity, %bene = Doubles alcohol effects for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Fuelling cans energy,$2,32)) { var %title = Fuelling Your Way to Failure, %bene = Doubles energy drink effects for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Yes Diabetes candy happiness happy effect effects,$2,32)) { var %title = Yes Please Diabetes, %bene = Doubles candy effects for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Ignorance happy happiness maximum max,$2,32)) { var %title = Ignorance is Bliss, %bene = Happiness above maximum does not reset for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Making friends enemies maximum max,$2,32)) { var %title = Making Friends $+ $chr(44) Enemies $+ $chr(44) and Cakes, %bene = Increases blacklist & friend list capacity by 100 upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(High merit reset,$2,32)) { var %title = High School For Adults, %bene = Provides a free merit reset upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Milk addiction drug,$2,32)) { var %title = Milk Yourself Sober, %bene = Removes a substantial amount of drug addiction upon completion. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Shawshank jail bust,$2,32)) { var %title = Shawshank Sure Ain't For Me!, %bene = Large jail bust & escape boost for the next 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Winking contract mission,$2,32)) { var %title = Winking to Win, %bene = Doubles contract credit and money rewards for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Finders Keepers city item,$2,32)) { var %title = Finders Keepers, %bene = Drastically increases city item spawns for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Duke retaliate,$2,32)) { var %title = Duke - My Story, %bene = Duke will occasionally retaliate against your attackers for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        if ($istok(Memories Mammaries repeat,$2,32)) { var %title = Memories and Mammaries, %bene = Takes the same effect from the last used book for 31 days. | tc.bookdisp # $+ $chr(124) $+ %title $+ $chr(124) $+ %bene }
        ; If this is called, we haven't displayed a book. Time to bitch.
        if (%title == $null) { msg # $tc.col(5) $+ I have no idea what " $+ $tc.col(2) $+ $2- $+ $tc.col(5) $+ " is. }        
      }
    }
    ; --------------------------------- drug lookup commands ---------------------------------    

    if ($1 == !druginfo || $1 == !dinfo) {
      if ($istok(xanax xan,$2,32)) { 
        var %dname = Xanax, %dcd = 360-480 mins
        var %deff = $tc.col(6) $+ 250 energy, $tc.col(6) $+ 75 happiness
        var %dod = $tc.col(7) $+ 5000 minutes in hosp, $tc.col(7) $+ ~3 xanax drug addiction gain, $tc.col(7) $+ 100% energy/happiness/nerve loss
        var %dodcd = ~24 hours
        var %dpass = $tc.col(7) $+ 35% passive decrease to all battle stats
      }
      if ($istok(vicodin vic,$2,32)) { 
        var %dname = Vicodin, %dcd = 240-360 mins
        var %dod = $tc.col(7) $+ 150 happiness loss
        var %dodcd = 240-360 mins
        var %dpass = $tc.col(6) $+ 25% passive increase to all battle stats
      }
      if ($istok(speed,$2,32)) { 
        var %dname = Speed, %dcd = 268 mins
        var %dod = $tc.col(7) $+ 450 minutes in hospital, $tc.col(7) $+ perminant loss of strength and defense (6x player level)
        var %dodcd = 268-300 mins
        var %dpass = $tc.col(6) $+ 20% passive increase to speed, $tc.col(7) $+ 20% passive decrease to dexterity
      }
      if ($istok(shrooms mushrooms,$2,32)) { 
        var %dname = Shrooms, %dcd = 200-234 mins
        var %deff = $tc.col(6) $+ 400-500 happiness, $tc.col(7) $+ 25 energy decrease (will not go under 0)
        var %dod = $tc.col(7) $+ 100 minutes in hospital, $tc.col(7) $+ 100% energy/happiness/nerve loss
        var %dodcd = 200-234 mins
        var %dpass = $tc.col(7) $+ 20% passive decrease to all battle stats
      }
      if ($istok(pcp pvp,$2,32)) { 
        var %dname = PCP, %dcd = 340-400 mins
        var %deff = $tc.col(6) $+ 250 happiness
        var %dod = $tc.col(7) $+ 1000 minutes in hospital
        var %dodcd = 340-400 mins
        var %dpass = $tc.col(6) $+ 20% passive increase to strength and dexterity,
      }
      if ($istok(opium,$2,32)) { 
        var %dname = Opium, %dcd = 200-250 mins
        var %deff = $tc.col(6) $+ Removes all standard hosp time, $tc.col(6) $+ 65-90 happiness
        var %dod = $tc.col(6) $+ No overdose
        var %dodcd = N/A
      }
      if ($istok(lsd acid,$2,32)) { 
        var %dname = LSD, %dcd = 400-450 mins
        var %deff = $tc.col(6) $+ 50 energy, $tc.col(6) $+ 20-50 happiness, $tc.col(6) $+ 5 nerve
        var %dod = $tc.col(7) $+ 100% energy/nerve loss, $tc.col(7) $+ 50% happiness loss
        var %dodcd = 400-450 mins
        var %dpass = $tc.col(6) $+ 30% passive increase in strength, $tc.col(6) $+ 50% passive increase in defense, $tc.col(7) $+ 30% passive decrease in speed/dexterity
      }
      if ($istok(ketamine,$2,32)) { 
        var %dname = Ketamine, %dcd = 50-90 mins      
        var %dod = $tc.col(7) $+ 1000 minutes hosp, $tc.col(7) $+ 100% energy/nerve loss, $tc.col(7) $+ increased after effects (decreased speed/dex during after effects = 24-27hrs)
        var %dodcd = 50-90 mins
        var %dpass = $tc.col(6) $+ 50% passive increase to defense, $tc.col(7) $+ 20% passive decrease to speed and strength
      }
      if ($istok(ecstasy xtc,$2,32)) { 
        var %dname = Ecstasy, %dcd = 200-220 mins
        var %deff = $tc.col(6) $+ Doubles current happiness
        var %dod = $tc.col(7) $+ 100% energy/happiness loss
        var %dodcd = 200-220 mins
      }
      if ($istok(cannabis weed cheeb 420,$2,32)) { 
        var %dname = Cannabis, %dcd = 60-90 mins 
        var %deff = $tc.col(6) $+ Increased crime success rate, $tc.col(6) +0-3 nerve
        var %dod = $tc.col(7) $+ 300-330 minutes hosp time, $tc.col(7) $+ 100% energy/happiness/nerve loss, $tc.col(6) $+ awards 'Spaced Out' honor
        var %dodcd = 60-90 mins - very rare
        var %dpass = $tc.col(7) $+ 20% passive decrease in strength, $tc.col(7) $+ 25% passive decrease in defense, $tc.col(7) $+ 35% passive decrease in speed
      }
      if (%dname != $null) { 
        var %drugheader = $tc.col(2) $+ %dname $+ $tc.col(1) - Cooldown: $+ $tc.col(2) %dcd 
        if (%deff != $null) { var %drugheader = %drugheader $tc.col(1) $+ - Additional Effect: $+ $tc.col(2) $replace(%deff,$chr(44),$+($tc.col(1),$chr(44),$chr(32))) }
        msg # %drugheader
        if (%dpass != $null) { msg # $tc.col(1) $+ Passive Stat changes: $+ $tc.col(2) $replace(%dpass,$chr(44),$+($tc.col(1),$chr(44),$chr(32))) }
        msg # $tc.col(1) $+ Overdose Effect $tc.col(8) $+ ( $+ %dodcd $+ ): $+ $tc.col(2) $replace(%dod,$chr(44),$+($tc.col(1),$chr(44),$chr(32))) 
      }
      else {
        msg # $tc.col(3) $+ Displays information about a drug. $tc.col(1) $+ Usage: $+ $tc.col(2) !dinfo Drug $tc.col(1) $+ - Source: $+ $tc.col(2) https://www.torn.com/wiki/Drugs
      }
    }

    ; --------------------------------- mod lookup commands ---------------------------------    

    if ($1 = !mods || $1 == !mod) {
      msg # $tc.col(3) $+ Commands: $+ $tc.col(2) !rangemod $+ $tc.col(1) = Primary/Secondary Mods $+ $tc.col(1) $+ , $+ $tc.col(2) !meleemod $+ $tc.col(1) = Melee Mods $tc.col(8) $+ (inactive until release) $+ $tc.col(1) $+ , $+ $tc.col(2) !armormod $+ $tc.col(1) = Armor mods $tc.col(8) $+ (inactive until release)
    }

    if ($1 = !rangemods || $1 == !rangemod) {
      if ($2 == sight || $2 == sights) { 
        var %s1n = Reflex Sight, %s1b = $tc.col(6) $+ +14% Accuracy
        var %s2n = Holographic Sight, %s2b = $tc.col(6) $+ +16% Accuracy
        var %s3n = Acog Sight, %s3b = $tc.col(6) $+ +18% Accuracy
        var %s4n = Thermal Sight, %s4b = $tc.col(6) $+ +20% Accuracy
        var %compat = MCG PST RFL SMG, %excl = Dual Primary Weapons
      }
      if ($2 == laser || $2 == lasers) { 
        var %s1n = 1mw Laser, %s1b = $tc.col(6) $+ +3% Critical Hit Chance
        var %s2n = 5mw Laser, %s2b = $tc.col(6) $+ +4% Critical Hit Chance
        var %s3n = 30mw Laser, %s3b = $tc.col(6) $+ +5% Critical Hit Chance
        var %s4n = 100mw Laser, %s4b = $tc.col(6) $+ +6% Critical Hit Chance
        var %compat = HRT MCG PST RFL SHT SMG
      }
      if ($2 == suppressor || $2 == suppressors || $2 == sup || $2 == supp) { 
        var %s1n = Small Suppressor, %s1b = $tc.col(6) $+ +3 Stealth Rating $+ $tc.col(1) $+ , $+ $tc.col(7) -5% Damage
        var %s2n = Standard Suppressor, %s2b = $tc.col(6) $+ +4 Stealth Rating $+ $tc.col(1) $+ , $+ $tc.col(7) -5% Damage
        var %s3n = Large Suppressor, %s3b = $tc.col(6) $+ +5 Stealth Rating $+ $tc.col(1) $+ , $+ $tc.col(7) -5% Damage
        var %compat = PST RFL SMG
        var %excl = No muzzle equipped
      }
      if ($2 == mags || $2 == mag || $2 == magazine || $2 == magazines) { 
        var %s1n = Extended Mags, %s1b = $tc.col(6) $+ +20% Clip Size
        var %s2n = High Capacity Mags, %s2b = $tc.col(6) $+ +30% Clip Size
        var %compat = MCG PST RFL SHT SMG, %excl = Dual Primary Weapons
      }
      if ($2 == add || $2 == additional || $2 == extra || $2 == clip) { 
        var %s1n = Extra Clip x1, %s1b = $tc.col(6) $+ One extra clip in fight
        var %s2n = Extra Clip x2, %s2b = $tc.col(6) $+ Two extra clips in fight
        var %compat = HRT MCG PST RFL SHT SMG, %excl = Dual Primary Weapons
      }      
      if ($2 == trigger || $2 == triggers || $2 == triggered) { 
        var %s1n = Adjustable Trigger, %s1b = $tc.col(6) $+ Doubles speed stat on 1st turn
        var %s2n = Hair Trigger, %s2b = $tc.col(6) $+ Triples speed stat on 1st turn
        var %compat = MCG PST RFL SHT SMG
      }      
      if ($2 == mount || $2 == mounts) { 
        var %s1n = Bipod, %s1b = $tc.col(6) $+ +20% Accuracy $+ , $+ $tc.col(7) -30% Dexterity
        var %s2n = Tripod, %s2b = $tc.col(6) $+ +25% Accuracy $+ , $+ $tc.col(7) -30% Dexterity
        var %compat = MCG PST RFL SHT SMG, %excl = Dual Primary Weapons
      } 
      if ($2 == grips || $2 == grip) { 
        var %s1n = Custom Grip, %s1b = $tc.col(6) $+ +7.5% Accuracy
        var %compat = MCG PST RFL SHT SMG
      } 
      if ($2 == chokes || $2 == choke) { 
        var %s1n = Skeet Choke, %s1b = $tc.col(6) $+ +6% Damage
        var %s2n = Improved Choke, %s2b = $tc.col(6) $+ +8% Damage
        var %s3n = Full Choke, %s3b = $tc.col(6) $+ +10% Damage
        var %compat = SHT
      } 
      if ($2 == recoil) { 
        var %s1n = Recoil Pad, %s1b = $tc.col(6) $+ +25% Ammo Control        
        var %compat = MCG RFL SHT
      } 
      if ($2 == muzzle || $2 == muzzles) { 
        var %s1n = Standard Brake, %s1b = $tc.col(6) $+ +10% Accuracy $+ , $+ $tc.col(7) -3 Stealth Rating
        var %s2n = Heavy Duty Brake, %s2b = $tc.col(6) $+ +13% Accuracy $+ , $+ $tc.col(7) -3 Stealth Rating
        var %s3n = Tactical Brake, %s3b = $tc.col(6) $+ +16% Accuracy $+ , $+ $tc.col(7) -3 Stealth Rating
        var %compat = MCG PST RFL SMG
        var %excl = No suppressor equipped
      } 
      if ($2 == light || $2 == lights) { 
        var %s1n = Small Light, %s1b = $tc.col(6) $+ -6% Opponent Accuracy $+ , $+ $tc.col(6) -3 Opponent Stealth Rating
        var %s2n = Precision Light, %s2b = $tc.col(6) $+ -8% Opponent Accuracy $+ , $+ $tc.col(6) -3 Opponent Stealth Rating
        var %s3n = Tactical Illuminator, %s3b = $tc.col(6) $+ -10% Opponent Accuracy $+ , $+ $tc.col(6) -3 Opponent Stealth Rating
        var %compat = PST RFL SMG SHT
      } 
      if (%s1n != $null) { 
        msg # $tc.col(1) $+ Stage 1: $+ $tc.col(2) %s1n $tc.col(1) $+ = $+ $tc.col(2) %s1b $+ $tc.col(1) $+ .
        if (%s2n != $null) { msg # $tc.col(1) $+ Stage 2: $+ $tc.col(2) %s2n $tc.col(1) $+ = $+ $tc.col(2) %s2b $+ $tc.col(1) $+ . }
        if (%s3n != $null) { msg # $tc.col(1) $+ Stage 3: $+ $tc.col(2) %s3n $tc.col(1) $+ = $+ $tc.col(2) %s3b $+ $tc.col(1) $+ . }
        if (%s4n != $null) { msg # $tc.col(1) $+ Stage 4: $+ $tc.col(2) %s4n $tc.col(1) $+ = $+ $tc.col(2) %s4b $+ $tc.col(1) $+ . }
        msg # $tc.col(1) $+ Compatibility: $+ $tc.col(2) $replace(%compat,$chr(32),$+($tc.col(1),$chr(44),$tc.col(2),$chr(32)),HRT,Heavy Artillery,MCG,Machine Guns,PST,Pistols,RFL,Rifles,SHT,Shotguns,SMG,Submachine Guns) $+ $tc.col(1) $+ .
        if (%excl != $null) { msg # $tc.col(1) $+ Exclusions: $+ $tc.col(2) %excl $+ $tc.col(1) $+ . }
      }
      else {
        msg # $tc.col(3) $+ Displays information about a mod. $tc.col(1) $+ Usage: $+ $tc.col(2) !rangemod (mod) 
        msg # $tc.col(1) $+ List: $+ $tc.col(2) Laser $+ $tc.col(1) $+ , $+ $tc.col(2) Sight $+ $tc.col(1) $+ , $+ $tc.col(2) Suppressor $+ $tc.col(1) $+ , $+ $tc.col(2) Magazine Size $+ $tc.col(1) $+ , $+ $tc.col(2) Additional Mags $+ $tc.col(1) $+ , $+ $tc.col(2) Trigger $+ $tc.col(1) $+ , $+ $tc.col(2) Mount $+ $tc.col(1) $+ , $+ $tc.col(2) Grip $+ $tc.col(1) $+ , $+ $tc.col(2) Muzzle Brakes
        msg # $tc.col(1) $+ Source: $+ $tc.col(2) http://www.torn.com/forums.php#!p=threads&f=61&t=15979865&b=0&a=0 $tc.col(1) $+ - Thread credit: $tc.col(2) $+ Lord_Gorgen $tc.colid(1629756,1,2)
      }
    }
    if ($1 == !meleemods || $1 == !meleemod) { msg # $tc.col(4) $+ Inactive until release. }
    if ($1 == !armormods || $1 == !armormod || $1 == !armourmods || $1 == !armourmod) { msg # $tc.col(4) $+ Inactive until release. }


    ; --------------------------------- other lookup commands ---------------------------------
    if ($1 == !ammo) {
      var %hp = $tc.col(2) $+ Hollow Point $+ $tc.col(1) = $tc.col(6) $+ damage increase $+ $tc.col(1) $+ , $tc.col(7) $+ armor penetration decrease $+ $tc.col(1) $+ .
      var %tr = $tc.col(2) $+ Tracer $+ $tc.col(1) = $tc.col(6) $+ accuracy increase $+ $tc.col(1) $+ .
      var %pi = $tc.col(2) $+ Piercing $+ $tc.col(1) = $tc.col(6) $+ armor penetration increase $+ $tc.col(1) $+ .
      var %in = $tc.col(2) $+ Incendiary $+ $tc.col(1) = $tc.col(6) $+ damage increase $+ $tc.col(1) $+ .
      msg # %hp %tr %pi %in
    }
    if ($1 == !blood) {
      if ($2 == O-) { var %type = O-, %comp = O-, %incomp = O+ A- A+ B- B+ AB- AB+ }
      if ($2 == O+) { var %type = O+, %comp = O- O+, %incomp = A- A+ B- B+ AB- AB+ }
      if ($2 == A-) { var %type = A-, %comp = O- A-, %incomp = O+ A+ B- B+ AB- AB+ }
      if ($2 == A+) { var %type = A+, %comp = O- O+ A- A+, %incomp = B- B+ AB- AB+ }
      if ($2 == B-) { var %type = B-, %comp = O- B-, %incomp = O+ A- A+ B+ AB- AB+ }
      if ($2 == B+) { var %type = B+, %comp = O- O+ B- B+, %incomp = A- A+ AB- AB+ }
      if ($2 == AB-) { var %type = AB-, %comp = O- A- B- AB-, %incomp = O+ A+ B+ AB+ }
      if ($2 == AB+) { var %type = AB+, %comp = O- O+ A- A+ B- B+ AB- AB+ }
      if (%comp != $null) { 
        msg # $tc.col(1) $+ Type $+ $tc.col(2) %type $tc.col(1) $+ is $tc.col(6) $+ compatible $+ $tc.col(1) with: $+ $tc.col(2) $replace(%comp,$chr(32),$+($tc.col(1),$chr(44),$tc.col(2),$chr(32))) $+ $tc.col(1) $+ . 
        if (%incomp != $null) { msg # $tc.col(1) $+ Type $+ $tc.col(2) %type $tc.col(1) $+ is $tc.col(7) $+ incompatible $+ $tc.col(1) with: $+ $tc.col(2) $replace(%incomp,$chr(32),$+($tc.col(1),$chr(44),$tc.col(2),$chr(32))) $+ $tc.col(1) $+ . }
      }
      else { msg # $tc.col(1) $+ Blood Type Compatibility Chart: $+ $tc.col(2) http://i.imgur.com/TyQTvhr.jpg }


    }
    if ($1 == !se) {
      if ($2 == $null) {
        var %db = $tc.col(2) $+ Dumbbells: $+ $tc.col(1) = $tc.col(6) $+ Increases Strength $+ $tc.col(1) $+ .
        var %sk = $tc.col(2) $+ Skateboard: $+ $tc.col(1) = $tc.col(6) $+ Increases Speed $+ $tc.col(1) $+ .
        var %bg = $tc.col(2) $+ Boxing Gloves: $+ $tc.col(1) = $tc.col(6) $+ Increases Defense $+ $tc.col(1) $+ .
        var %pa = $tc.col(2) $+ Parachute: $+ $tc.col(1) = $tc.col(6) $+ Increases Dexterity $+ $tc.col(1) $+ .
        msg # %db %sk %bg %pa
      }
      else {
        if ($istok(str spd def dex,$2,32) && $3 isnum) {
          if ($3 > 1000) { msg # $tc.col(5) $+ Nope. | halt }        
          var %retid = $tc.getid($nick), %retname = $tc.getname(%retid)
          if (%retid == $null) { msg # $tc.col(5) $+ Sorry $+ $tc.col(2) %find $+ $tc.col(5) $+ , I have no clue who you are! }
          else { 
            var %statsfile = $tc.statsfile(%retid), %lastsaved = $read($qt(%statsfile), $lines(%statsfile))
            if ($2 == str) { var %statname = Strength, %sename = Dumbbells, %stat = $gettok(%lastsaved,2,32) }
            if ($2 == spd) { var %statname = Speed, %sename = Skateboards, %stat = $gettok(%lastsaved,4,32) }
            if ($2 == def) { var %statname = Defense, %sename = Boxing Gloves, %stat = $gettok(%lastsaved,3,32) }
            if ($2 == dex) { var %statname = Dexterity, %sename = Parachutes, %stat = $gettok(%lastsaved,5,32) }
            var %ptr = 1, %end = $3, %endstat = %stat
            while (%ptr <= %end) {
              var %endstat = $calc(%endstat * 1.01)
              inc %ptr
            }
            msg # $tc.col(1) $+ Assuming no additional trains, after $+ $tc.col(2) $3 %sename $+ $tc.col(1) $+ , your $+ $tc.col(2) %statname $+ $tc.col(1) will end up at $+ $tc.col(2) $tc.addComma($round(%endstat,4)) $+ $tc.col(1) $+ .
          }
        }
        else { 
          msg # $tc.col(3) $+ Stat enchancer encyclopaedia. $tc.col(1) $+ Using without any parameters will display a summary.
          msg # $tc.col(1) Usage: $+ $tc.col(2) !se (str/spd/def/dex) (Num) $+ $tc.col(1) - will display your saved stat after (Num) amount of stat enhancers. 
        }
      }
    }

    ; --------------------------------- crimes storage commands ---------------------------------

    if ($istok(Selling Theft Auto Drug Computer Fraud Murder Other,$remove($1,$chr(58)),32)) {
      if ($1-3 == Selling illegal goods: || $1-3 == Selling illegal products) { 
        if ($5 == $null) { if ($tc.cleanN($4) != $null) { var %cloc = 3, %type = Selling Illegal Products, %crimes = $strip($replace($tc.cleanN($4),$chr(44),$null)) } }
        else { msg # $tc.col(5) $+ Sorry, crime storage does not support pasting from mibbit. Please paste as individual lines for the time being or use a real client. ;) | halt }
      }
      if ($1 == Theft: || $1 == Theft) { if ($tc.cleanN($2) != $null) { var %cloc = 4, %type = Thefts, %crimes = $strip($replace($tc.cleanN($2),$chr(44),$null)) } }
      if ($1-2 == Auto theft: || $1-2 == Auto theft) { if ($tc.cleanN($3) != $null) { var %cloc = 5, %type = Auto Thefts, %crimes = $strip($replace($tc.cleanN($3),$chr(44),$null)) } }
      if ($1-2 == Drug deals: || $1-2 == Drug deals) { if ($tc.cleanN($3) != $null) { var %cloc = 6, %type = Drug Deals, %crimes = $strip($replace($tc.cleanN($3),$chr(44),$null)) } }
      if ($1-2 == Computer crimes: || $1-2 == Computer crimes) { if ($tc.cleanN($3) != $null) { var %cloc = 7, %type = Computer Crimes, %crimes = $strip($replace($tc.cleanN($3),$chr(44),$null)) } }
      if ($1 == Fraud: || $1-2 == Fraud crimes) { var %cloc = 8, %type = Frauds
        if ($2 == crimes) { if ($tc.cleanN($3) != $null) { var %crimes = $strip($replace($tc.cleanN($3),$chr(44),$null)) } }
        else { if ($tc.cleanN($2) != $null) { var %crimes = $strip($replace($tc.cleanN($2),$chr(44),$null)) } }      
      }
      if ($1 == Murder: || $1 == Murder) { if ($tc.cleanN($2) != $null) { var %cloc = 9, %type = Murders, %crimes = $strip($replace($tc.cleanN($2),$chr(44),$null)) } }
      if ($1 == Other: || $1 == Other) { if ($tc.cleanN($2) != $null) { var %cloc = 10, %type = Other Crimes, %crimes = $strip($replace($tc.cleanN($2),$chr(44),$null)) } }

      if (%crimes > 1000000) { msg # $tc.col(5) $+ Nope. | halt } | if (%crimes !isnum) { halt }

      if (%type != $null) {
        if (%tc.crold. [ $+ [ $nick ] ] != $null) { var %id = $gettok(%tc.crold. [ $+ [ $nick ] ],1,32) } | else { var %id = $tc.getid($nick) }        
        if (%id != $null) {
          if (%tc.crold. [ $+ [ $nick ] ] == $null) { 
            var %ptr = 1, %eof = $lines($tc.crimesfile)
            while (%ptr <= %eof) { if (%id == $gettok($read($tc.crimesfile,%ptr),1,32)) { set %tc.crold. [ $+ [ $nick ] ] $read($tc.crimesfile,%ptr) | break } | inc %ptr }
          }
          ; Not the greatest initialization method, but it'll do for now.
          if (%tc.crold. [ $+ [ $nick ] ] == $null) { set %tc.crold. [ $+ [ $nick ] ] %id $tc.time -1 -1 -1 -1 -1 -1 -1 -1 } 
          if (%tc.crnew. [ $+ [ $nick ] ] == $null) { set %tc.crnew. [ $+ [ $nick ] ] %tc.crold. [ $+ [ $nick ] ] }
          if (%tc.crupd. [ $+ [ $nick ] ] == $null) { set %tc.crupd. [ $+ [ $nick ] ] 0 0 0 0 0 0 0 0 }
          set %tc.crnew. [ $+ [ $nick ] ] $puttok(%tc.crnew. [ $+ [ $nick ] ],$tc.time,2,32)
          set %tc.crnew. [ $+ [ $nick ] ] $puttok(%tc.crnew. [ $+ [ $nick ] ],%crimes,%cloc,32)
          set %tc.crupd. [ $+ [ $nick ] ] $puttok(%tc.crupd. [ $+ [ $nick ] ],1,$calc(%cloc - 2),32)
          var %crimedata = $tc.crimedata(%cloc,%crimes)
          if (%crimes != $gettok(%tc.crold. [ $+ [ $nick ] ],%cloc,32) && %crimes != 0) {    
            if ($gettok(%tc.crold. [ $+ [ $nick ] ],%cloc,32) == -1) { 
              if (%crimes >= $gettok(%crimedata,2,124)) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(3) $+ You have all the merits/medals for this category! }
              elseif ($gettok(%crimedata,3,124) != 0 && $gettok(%crimedata,5,124) == 0) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(2) $+ $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,3,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,3,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,4,124) $tc.col(1) $+ merit. }
              elseif ($gettok(%crimedata,3,124) == 0 && $gettok(%crimedata,5,124) != 0) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(2) $+ $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,5,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,5,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,6,124) $tc.col(1) $+ medal. }
              else { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(2) $+ $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,5,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,5,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,6,124) $tc.col(1) $+ medal and $+ $tc.col(2) $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,3,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,3,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,4,124) $tc.col(1) $+ merit. }
            }
            else {
              var %change = $calc(%crimes - $gettok(%tc.crold. [ $+ [ $nick ] ],%cloc,32))
              if (%change < 0) { var %chgtxt = $tc.col(7) $+ down } | else { var %chgtxt = $tc.col(6) $+ up }
              if (%crimes >= $gettok(%crimedata,2,124)) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(3) $+ You have all the merits/medals for this category! }
              elseif ($gettok(%crimedata,3,124) != 0 && $gettok(%crimedata,5,124) == 0) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(1) $+ You have gone %chgtxt $+ $tc.col(2) $remove($tc.addComma(%change),$chr(45)) $tc.col(1) $+ attempts since your last update $+ $tc.col(2) $duration($calc($tc.time - $gettok(%tc.crold. [ $+ [ $nick ] ],2,32))) $tc.col(1) $+ ago, and now have $+ $tc.col(2) $tc.addComma($calc($gettok(%crimedata,3,124) - %crimes)) $tc.col(1) $+ to go towards the $+ $tc.col(2) $gettok(%crimedata,4,124) $tc.col(1) $+ merit. }
              elseif ($gettok(%crimedata,3,124) == 0 && $gettok(%crimedata,5,124) != 0) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(1) $+ You have gone %chgtxt $+ $tc.col(2) $remove($tc.addComma(%change),$chr(45)) $tc.col(1) $+ attempts since your last update $+ $tc.col(2) $duration($calc($tc.time - $gettok(%tc.crold. [ $+ [ $nick ] ],2,32))) $tc.col(1) $+ ago, and now have $+ $tc.col(2) $tc.addComma($calc($gettok(%crimedata,5,124) - %crimes)) $tc.col(1) $+ to go towards the $+ $tc.col(2) $gettok(%crimedata,6,124) $tc.col(1) $+ medal. }
              else { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(1) $+ You have gone %chgtxt $+ $tc.col(2) $remove($tc.addComma(%change),$chr(45)) $tc.col(1) $+ attempts since your last update $+ $tc.col(2) $duration($calc($tc.time - $gettok(%tc.crold. [ $+ [ $nick ] ],2,32))) $tc.col(1) $+ ago. You now have $+ $tc.col(2) $tc.addComma($calc($gettok(%crimedata,5,124) - %crimes)) $tc.col(1) $+ to go towards the $+ $tc.col(2) $gettok(%crimedata,6,124) $tc.col(1) $+ medal and $+ $tc.col(2) $tc.addComma($calc($gettok(%crimedata,3,124) - %crimes)) $tc.col(1) $+ to go towards the $+ $tc.col(2) $gettok(%crimedata,4,124) $tc.col(1) $+ merit. }            
            }
          }
          else {
            if ($gettok(%tc.crold. [ $+ [ $nick ] ],%cloc,32) != -1) {
              if (%tc.crnochg. [ $+ [ $nick ] ] == $null) { set %tc.crnochg. [ $+ [ $nick ] ] $tc.col(2) $+ %type }
              else { set %tc.crnochg. [ $+ [ $nick ] ] %tc.crnochg. [ $+ [ $nick ] ] $+ $tc.col(1) $+ $chr(44) $+ $tc.col(2) %type }
            }
          }
          if ($findtok(%tc.crupd. [ $+ [ $nick ] ],1,0,32) == 8) { tc.crimesave $nick # }
          else { .timer $+ $nick $+ $chr(46) $+ crimes -co 1 2 tc.crimesave $nick # }
        }
        else { .timer $+ $nick $+ $chr(46) $+ crimes -co 1 3 msg # $tc.col(2) $+ $nick $+ $tc.col(5) $+ , you have no ID saved with the bot! } 
      }
    }
    if ($1 == !crimes) {
      if ($timer(0.tcflag.spam)) { msg # $tc.col(5) $+ No can do, too many requests too often. You can try again in $+ $tc.col(2) $timer(0.tcflag.spam).secs $tc.col(5) $+ seconds. | halt }
      if ($isfile($tc.crimesfile)) {  
        if ($2 != $null) { var %find $2 } | else { var %find = $nick }
        var %id = $tc.getid(%find), %name = $tc.getname(%id)
        if (%id != $null) { 
          var %ptr = 1, %eof = $lines($tc.crimesfile)
          while (%ptr <= %eof) { if (%id == $gettok($read($tc.crimesfile,%ptr),1,32)) { var %crimeread = $read($tc.crimesfile,%ptr) | break } | inc %ptr }
          if (%crimeread == $null) { msg # $tc.col(5) $+ No saved crimes found for $+ $tc.col(2) %name $+ $tc.col(5) $+ ! }
          else {
            var %ptr = 3 | while (%ptr <= 10) {
              var %crimedata = $tc.crimedata(%ptr,$gettok(%crimeread,%ptr,32)), %crimes = $gettok(%crimeread,%ptr,32)
              if (%crimes >= $gettok(%crimedata,2,124)) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(6) $+ You have all the medals/merits! }
              elseif ($gettok(%crimedata,3,124) != 0 && $gettok(%crimedata,5,124) == 0 && %crimes > 0) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(2) $+ $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,3,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,3,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,4,124) $tc.col(1) $+ merit. }
              elseif ($gettok(%crimedata,3,124) == 0 && $gettok(%crimedata,5,124) != 0 && %crimes > 0) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(2) $+ $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,5,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,5,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,6,124) $tc.col(1) $+ medal. }
              elseif (%crimes != 0) { msg # $tc.col(8) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(2) $+ $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,5,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,5,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,6,124) $tc.col(1) $+ medal and $+ $tc.col(2) $tc.addComma(%crimes) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma($gettok(%crimedata,3,124)) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc($gettok(%crimedata,3,124) - %crimes)) $tc.col(1) $+ to go) towards the $+ $tc.col(2) $gettok(%crimedata,4,124) $tc.col(1) $+ merit. }
              else { msg # $tc.col(1) $+ $gettok(%crimedata,1,124) $+ $chr(58) $tc.col(7) $+ Zero Crimes. }
              inc %ptr
            }
            .timer $+ 0.tcflag.spam 1 $tc.gcd noop
          }
        }
        else { msg # $tc.col(5) $+ No saved ID found for $+ $tc.col(2) %name $+ $tc.col(5) $+ ! }
      }
      else { msg # $tc.col(5) $+ No-one's crimes have been saved yet! }
    }

    ; --------------------------------- banker commands --------------------------------- 

    if ($1 == !banker || $1 == !bankers) {
      if ($readini($tc.cfgfile,n,Banker,enable) == e) { 
        var %line $readini($tc.cfgfile,n,Banker,line)
        if (%line) { msg # $tc.col(1) $+ %line }
        else { msg # $tc.col(5) $+ No banker line set, use $+ $tc.col(2) !setbanker $+ $tc.col(5) to set it. }
      }
    }
    if ($1 == !setbanker || $1 == !setbankers) {
      if ($readini($tc.cfgfile,n,Banker,enable) == e) { 
        if ($2- != $null) {
          if ($tc.chkadm($nick) == 1) {
            writeini -n $tc.cfgfile Banker line $2-
            flushini $tc.cfgfile
            msg # $tc.col(1) $+ Banker line has been set to: $+ $tc.col(2) $2-
          }
          else { msg # $tc.col(5) $+ No can do, you need to be a bot admin to use $+ $tc.col(2) !setbanker }
        }
        else { msg # $tc.col(3) $+ Sets the line to be displayed when someone uses !banker. $tc.col(8) $+ (Requires bot admin) $tc.col(1) $+ Usage: $+ $tc.col(2) !setbanker wordswordswords (color codes can be used) }
      }
    }

    ; --------------------------------- quote bot commands --------------------------------- 

    if ($1 == !addquote) {
      if ($2 != $null) {
        write $tc.quotefile $chan $+ $chr(9) $+ $nick $+ $chr(9) $+ $tc.time $+ $chr(9) $+ $replace($2-,$chr(9),$chr(32))
        if ($readini($tc.cfgfile,n,Quote,split) == yes) {
          var %ctr = $tc.qscan($chan)
          msg $chan $tc.col(1) $+ Added quote number $+ $tc.col(2) %ctr $tc.col(1) $+ for $+ $tc.col(2) $chan $+ $tc.col(1) $+ !
        } 
        else { msg $chan $tc.col(1) $+ Added quote number $+ $tc.col(2) $lines($tc.quotefile) $+ $tc.col(1) $+ ! }
      }
      else { msg $chan $tc.col(3) $+ Adds a quote to the bot. $tc.col(1) $+ Usage: $+ $tc.col(2) !addquote Quote }
    }

    if ($1 == !quote) {
      if ($2 isnum && $2 > 0) { 
        ; Scan for a specific quote number (using !quote Number)
        if ($readini($tc.cfgfile,n,Quote,split) == yes) { 
          var %ctr = $tc.qscan($chan)
          if (%ctr != $null && %ctr > 0) { var %loc = $tc.qscan($chan,$2) }
          else { msg $chan $tc.col(5) $+ No quotes saved for this channel. | halt }
        }
        else { var %ctr = $lines($tc.quotefile), %loc = $2 }
        var %qrand = $2
      }
      elseif ($2 == $null) {
        ; Scan for a random quote number (just !quote by itself)
        if ($readini($tc.cfgfile,n,Quote,split) == yes) {
          var %ctr = $tc.qscan($chan)
          if (%ctr != $null && %ctr > 0) { 
            var %qrand = $rand(1,%ctr) 
            var %loc = $tc.qscan($chan,%qrand)
          }
          else { msg $chan $tc.col(5) $+ No quotes saved for this channel. | halt }
        }
        else { 
          var %ctr = $lines($tc.quotefile)
          if (%ctr > 0) { var %loc = $rand(1,%ctr), %qrand = %loc }
          else { msg $chan $tc.col(5) $+ No quotes saved in database. | halt }           
        }
      }
      ; Help
      else { msg $chan $tc.col(3) $+ Displays a stored quote. $tc.col(1) $+ Usage: $+ $tc.col(2) !quote Number $+ $tc.col(1) $+ . Using $+ $tc.col(2) !quote $tc.col(1) $+ with no number will have the bot select a random quote. | halt }
      if (%qrand > %ctr) { msg $chan $tc.col(5) $+ There are only $+ $tc.col(2) %ctr $tc.col(5) $+ quotes stored. | halt }
      ; should only ever reach this point if it has everything it needs. There should be checks throughout above for errors.
      var %quote = $gettok($read($tc.quotefile,n,%loc),4,9)
      if (%quote != $null) { msg $chan $tc.col(8) $+ Quote $+ $tc.col(2) %qrand $+ $tc.col(8) $+ / $+ $tc.col(2) $+ %ctr $+ $tc.col(8) $+ : $+ $tc.col(1) $eval(%quote,1) }
      else { msg $chan $tc.col(5) $+ Quote $+ $tc.col(2) %qrand $tc.col(5) $+ was not found. }
    }

    if ($1 == !delquote) {
      tc.chkaccess delquote $nick $chan
      if ($2 isnum && $2 > 0) {
        if ($readini($tc.cfgfile,n,Quote,split) == yes) {
          var %ptr = 1, %ctr = 0
          while (%ptr <= $lines($tc.quotefile)) {
            if ($gettok($read($tc.quotefile,n,%ptr),1,9) == $chan) { 
              inc %ctr 
              if (%ctr = $2) { var %loc = %ptr | break }
            }
            inc %ptr
          }
          if (%loc != $null) {
            write -dl $+ %loc $tc.quotefile 
            msg $chan $tc.col(1) $+ Deleted quote number $+ $tc.col(2) $2 $+ $tc.col(1) $+ !
          } 
          else { msg $chan $tc.col(5) $+ Quote $+ $tc.col(2) $2 $tc.col(5) $+ was not found. }
        } 
        else { 
          if ($read($tc.quotefile,n,$2) != $null) { 
            write -dl $+ $2 $tc.quotefile 
            msg $chan $tc.col(1) $+ Deleted quote number $+ $tc.col(2) $2 $+ $tc.col(1) $+ !
          }
          else { msg $chan $tc.col(5) $+ Quote $+ $tc.col(2) $2 $tc.col(5) $+ was not found. }
        }        
      }
      else { msg $chan $tc.col(3) $+ Deletes a stored quote. $tc.col(1) $+ Usage: $+ $tc.col(2) !delquote Number }
    }

    if ($1 == !qspam) {    
      if (!$timer(0.tcflag.qspam $+ $remove($chan,$chr(35)))) {
        if ($2 isnum && $2 >= 5) {
          if ($readini($tc.cfgfile,n,Quote,split) == yes) {
            var %ctr = $tc.qscan($chan)
            if (%ctr == 0) { msg $chan $tc.col(5) $+ No quotes saved for this channel. | halt }
          }
          else { 
            if ($lines($tc.quotefile) <= 0) { msg $chan $tc.col(5) $+ No quotes saved in database. | halt } 
          }
          var %dur = $calc($2 * 60)
          .timer0.tcflag.qspam $+ $remove($chan,$chr(35)) 1 %dur tc.qspam %dur $chan 
          msg $chan $tc.col(1) $+ I will post a random quote every $+ $tc.col(2) $duration(%dur) $+ $tc.col(1) $+ . 
        }
        else { msg $chan $tc.col(3) $+ Makes the bot post a random quote every X minutes. $tc.col(1) $+ Usage: $+ $tc.col(2) !qspam Interval $tc.col(1) $+ (in minutes, minimum: 5 min) | halt }   
      }
      else {
        .timer0.tcflag.qspam $+ $remove($chan,$chr(35)) off
        msg $chan $tc.col(1) $+ Random quote timer has been disabled.
      }
    }

    if ($1 == !qtotal) {
      if ($readini($tc.cfgfile,n,Quote,split) == yes) {
        var %ctr = $tc.qscan($chan)
        msg $chan $tc.col(1) $+ There are $+ $tc.col(2) %ctr $tc.col(1) $+ total quotes stored for $+ $tc.col(2) $chan $tc.col(1) $+ and $+ $tc.col(2) $lines($tc.quotefile) $tc.col(1) $+ total quotes from all channels.
      } 
      else { 
        var %ctr = $lines($tc.quotefile) 
        msg $chan $tc.col(1) $+ There are $+ $tc.col(2) $lines($tc.quotefile) $tc.col(1) $+ total quotes stored in the bot.
      }
    }

    if ($1 == !qfind) {
      if ($2 != $null) {
        if ($len($2-) < 3) { msg # $tc.col(5) $+ http://i.imgur.com/y2Rs4nQ.gif - please refine your search criteria a bit (at least 3 letters). | halt }
        msg # $tc.col(1) $+ Searching for $+ $tc.col(2) $2- $tc.col(1) $+ within $+ $tc.col(2) $lines($tc.quotefile) $tc.col(1) $+ total stored quotes...
        if ($readini($tc.cfgfile,n,Quote,split) == yes) {
          var %ptr = 1, %ctr = 0 
          while (%ptr <= $lines($tc.quotefile)) {
            if ($gettok($read($tc.quotefile,n,%ptr),1,9) == $chan) { 
              inc %ctr 
              if ($2- isin $gettok($read($tc.quotefile,n,%ptr),4,9)) { 
                var %loc = %ctr, %floc = %ptr 
                var %qfind = $addtok(%qfind,%ctr,32) 
              } 
            }
            inc %ptr
          }
        } 
        else { 
          var %ptr = 1, %ctr = $lines($tc.quotefile)
          while (%ptr <= %ctr) {
            if ($2- isin $gettok($read($tc.quotefile,n,%ptr),4,9)) { 
              var %loc = %ptr, %floc = %ptr
              var %qfind = $addtok(%qfind,%ptr,32) 
            } 
            inc %ptr
          }
        }        
        if (%qfind != $null) { 
          msg # $tc.col(1) $+ Found in $+ $tc.col(2) $numtok(%qfind,32) $tc.col(1) $+ quotes!
          if ($numtok(%qfind,32) >= 200) { msg # $tc.col(5) $+ http://i.imgur.com/y2Rs4nQ.gif - way too many results, please refine your search criteria a bit. | halt }
          if ($numtok(%qfind,32) == 1) { msg # $tc.col(8) $+ Quote $+ $tc.col(2) %loc $+ $tc.col(8) $+ / $+ $tc.col(2) $+ %ctr $+ $tc.col(8) $+ : $+ $tc.col(1) $gettok($read($tc.quotefile,n,%floc),4,9) }
          else {
            var %p = 1, %qfindstr = $null
            while (%p <= $numtok(%qfind,32)) { 
              var %qfindstr = %qfindstr $+ $chr(32) $+ $tc.col(2) $+ $gettok(%qfind,%p,32) $+ $tc.col(1) $+ $chr(44)               
              inc %p
              if ($len(%qfindstr) > 400) { 
                if ($calc(%p - 1) == $numtok(%qfind,32)) { msg # $tc.col(1) $+ Quotes: $left(%qfindstr,$calc($len(%qfindstr) - 1)) $+ $chr(46) }
                else { msg # $tc.col(1) $+ Quotes: %qfindstr }
                var %qfindstr = $null
              }
            }         
            if (%qfindstr != $null) { msg # $tc.col(1) $+ Quotes: $left(%qfindstr,$calc($len(%qfindstr) - 1)) $+ $chr(46) }
          }

        }
        else { msg # $tc.col(5) $+ Search for $+ $tc.col(2) $2 $tc.col(5) $+ was not found in any quote stored. } 
      }
      else { msg # $tc.col(3) $+ Searches the bot for quotes containing a query. $tc.col(1) $+ Usage: $+ $tc.col(2) !qfind Query }
    }
    if ($1 == !qinfo) {
      if ($2 isnum && $2 > 0) {
        if ($readini($tc.cfgfile,n,Quote,split) == yes) {
          var %ptr = 1, %ctr = 0
          while (%ptr <= $lines($tc.quotefile)) {
            if ($gettok($read($tc.quotefile,n,%ptr),1,9) == $chan) { 
              inc %ctr 
              if (%ctr = $2) { var %floc = %ptr, %loc = %ctr } 
            }
            inc %ptr
          }
        } 
        else { var %floc = $2, %loc = %floc, %ctr = $lines($tc.quotefile) }
        if (%floc != $null) { 
          if ($gettok($read($tc.quotefile,n,%floc),3,9) != 0) { 
            var %qdate = $asctime($gettok($read($tc.quotefile,%floc),3,9),mmm dd yyyy)
            var %qtime = $asctime($gettok($read($tc.quotefile,%floc),3,9),H:nn tt) 
          }
          else { var %qdate = Unknown, %qtime = Unknown } 
          msg # $tc.col(1) $+ Quote $+ $tc.col(2) %loc $+ $tc.col(1) $+ / $+ $tc.col(2) $+ %ctr $tc.col(1) $+ was posted in $+ $tc.col(2) $gettok($read($tc.quotefile,%floc),1,9) $tc.col(1) $+ by $+ $tc.col(2) $gettok($read($tc.quotefile,%floc),2,9) $tc.col(1) $+ at $+ $tc.col(2) %qdate $tc.col(1) $+ @ $+ $tc.col(2) %qtime $+ $tc.col(1) $+ . (GMT/TC Time) 
        }
        else { msg # $tc.col(5) $+ Don't know anything about quote $+ $tc.col(2) $2 $+ $tc.col(5) $+ . }        
      }
      else { msg # $tc.col(3) $+ Displays additional information about a stored quote. $tc.col(1) $+ Usage: $+ $tc.col(2) !qinfo Number }
    }

    ; --------------------------------- drug tracker commands ---------------------------------

    if ($1-11 == After some time you feel no pleasure, but a huge headache.) { tc.xanod $nick # }

    if ($1-15 == You pop a few Xanax pills into your mouth and down a glass of water.) { 
      if ($16 == A) { tc.xanod $nick # | halt }    
      if ($16 == After) { tc.xanod $nick # | halt }
    }    

    ; --------------------------------- misc info tracking commands ---------------------------------

    if ($1-6 == Congratulations! You upgraded your level to) {
      var %lvl = $int($remove($7,$chr(33))), %id = $tc.getid($nick)
      if (%id != $null) {
        if (%lvl > 100 || %lvl <= 0) { msg # $tc.col(5) $+ Liar. | halt }
        if ($exists($tc.psetfile)) { var %lastlvl = $gettok($tc.getset(%id,lvl),1,32), %lasttime = $gettok($tc.getset(%id,lvl),2,32) }
        tc.set %id lvl %lvl $tc.time
        if (%lastlvl > %lvl && %lastlvl != $null) { msg # $tc.col(7) $+ Erm, how do you lose a level? Whichever, updated you reaching $+ $tc.col(2) lv $+ %lvl $+ $tc.col(7) $+ . }
        else { msg # $tc.col(6) $+ Congrats $+ $tc.col(2) $nick $+ $tc.col(6) $+ ! I have recorded you reaching $+ $tc.col(2) lv $+ %lvl $+ $tc.col(6) $+ ! } 
        if (%lasttime != $null) { msg # $tc.col(1) $+ You last recorded hitting $+ $tc.col(2) lv $+ %lastlvl $tc.col(1) $+ about $+ $tc.col(2) $duration($calc($tc.time - %lasttime)) $tc.col(1) $+ ago on $+ $tc.col(2) $asctime(%lasttime, mmm d yyyy) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%lasttime, h:nn:ss tt) $tc.col(1) $+ (GMT/TC Time). }
        else { msg # $tc.col(1) $+ You can see how long ago it was using $+ $tc.col(2) !lastlvl }
      }
    }

    if ($1 == !lastlvl) {
      if ($2 != $null) { var %name = $2 } | else { var %name = $nick }
      var %id = $tc.getid(%name)
      if (%id != $null) {
        if ($exists($tc.psetfile)) { var %lastlvl = $gettok($tc.getset(%id,lvl),1,32), %lasttime = $gettok($tc.getset(%id,lvl),2,32) }
        if (%lasttime != $null) { msg # $tc.col(2) $+ %name $+ $tc.col(1) $+ , you last recorded hitting $+ $tc.col(2) lv $+ %lastlvl $tc.col(1) $+ about $+ $tc.col(2) $duration($calc($tc.time - %lasttime)) $tc.col(1) $+ ago on $+ $tc.col(2) $asctime(%lasttime, mmm d yyyy) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%lasttime, h:nn:ss tt) $tc.col(1) $+ (GMT/TC Time). }
        else { msg # $tc.col(2) $+ %name $tc.col(5) $+ has not recorded a level up yet! }
      }
      else { msg # $tc.col(2) $+ %name $tc.col(5) $+ does not have an ID saved in the bot. }
    }

    if ($1 == !xanod || $1 == !lastod) {
      if ($2 != $null) { var %name = $2 } | else { var %name = $nick }
      var %id = $tc.getid(%name)
      if (%id != $null) {
        if ($exists($tc.psetfile)) { var %last = $tc.getset(%id,xanod) }
        if (%last != $null) { msg # $tc.col(2) $+ %name $tc.col(1) $+ has last overdosed on Xanax $+ $tc.col(2) $duration($calc($tc.time - %last)) $tc.col(1) $+ ago on $+ $tc.col(2) $asctime(%last, mmm d yyyy) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%last, h:nn:ss tt) $tc.col(1) $+ (GMT/TC Time). }
        else { msg # $tc.col(2) $+ %name $tc.col(5) $+ has not recorded an overdose yet! }
      }
      else { msg # $tc.col(2) $+ %name $tc.col(5) $+ does not have an ID saved in the bot. }
    } 

    if ($1 == !mcs) {
      var %id = $tc.getid($nick), %name = $tc.getname(%id)
      if (%id != $null) {
        if ($2 == r) {
          tc.set %id MCS $tc.time 
          msg # $tc.col(2) $+ %name $+ $tc.col(1) $+ , I have reset your MCS boost time!
        }
        else {
          var %mcs = $tc.getset(%id,mcs)
          if (%mcs != $null) { msg # $tc.col(1) $+ Your last MCS boost was on: $+ $tc.col(2) $asctime(%mcs,mmm dd) $tc.col(1) $+ @ $+ $tc.col(2) $asctime(%mcs,h:nn:ss tt) $tc.col(1) $+ TC time $tc.col(8) $+ ( $+ $duration($calc($tc.time - %mcs)) ago) $+ $tc.col(1) $+ . Use $+ $tc.col(2) !mcs r $tc.col(1) $+ to reset time. }         
          else { msg # $tc.col(2) $+ %name $+ $tc.col(5) $+ , you haven't recorded a MCS boost yet! To add one, use !mcs r }
        }
      }    
      else { msg # $tc.col(5) $+ Don't know who you are $+ $tc.col(2) $nick $+ $tc.col(5) $+ , need to add your ID to the bot before using the MCS tracker! }        
    }

    if (($1-3 == You have lost) && ($5-10 == of your drug addiction by rehabilitating)) {
      var %id = $tc.getid($nick), %name = $tc.getname(%id)
      if (%id != $null) {
        var %rep = $tc.cleanN($11)
        tc.set %id rehab $tc.time %rep
        msg # $tc.col(1) $+ Ouch $+ $tc.col(2) %name $+ $tc.col(1) $+ ! I have updated your latest rehab bill. $tc.col(8) $+ ( $+ %rep reps for $ $+ $tc.addcomma($calc(%rep * 250000)) $+ ) $tc.col(1) $+ - You can view your rehab records by using $+ $tc.col(2) 
        var %record = $tc.getset(%id,rehabrec)
        if (%record != $null) { 
          if ($gettok(%record,2,32) < %rep) { 
            msg # $tc.col(6) $+ You have also beat your record! $tc.col(1) $+ It was previously set on: $+ $tc.col(2) $asctime($gettok(%record,1,32),mmm dd) $tc.col(1) $+ @ $+ $tc.col(2) $asctime($gettok(%record,1,32),h:nn:ss tt) $tc.col(1) $+ TC time $tc.col(8) $+ ( $+ $duration($calc($tc.time - $gettok(%record,1,32))) ago) $tc.col(1) $+ for $gettok(%record,2,32) reps. It had cost you $+ $tc.col(2) $ $+ $tc.addcomma($calc($gettok(%record,2,32) * 250000)) $+ $tc.col(1) $+ !
            tc.set %id rehabrec $tc.time %rep
          }
        }
        else {
          msg # $tc.col(6) $+ I have also set your current record to this last bill.
          tc.set %id rehabrec $tc.time %rep
        }
      }
    }

    if ($1 == !rinfo || $1 == !rehab) {
      var %id = $tc.getid($nick), %name = $tc.getname(%id)
      if (%id != $null) {
        var %rehab = $tc.getset(%id,rehab)
        if (%rehab != $null) { 
          var %record = $tc.getset(%id,rehabrec)
          msg # $tc.col(2) $+ %name $+ $tc.col(1) $+ , your last rehab bill was on: $+ $tc.col(2) $asctime($gettok(%rehab,1,32),mmm d yyyy) $tc.col(1) $+ @ $+ $tc.col(2) $asctime($gettok(%rehab,1,32),h:nn:ss tt) $tc.col(1) $+ TC time $tc.col(8) $+ ( $+ $duration($calc($tc.time - $gettok(%rehab,1,32))) ago) $tc.col(1) $+ for $gettok(%rehab,2,32) reps. It cost you $+ $tc.col(2) $ $+ $tc.addcomma($calc($gettok(%rehab,2,32) * 250000)) $+ $tc.col(1) $+ .
          msg # $tc.col(1) $+ Your largest bill was on: $+ $tc.col(2) $asctime($gettok(%record,1,32),mmm d yyyy) $tc.col(1) $+ @ $+ $tc.col(2) $asctime($gettok(%record,1,32),h:nn:ss tt) $tc.col(1) $+ TC time $tc.col(8) $+ ( $+ $duration($calc($tc.time - $gettok(%record,1,32))) ago) $+ $tc.col(1) for $gettok(%record,2,32) reps. It cost you $+ $tc.col(2) $ $+ $tc.addcomma($calc($gettok(%record,2,32) * 250000)) $+ $tc.col(1) $+ !
        }         
        else { msg # $tc.col(2) $+ %name $+ $tc.col(5) $+ , you haven't recorded a rehab bill yet! }
      }        
      else { msg # $tc.col(5) $+ Don't know who you are $+ $tc.col(2) $nick $+ $tc.col(5) $+ , need to add your ID to the bot before using the MCS tracker! }        
    }

    if ($1 == !bdate) {
      if ($2 == $null) { var %month = $asctime($tc.time,mmm), %day = $asctime($tc.time,dd), %headerpost = $tc.col(1) $+ People with today's birthday: }
      else {
        if ($istok(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec,$left($2,3),32)) { var %month = $upper($left($2,1)) $+ $lower($mid($2,2,2)) }
        else { msg # $tc.col(5) $+ Not sure what month $+ $tc.col(2) $2 $tc.col(5) $+ is. | halt }      
        if ($3 isnum) { var %day = $3 } | else { msg # $tc.col(5) $+ Not sure what day $+ $tc.col(2) $3 $tc.col(5) $+ is. | halt }
        var %headerpost = $tc.col(1) $+ People with a birthday on $+ $tc.col(2) %month %day $+ $tc.col(1) $+ :
      }
      var %ptr = 1, %end = $ini($tc.psetfile,0)
      while (%ptr <= %end) {
        var %id = $ini($tc.psetfile,%ptr), %chk = $tc.getset(%id,bday)
        if ($gettok(%chk,1,32) == %month && $gettok(%chk,2,32) == %day) { var %bdaylist = $addtok(%bdaylist,$tc.getname(%id),32) }
        inc %ptr
      }
      if (%bdaylist != $null) {
        var %namelist = $tc.col(2) $gettok(%bdaylist,1,32)
        if ($numtok(%bdaylist,32) > 1) {
          var %ptr = 2, %end = $numtok(%bdaylist,32)        
          while (%ptr <= %end) {
            var %namelist = %namelist $+ $tc.col(1) $+ , $tc.col(2) $+ $gettok(%bdaylist,%ptr,32)
            inc %ptr
          }
        }
        msg # %headerpost | msg # %namelist
      }
      else { 
        if ($2 == $null) { msg # $tc.col(5) $+ It's no-one's birthday today. }
        else { msg # $tc.col(5) $+ It's no-one's birthday on $+ $tc.col(2) %month %day $+ $tc.col(5) $+ . }
      }
      msg # $tc.col(3) $+ To find someone's stored birthdate using their name as search criteria, use: $+ $tc.col(2) !bday Name 
    }

    if ($1 == !bday) { 
      if ($2 != $null) { var %id = $tc.getid($2) } 
      else { var %id = $tc.getid($nick) }
      if (%id == $null) { msg # $tc.col(5) $+ No ID saved for $+ $tc.col(2) $2 $+ $tc.col(5) $+ , so they couldn't have saved a birthday. | halt }
      var %name = $tc.getname(%id), %bday = $tc.getset(%id,bday)
      if (%bday == $null) { msg # $tc.col(5) $+ No birthdate saved for $+ $tc.col(2) %name $+ $tc.col(5) $+ . | halt }
      msg # $tc.col(2) $+ %name $+ $tc.col(1) $+ 's birthday is: $+ $tc.col(2) %bday
      msg # $tc.col(3) $+ To find someone's stored birthdate using a date as search criteria, use: $+ $tc.col(2) !bdate (Date) 
    } 

    ; --------------------------------- other stat paste commands ---------------------------------

    if ($1-2 = Attacks won:) {
      var %count = $int($remove($3,$chr(44)))
      if (%count > 1000000) { msg # $tc.col(5) $+ Liar. | halt }
      if (%count < 0) { msg # $tc.col(5) $+ I don't think so, $+ $tc.col(2) $nick $+ $tc.col(5) $+ . | halt }    
      if (%count >= 0 && %count < 5) { var %merit = 5, %mername = Woodland Camo, %medal = 50, %medname = Anti Social (1) }
      if (%count >= 5 && %count < 20) { var %merit = 20, %mername = Desert Camo, %medal = 50, %medname = Anti Social (1) }
      if (%count >= 20 && %count < 50) { var %merit = 50, %mername = Urban Camo, %medal = 50, %medname = Anti Social (1) } 
      if (%count >= 50 && %count < 100) { var %merit = 100, %mername = Arctic Camo, %medal = 250, %medname = Happy Slapper (2) }
      if (%count >= 100 && %count < 250) { var %merit = 250, %mername = Fall Camo, %medal = 250, %medname = Happy Slapper (2) }
      if (%count >= 250 && %count < 500) { var %merit = 500, %mername = Yellow Camo, %medal = 500, %medname = Scar Maker (3) }
      if (%count >= 500 && %count < 1000) { var %merit = 1000, %mername = Digital Camo, %medal = 2500, %medname = Going Postal (4) }
      if (%count >= 1000 && %count < 2000) { var %merit = 2000, %mername = Red Camo, %medal = 2500, %medname = Going Postal (4) }
      if (%count >= 2000 && %count < 2500) { var %merit = 3000, %mername = Blue Camo, %medal = 2500, %medname = Going Postal (4) }
      if (%count >= 2500 && %count < 3000) { var %merit = 3000, %mername = Blue Camo, %medal = 10000, %medname = Somebody Call 911 (5) }
      if (%count >= 3000 && %count < 4000) { var %merit = 4000, %mername = Orange Camo, %medal = 10000, %medname = Somebody Call 911 (5) }
      if (%count >= 4000 && %count < 5000) { var %merit = 5000, %mername = Pink Camo, %medal = 10000, %medname = Somebody Call 911 (5) }
      if (%count >= 5000 && %count < 10000) { var %medal = 10000, %medname = Somebody Call 911 (5) }
      if (%count >= 10000) { msg # $tc.col(2) $+ $nick $+ $tc.col(6) $+ , you have all the merits and medals for winning attacks! | halt }
      if (%merit == $null) { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , $tc.col(6) $+ you have all the merits $tc.col(1) $+ and are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%medal) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%medal - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %medname $tc.col(1) $+ medal! }
      else { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , $tc.col(1) $+ you are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%merit) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%merit - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %mername $tc.col(1) $+ merit and are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%medal) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%medal - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %medname $tc.col(1) $+ medal! }     
    }
    if ($1-2 = Defends won:) {
      var %count = $int($remove($3,$chr(44)))
      if (%count > 1000000) { msg # $tc.col(5) $+ Liar. | halt }
      if (%count < 0) { msg # $tc.col(5) $+ I don't think so, $+ $tc.col(2) $nick $+ $tc.col(5) $+ . | halt }    
      if (%count >= 0 && %count < 50) { var %merit = 50, %mername = Self Defence, %medal = 50, %medname = Bouncer (1) }      
      if (%count >= 50 && %count < 250) { var %medal = 250, %medname = Brick Wall (2) }      
      if (%count >= 250 && %count < 500) { var %medal = 500, %medname = Turtle (3) }
      if (%count >= 500 && %count < 2500) { var %medal = 2500, %medname = Solid as a Rock (4) }
      if (%count >= 2500 && %count < 10000) { var %medal = 10000, %medname = Fortress (5) }
      if (%count >= 10000) { msg # $tc.col(2) $+ $nick $+ $tc.col(6) $+ , you have all the merits and medals for winning defends! | halt }
      if (%merit == $null) { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , $tc.col(6) $+ you have all the merits $tc.col(1) $+ and are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%medal) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%medal - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %medname $tc.col(1) $+ medal! }
      else { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , $tc.col(1) $+ you are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%merit) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%merit - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %mername $tc.col(1) $+ merit and are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%medal) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%medal - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %medname $tc.col(1) $+ medal! }     
    }
    if ($1-2 = People busted:) {
      var %count = $int($remove($3,$chr(44)))
      if (%count > 1000000) { msg # $tc.col(5) $+ Liar. | halt }
      if (%count < 0) { msg # $tc.col(5) $+ I don't think so, $+ $tc.col(2) $nick $+ $tc.col(5) $+ . | halt }    
      if (%count >= 0 && %count < 250) { var %merit = 1000, %mername = Bar Buster, %medal = 250, %medname = Novice Buster (1) }      
      if (%count >= 250 && %count < 500) { var %merit = 1000, %mername = Bar Buster, %medal = 500, %medname = Intermediate Buster (2) }
      if (%count >= 500 && %count < 1000) { var %merit = 1000, %mername = Bar Buster, %medal = 1000, %medname = Advanced Buster (3) }
      if (%count >= 1000 && %count < 2000) { var %merit = 2500, %mername = Aiding and Abetting, %medal = 2000, %medname = Professional Buster (4) }
      if (%count >= 2000 && %count < 2500) { var %merit = 2500, %mername = Aiding and Abetting, %medal = 4000, %medname = Expert Buster (5) }
      if (%count >= 2500 && %count < 4000) { var %merit = 10000, %mername = Don't Drop It, %medal = 4000, %medname = Expert Buster (5) }
      if (%count >= 4000 && %count < 6000) { var %merit = 10000, %mername = Don't Drop It, %medal = 6000, %medname = Master Buster (6) }
      if (%count >= 6000 && %count < 8000) { var %merit = 10000, %mername = Don't Drop It, %medal = 8000, %medname = Guru Buster (7) }
      if (%count >= 8000 && %count < 10000) { var %merit = 10000, %mername = Don't Drop It }
      if (%count >= 10000) { msg # $tc.col(2) $+ $nick $+ $tc.col(6) $+ , you have all the merits and medals for busting! | halt }
      if (%medal == $null) { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , $tc.col(1) $+ you are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%merit) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%merit - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %mername $tc.col(1) $+ merit and $+ $tc.col(6) have all the medals! }     
      else { msg # $tc.col(2) $+ $nick $+ $tc.col(1) $+ , $tc.col(1) $+ you are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%merit) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%merit - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %mername $tc.col(1) $+ merit and are at $+ $tc.col(2) $tc.addComma(%count) $+ $tc.col(1) $+ / $+ $tc.col(2) $+ $tc.addComma(%medal) $tc.col(1) $+ ( $+ $tc.col(2) $+ $tc.addComma($calc(%medal - %count)) $tc.col(1) $+ to go) until the $+ $tc.col(2) %medname $tc.col(1) $+ medal! }     
    }   
    if ($left($1-,12) == Xanax taken:) {
      if ($mid($1-,13,1) == $chr(32)) { var %xan = $remove($3,$chr(44)) }
      else { var %xan = $remove($gettok($1-2,2,58),$chr(44)) }
      if (%xan > 20000) { msg # $tc.col(5) $+ Liar. | halt }
      if (%xan isnum) { msg # $tc.col(1) $+ Using an average base price of $+ $tc.col(2) $ $+ 740,000 $tc.col(1) $+ each (South Africa), you have used $+ $tc.col(2) $ $+ $tc.addComma($calc(%xan * 740000)) $tc.col(1) $+ worth of Xanax. } 
    }

    ; --------------------------------- other misc commands ---------------------------------

    if (https://www.youtube.com/ isin $1 || http://www.youtube.com/ isin $1 || http://youtu.be/ isin $1) {
      set %tc.ytchan #
      if (https://www.youtube.com/ isin $1) { set %tc.ytlink $remove($1,https://www.youtube.com) }
      if (http://www.youtube.com/ isin $1) { set %tc.ytlink $remove($1,http://www.youtube.com) }
      if (http://youtu.be/ isin $1) { set %tc.ytlink /watch?v= $+ $gettok($remove($1,http://youtu.be/),1,63) }
      if (%tc.ytlink != $null) {
        sockclose youtube 
        sockopen -e youtube www.youtube.com 443 
      } 
      else { unset %tc.yt* }
    }

    if ($1 == !events) { msg # $tc.col(1) $+ Events Page: $+ $tc.col(2) http://www.torn.com/events.php }

    if ($1 == !market) { 
      if ($2 != $null) { 
        if ($2 == dp) { var %query = donator pack }
        elseif ($2 == db) { var %query = dirty bomb }
        elseif ($2 == fhc) { var %query = feathery hotel coupon }
        elseif ($2 == xan) { var %query = xanax }
        elseif ($2 == xtc) { var %query = ecstasy }
        elseif ($2 == bct) { var %query = business class ticket }
        elseif ($2 == sed) { var %query = small explosive device }
        elseif ($2 == bom) { var %query = box of medical supplies }
        elseif ($2 == lbc) { var %query = lawyer business coupon }
        elseif ($2 == dbk) { var %query = diamond bladed knife }
        else { var %query = $2- }
        msg # $tc.col(1) $+ Market search for $+ $tc.col(2) $2- $+ $tc.col(1) $+ : $+ $tc.col(2) http://www.torn.com/imarket.php#/p=shop&step=shop&type=&searchname= $+ $tc.uencode(%query)
      }
      else { msg # $tc.col(3) $+ Displays a market search link for a particular query. $tc.col(1) $+ Usage: $+ $tc.col(2) !market query }    
    }
    if ($1 == !donate) { msg # $tc.col(1) $+ Donate to the faction: $+ $tc.col(2) http://www.torn.com/factions.php?step=your&action=armoury&tabID=8#/tab=armoury }
    if ($1 == !time || $1 == !refill) { msg # $tc.col(1) $+ Current GMT/TC time is: $+ $tc.col(2) $asctime($tc.time,mmm d yyyy) $tc.col(1) $+ at $+ $tc.col(2) $asctime($tc.time, h:nn:ss tt) $+ $tc.col(1) $+ . Energy refill resets in: $+ $tc.col(2) $duration($calc($ctime($asctime($calc($tc.time + 86400),dd/mm/yy) 0:05) - $tc.time)) $+ $tc.col(1) $+ $chr(46) }
    if ($1 == !calc) { 
      if ($2 != $null) { 
        ; easier to read if in a var
        if ($chr(37) isin $2-) { msg # $tc.col(5) $+ What you tryin to peep at foo? | halt }
        if (/0 isin $2-) { msg # $tc.col(5) $+ NO! DON'T DO IT! DON'T DO I ...... $tc.col(1) $+ b $+ $tc.col(2) $+ z $+ $tc.col(3) $+ z $+ $tc.col(4) $+ z $+ $tc.col(5) $+ z $+ $tc.col(6) $+ z $+ $tc.col(7) $+ z $+ $tc.col(8) $+ t | halt }
        ; old
        ;var %calc = $calc($replace($remove($2-, $chr(44)),x,*,k,000,mil,000000,bil,000000000,m,000000,b,000000000,�,(3/4),�,(1/4),�,^2,�,(1/2)))
        ; new. Thanks to Ausseh
        var %calc = $calc($regsubex($ticks,$remove($2-,$chr(44),$chr(32),il),/(^|[+-/*^%]+|[+-/^*%]+\50|\50+|\50)(([0-9]|\56)+)(k|m|b|t|q)/Sig,\1 $+ ( $+ \2 $+ $iif(\4 == q,*1000000000000000,$iif(\4 == t,*1000000000000,$iif(\4 == b,*1000000000,$iif(\4 == m,*1000000,*1000)))) $+ )))
        msg # $tc.col(1) $+ Calc: $+ $tc.col(2) $2- $tc.col(1) $+ = $+ $tc.col(2) $tc.addcomma(%calc)
      }
      else { msg # $tc.col(3) $+ Displays the result from a simple calculation. $tc.col(1) $+ Usage: $+ $tc.col(2) !calc formula $+ $tc.col(1) $+ , ie: !calc 2+2 }
    }
    if ($1 == !topic) { msg # $tc.col(1) $+ Topic: $+ $tc.col(2) $chan(#).topic }
    if ($1 == !google) { 
      if ($2 == $null) { msg # $tc.col(3) $+ Displays a google link for a query. $tc.col(1) $+ Usage: $+ $tc.col(2) !google Query }
      else { msg # $tc.col(1) $+ Google: $+ $tc.col(2) http://www.google.com/search?as_q= $+ $tc.uencode($2-) }
    }
    if ($1 == !help) { msg # $tc.col(1) $+ Help site: $+ $tc.col(2) http://www.tornstats.com/tornbot/uta9.html }
    if ($1 == !botinfo) { msg # $tc.col(1) $+ Universal TC Assistant made by: $+ $tc.col(2) PMV $tc.colid(1577993,1,2) v $+ $tc.col(2) $+ $tc.botvers }
  }
}

; --------------------------------- upgrade script ---------------------------------

; Pre-init upgrades.
; This called before anything at all is initialized if hard coded bot version is different then what's stored in the files.
; Note: If upgrading from an earlier version other then 6.71, upgrade to 6.71 first, then continue from there.
alias -l tc.upgrade {
  var %oldvers = $readini($tc.cfgfile,n,File,vers)

  ; v6.71 upgrade check
  if (%oldvers == 6.71) { tc.v7upg }

  ; upgrades since v7.0
  if ($readini($tc.cfgfile,n,Cron,decay) == $null) { writeini -n $tc.cfgfile Cron decay $tc.time }
  if ($readini($tc.cfgfile,n,Config,odrevive) == $null) { writeini -n $tc.cfgfile Config odrevive yes }
  if ($readini($tc.cfgfile,n,Config,statsaving) == $null) { writeini -n $tc.cfgfile Config statsaving e }
  if ($readini($tc.cfgfile,n,Banker,enable) == $null) { writeini -n $tc.cfgfile Banker enable e }
  if ($readini($tc.cfgfile,n,API,enable) == $null) { writeini -n $tc.cfgfile API enable d }

  ; mark version of setting files to current version and post success in status.
  writeini -n $tc.cfgfile File vers $tc.botvers
  echo -ag 03Thank you for using my script - script data has been upgraded from %oldvers to $tc.botvers
}

alias tcoption { tc.cfg }
alias tcoptions { tc.cfg }

; --------------------------------- initial file generators and setup script ---------------------------------

alias tcsetup {
  if ($exists($tc.cfgfile)) { echo -ag Config file already exists! Right click the channel or status window and click Config to modify options. | halt }
  var %tcsetupchan = $input(REQUIRED: Add the initial channels that you want the bot to function in $+ $crlf $+ Seperate them with commas if you want more then one,e,TC Bot Setup)
  if (%tcsetupchan == $null) { echo -ag Setup halted! Need a channel, type /tcsetup to try again. | halt }
  var %tcsetupchain = $input(Enter chaining-enabled channel(s). $+ $crlf $+ If you wish to use the same channels as the general bot then leave blank.,e,TC Bot Setup)
  if (%tcsetupchain == $null) { var %tcsetupchain = %tcsetupchan }
  var %tcsetupaddrevchan = $input(Do you wish to use a revive channel?,y,TC Bot Setup)
  if (%tcsetupaddrevchan == $true) { 
    var %tcsetuprevive = $input(Enter revive channel(s) seperated by comma. $+ $crlf $+ If you wish to use the same channels as the general bot then leave blank.,e,TC Bot Setup)
    if (%tcsetuprevive == $null) { var %tcsetuprevive = %tcsetupchan }
    else { var %tcsetuprevive = $null }
  }
  var %tcsetupfact = $input(REQUIRED: Type the name of your faction exactly how it is shown in game,e,TC Bot Setup)
  if (%tcsetupfact == $null) { echo -ag Setup halted! Need faction name, type /tcsetup to try again. | halt }
  var %tcsetupchup = $input(Faction chaining upgrade? $+ $crlf $+ Use numbers. ie: II = 2 $+ $crlf $+ Leaving blank will use 0,e,TC Bot Setup)
  if (%tcsetupchup == $null) { var %tcsetupchup = 0 }
  echo -ag Thank you, writing default values for the rest. You can change them in the control panel afterwards.
  writeini -n $tc.cfgfile Config faction %tcsetupfact
  writeini -n $tc.cfgfile Config chan %tcsetupchan   
  writeini -n $tc.cfgfile Chain chan %tcsetupchain
  if (%tcsetuprevive != $null) { writeini -n $tc.cfgfile Chain revive %tcsetuprevive }
  writeini -n $tc.cfgfile Chain upg %tcsetupchup
  writeini -n $tc.cfgfile Chain resp 0
  writeini -n $tc.cfgfile Chain chid 0  
  writeini -n $tc.cfgfile Config ereset yes 
  writeini -n $tc.cfgfile Config bstats no
  writeini -n $tc.cfgfile Config findadd no
  writeini -n $tc.cfgfile Config spyen yes
  writeini -n $tc.cfgfile Config hermetic 0
  writeini -h $tc.cfgfile Chain revnote no
  writeini -n $tc.cfgfile Chain postchains yes
  writeini -n $tc.cfgfile Chain warresp 0
  writeini -n $tc.cfgfile Access min o
  writeini -n $tc.cfgfile Access delid 1
  writeini -n $tc.cfgfile Access chgid 1
  writeini -n $tc.cfgfile Access delspy 1
  writeini -n $tc.cfgfile Access delquote 1
  writeini -n $tc.cfgfile Access wartally 1
  writeini -n $tc.cfgfile Access setbday 1
  writeini -n $tc.cfgfile Quote split no
  writeini -n $tc.cfgfile Color color y
  writeini -n $tc.cfgfile Color pri 10
  writeini -n $tc.cfgfile Color sec 7
  writeini -n $tc.cfgfile Color help 3
  writeini -n $tc.cfgfile Color note 13
  writeini -n $tc.cfgfile Color error 4
  writeini -n $tc.cfgfile Color good 3
  writeini -n $tc.cfgfile Color bad 4
  writeini -n $tc.cfgfile Color subnote 6
  writeini -n $tc.cfgfile File vers $tc.botvers
  flushini $tc.cfgfile
  hfree -w tc.*
  echo -ag Config file generated and flushed to disk.
  echo -ag ...  
  echo -ag -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  echo -ag Setup Complete! The bot should be good to go, right click on status or channel window to view/change config values from now on. Or, type: /tcoptions
  echo -ag One more quick thing that you should do: Right click your chat ID in the nick list, go to bot menu and add to admin. This will add your ip to the admin list. It's not required but you won't be able to access admin-only commands without doing so.
  echo -ag Also please note: the bot will not do nickserv identify or joining channels, use perform within mirc for that.
  echo -ag -=-
  echo -ag Thank you for using my bot. :)
  echo -ag I want to give a big thanks to Rayne [1868280], Stewie [1494547], handsomepants [1897243], forgey [1640970], sinisista [1615339], HLH [1378809], IceBlueFire [776], Ausseh [1554731], and Stretch [1846075] for their help in making/testing this.
  echo -ag All I ask is that you keep my name as the original author or at least give me credit if you modify/copy my code.
  echo -ag If you run into any bugs or have a suggestion to add to a future release, feel free to mail PMV [1577993] and I'll see what I can do. 
  echo -ag Also, if you end up liking it and wish to donate some TC supplies as a token of thanks, I won't say no either. :) 
  echo -ag -=-
  echo -ag The script will try to re-initialize now, see status window for success/error messages.
  echo -ag -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  tc.init
}

; v6.71 to v7.0 upgrade script
alias -l tc.v7upg {
  echo -ag 07Running v6.71 to v7.x upgrade script. 
  ; ID converter
  if ($exists($qt($scriptdir $+ id.txt)) && !$exists($tc.idsfile)) {
    echo -ag 04Old pre-v7 ID file detected at $scriptdir $+ id.txt and no hash table file exists. 
    echo -ag This script now uses an internal hash table, running conversion script.
    var %infile = $qt($scriptdir $+ id.txt), %eof = $lines(%infile), %pos = 1
    echo -ag Reading %eof IDs in id.txt and adding to hash table...
    while (%pos <= %eof) {
      var %read = $read($qt(%infile), %pos), %name = $gettok(%read,1,32), %id = $gettok(%read,2,32)
      hadd -m convids %name %id
      inc %pos
    }
    echo -ag Saving new table and removing old files... (They should end up in the recycle bin)
    .remove -b $qt($scriptdir $+ id.txt)
    if ($exists($qt($scriptdir $+ tempid.txt))) { .remove -b $qt($scriptdir $+ tempid.txt) }
    hsave convids $tc.idsfile
    hfree convids
    echo -ah ID conversion was successful
  }  
  ; chain id and wartally reset
  echo -ag 04v6.71 to v7 upgrade - resetting chain/wartally markers since the core has been changed. 
  echo -ag Also, chain data is now stored in $tc.chlogdir so the old chains\ folder can be removed/archived.
  if ($readini($tc.cfgfile,n,Chain,id) != $null) { remini  $tc.cfgfile Chain id }
  if ($readini($tc.cfgfile,n,Chain,wartally) != $null) { writeini -n $tc.cfgfile Chain wartally 1 }
  echo -ag 03Script data upgraded to v7.0 - Continuing with script update to current version.
}
