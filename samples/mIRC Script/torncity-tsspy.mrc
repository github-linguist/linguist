ON 1:START:{ unset %tornstats* | unset %tsspy.* | tornstats.init }

alias -l tornstats.cfgfile { return $qt($scriptdir $+ tornstats.ini) }
alias -l tornstats.localfile { return $qt($scriptdir $+ spy.db) }
alias -l tornstats.botvers { return 3.0 }

alias -l tornstats.init {
  if (!$exists($tornstats.cfgfile)) { tornstats.upg | echo -ag www.tornstats.com Spy Database script: Default values have been written. Right click channel section for config panel. }
  if ($readini($tornstats.cfgfile,n,Spy,vers) != $tornstats.botvers) { tornstats.upg | echo -ag www.tornstats.com Spy Database script: Thank you for updating, new config entries have been populated with default values. }
  set %tornstats.chanlist $addtok(%tornstats.chanlist,$readini($tornstats.cfgfile,n,Spy,chan),44)
  echo -sg 10tornstats.com Spy Database script made by:7 PMV 10[07157799310] v07 $+ $tornstats.botvers 10- Initialized Successfully!    
}


; ##################################################################### CONFIG CODE #####################################################################


dialog -l tornstats.cfgview {
  title "www.tornstats.com Spy Database Config"
  size -1 -1 260 125
  option dbu
  check "Enable Script (warning: Disable the spy bot on Universal TC Bot if using this script!)",1,4 4 220 10
  text "Channel(s):",1001,4 18 40 10, right
  text "Tornstats Key:",1002,4 32 40 10, right
  text "API Key:",1003,4 44 40 10, right
  edit "",100,50 16 150 10, autohs
  edit "",101,50 30 150 10, autohs
  edit "",102,50 42 150 10, autohs
  button "Set",10,220 20 25 25
  check "Cache Saved Spies Locally (works with Asshole v5.0 or higher/Universal TC Bot)",2,4 56 200 10
  check "Display Socket Errors",3,4 68 200 10
  text "",999,4 115 252 10, center
}

on 1:DIALOG:tornstats.cfgview:sclick:1:{
  if ($readini($tornstats.cfgfile,n,Spy,enable) == no) { writeini -n $tornstats.cfgfile Spy enable yes | did -c tornstats.cfgview 1 | did -a tornstats.cfgview 999 Script is now ENABLED. }
  else { writeini -n $tornstats.cfgfile Spy enable no | did -u tornstats.cfgview 1 | did -a tornstats.cfgview 999 Script is now DISABLED. }
  flushini $tornstats.cfgfile
}

on 1:DIALOG:tornstats.cfgview:sclick:2:{
  if ($readini($tornstats.cfgfile,n,Spy,cache) == no) { writeini -n $tornstats.cfgfile Spy cache yes | did -c tornstats.cfgview 2 | did -a tornstats.cfgview 999 Script will now save spy updates locally as well. }
  else { writeini -n $tornstats.cfgfile Spy cache no | did -u tornstats.cfgview 2 | did -a tornstats.cfgview 999 Script will NOT save updates locally. }
  flushini $tornstats.cfgfile
}

on 1:DIALOG:tornstats.cfgview:sclick:3:{
  if ($readini($tornstats.cfgfile,n,Spy,sockerr) == no) { writeini -n $tornstats.cfgfile Spy sockerr yes | did -c tornstats.cfgview 3 | did -a tornstats.cfgview 999 Script will now display socket errors if any. }
  else { writeini -n $tornstats.cfgfile Spy sockerr no | did -u tornstats.cfgview 3 | did -a tornstats.cfgview 999 Script will hide socket errors. }
  flushini $tornstats.cfgfile
}

on 1:DIALOG:tornstats.cfgview:sclick:10:{
  if ($did(tornstats.cfgview,100).text != $null) { writeini -n $tornstats.cfgfile Spy chan $remove($did(tornstats.cfgview,100).text,$chr(32)) }
  else { if ($readini($tornstats.cfgfile,n,Spy,chan) != $null) { remini $tornstats.cfgfile Spy chan } }
  if ($did(tornstats.cfgview,101).text != $null) { writeini -n $tornstats.cfgfile Spy key $did(tornstats.cfgview,101).text }
  else { if ($readini($tornstats.cfgfile,n,Spy,key) != $null) { remini $tornstats.cfgfile Spy key } }
  if ($did(tornstats.cfgview,102).text != $null) { writeini -n $tornstats.cfgfile Spy api $did(tornstats.cfgview,102).text }
  else { if ($readini($tornstats.cfgfile,n,Spy,api) != $null) { remini $tornstats.cfgfile Spy api } }
  set %tornstats.chanlist $readini($tornstats.cfgfile,n,Spy,chan)
  did -ra tornstats.cfgview 999 Channel list and both keys saved.
  did -ra tornstats.cfgview 100 $readini($tornstats.cfgfile,n,Spy,chan)
  did -ra tornstats.cfgview 101 $readini($tornstats.cfgfile,n,Spy,key)
  did -ra tornstats.cfgview 102 $readini($tornstats.cfgfile,n,Spy,api)
  flushini $tornstats.cfgfile
}

menu status,channel {
  www.tornstats.com Spy Database v $+ $tornstats.botvers Config:tornstats.cfg
}

alias -l tornstats.cfg {
  if ($dialog(tornstats.cfgview)) { halt } | dialog -m tornstats.cfgview tornstats.cfgview
  if ($readini($tornstats.cfgfile,n,Spy,enable) == yes) { did -c tornstats.cfgview 1 } | else { did -u tornstats.cfgview 1 }
  if ($readini($tornstats.cfgfile,n,Spy,cache) == yes) { did -c tornstats.cfgview 2 } | else { did -u tornstats.cfgview 2 }
  if ($readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { did -c tornstats.cfgview 3 } | else { did -u tornstats.cfgview 3 }
  did -ra tornstats.cfgview 100 $readini($tornstats.cfgfile,n,Spy,chan)
  did -ra tornstats.cfgview 101 $readini($tornstats.cfgfile,n,Spy,key)
  did -ra tornstats.cfgview 102 $readini($tornstats.cfgfile,n,Spy,api)
}

alias -l tornstats.cfgcheck { if (!$exists($tornstats.cfgfile)) { tornstats.upg | echo -ag www.tornstats.com Spy Database script: Default values have been written. Right click channel section for config panel. } }


; ##################################################################### SOCKET CODE #####################################################################



on *:sockopen:getinfo:{
  if ($sockerr) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    if (%tsspy.oldspy != $null) { var %table = tsspy. $+ %tsspy.oldspy }
    else { var %table = tsspy. $+ %tsspy.spy }
    sockwrite -nt $sockname GET /user/ $+ $hget(%table,id) $+ ?selections=profile&key= $+ $readini($tornstats.cfgfile,n,Spy,api) HTTP/1.1
    sockwrite -nt $sockname Host: api.torn.com
    sockwrite -nt $sockname $crlf
  }
}

on *:sockread:getinfo:{
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    var %read | sockRead %read | var %readln $remove(%read, $chr(9))  
    ;write $qt($scriptdirtest.htm) %readln
    if (%tsspy.oldspy != $null) { var %table = tsspy. $+ %tsspy.oldspy }
    else { var %table = tsspy. $+ %tsspy.spy }
    if ($regex(%readln,error":"(.+?)")) { 
      msg %tsspy.postchan 04API Error: $regml(1)
      tornstats.spywrite
    }
    if ($regex(%readln,level":(.+?)")) { hadd %table lvl $tornstats.cleanN($regml(1)) }  
    if ($regex(%readln,faction_name":"(.+?)")) { hadd %table fact $tornstats.cleanW($regml(1))) }
    if (($hget(%table,lvl) != $null) && ($hget(%table,fact) != $null)) { tornstats.spywrite }
  }
}


on *:SOCKOPEN:spysave: {
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    if (%tsspy.oldspy != $null) { var %table = tsspy. $+ %tsspy.oldspy }
    else { var %table = tsspy. $+ %tsspy.spy }
    var %a = /tornbot/someelusivebotscript.php?action=spy&step=save
    var %a = %a $+ &key= $+ $tornstats.uencode($readini($tornstats.cfgfile,n,Spy,key))
    var %a = %a $+ &name= $+ $tornstats.uencode($hget(%table,name))    
    var %a = %a $+ &id= $+ $tornstats.uencode($hget(%table,id))
    var %a = %a $+ &lvl= $+ $tornstats.uencode($hget(%table,lvl))
    var %a = %a $+ &faction= $+ $tornstats.uencode($hget(%table,fact))
    var %a = %a $+ &str= $+ $tornstats.uencode($hget(%table,str))
    var %a = %a $+ &def= $+ $tornstats.uencode($hget(%table,def))
    var %a = %a $+ &spd= $+ $tornstats.uencode($hget(%table,spd))
    var %a = %a $+ &dex= $+ $tornstats.uencode($hget(%table,dex))
    var %a = %a $+ &tot= $+ $tornstats.uencode($hget(%table,totb))
    sockwrite -nt $sockname GET %a HTTP/1.1
    sockwrite -nt $sockname User-Agent: Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)
    sockwrite -nt $sockname Host: www.tornstats.com
    sockwrite -nt $sockname Accept-Language: en-us
    sockwrite -nt $sockname Accept: */*
    sockwrite -nt $sockname $crlf
  }
}

on *:SOCKREAD:spysave: {
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    :nextread
    sockread -f %tsspy.str
    ;write $qt($scriptdirtest $+ $gmt $+ .htm) %tsspy.str
    if ($sockbr == 0) { return }
    if (%tsspy.str == $null) { %tsspy.str = - }
    if (<p> isin %tsspy.str) {
      set %tsspy.response $remove(%tsspy.str,<p>,</p>)
      tornstats.savetosite
      sockclose spyget
    }
    else { goto nextread }
  }
}

on *:SOCKCLOSE:spysave:{ tornstats.savetosite }

alias -l tornstats.savetosite {
  if (%tsspy.postchan != $null) {
    if (%tsspy.response != $null) { msg %tsspy.postchan 10 $+ %tsspy.response }
    else { msg %tsspy.postchan 04Oops! The server did not send a valid response, please check your settings! }
    if (%tsspy.oldspy != $null) { var %table = tsspy. $+ %tsspy.oldspy }
    else { var %table = tsspy. $+ %tsspy.spy }
    hfree %table
    .timer0.tornstats.spyauto off
    unset %tsspy.* | unset %tsprofsw.*
  }
}

on *:SOCKOPEN:spyget: {
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    var %a = /tornbot/someelusivebotscript.php?action=spy&step=get
    var %a = %a $+ &key= $+ $tornstats.uencode($readini($tornstats.cfgfile,n,Spy,key))
    if ($gettok(%tsspy.search,1,32) == i) { var %a = %a $+ &id= $+ $tornstats.uencode($gettok(%tsspy.search,2,32)) }
    else { var %a = %a $+ &name= $+ $tornstats.uencode($gettok(%tsspy.search,2,32)) }
    sockwrite -nt $sockname GET %a HTTP/1.1
    sockwrite -nt $sockname User-Agent: Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)
    sockwrite -nt $sockname Host: www.tornstats.com
    sockwrite -nt $sockname Accept-Language: en-us
    sockwrite -nt $sockname Accept: */*
    sockwrite -nt $sockname $crlf
  }
}

on *:SOCKREAD:spyget: {
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    :nextread
    sockread -f %tsspy.read
    ;write $qt($scriptdirtest $+ $gmt $+ .htm) %tsspy.read
    if ($sockbr == 0) { return }
    if (%tsspy.read == $null) { %tsspy.read = - }
    if (Name: isin %tsspy.read) { set %tsspy.name $gettok(%tsspy.read,2,32) }
    if (ID: isin %tsspy.read) { set %tsspy.id $gettok(%tsspy.read,2,32) }
    if (Level: isin %tsspy.read) {
      set %tsspy.lvl $gettok(%tsspy.read,2,32)
      if ($calc(%tsspy.lvl) == 0 || %tsspy.lvl == $null) { set %tsspy.lvl 04N/A }
    }
    if (Faction: isin %tsspy.read) {
      set %tsspy.fac $remove(%tsspy.read,Faction:)
      if (%tsspy.fac == 0 || %tsspy.fac == $null || %tsspy.fac == $chr(32)) { set %tsspy.fac 04N/A }
    }
    if (Strength: isin %tsspy.read) {
      set %tsspy.str $gettok(%tsspy.read,2,32)
      if ($calc(%tsspy.str) == 0 || %tsspy.str == $null) { set %tsspy.str 04N/A }
    }
    if (Defense: isin %tsspy.read) {
      set %tsspy.def $gettok(%tsspy.read,2,32)
      if ($calc(%tsspy.def) == 0 || %tsspy.def == $null) { set %tsspy.def 04N/A }
    }
    if (Speed: isin %tsspy.read) {
      set %tsspy.spd $gettok(%tsspy.read,2,32)
      if ($calc(%tsspy.spd) == 0 || %tsspy.spd == $null) { set %tsspy.spd 04N/A }
    }
    if (Dexterity: isin %tsspy.read) {
      set %tsspy.dex $gettok(%tsspy.read,2,32)
      if ($calc(%tsspy.dex) == 0 || %tsspy.dex == $null) { set %tsspy.dex 04N/A }
    }
    if (Total: isin %tsspy.read) {
      set %tsspy.tot $gettok(%tsspy.read,2,32)
      if ($calc(%tsspy.tot) == 0 || %tsspy.tot == $null) { set %tsspy.tot 04N/A }
    }
    if (Updated: isin %tsspy.read) { set %tsspy.upd $gettok($remove(%tsspy.read,</p>),2,32) }
    if (Error: isin %tsspy.read) { set %tsspy.err $deltok($remove(%tsspy.read,</p>,<p>),1,32) }
    if (</p> isin %tsspy.read) {
      tornstats.displayspy
      sockclose spyget
    }
    else { goto nextread }
  }
}

on *:SOCKCLOSE:spyget:{ tornstats.displayspy }

alias -l tornstats.displayspy {
  if (%tsspy.name != $null) {
    var %age = $calc($gmt - %tsspy.upd)
    if (%age >= 2592000) { var %oldtag = 04[OLD] }
    else { var %oldtag = $null }
    msg %tsspy.postchan 10Spy Entry for07 %tsspy.name 10[07 $+ %tsspy.id $+ 10] (Lv:07 %tsspy.lvl 10- Fact:07 %tsspy.fac $+ 10) - Added on:07 $asctime(%tsspy.upd,mmm d) 10@07 $asctime(%tsspy.upd,h:nn:ss tt) 06( $+ $duration(%age) ago) %oldtag
    msg %tsspy.postchan 10Strength:07 $tornstats.addComma(%tsspy.str)
    msg %tsspy.postchan 10Defense:07 $tornstats.addComma(%tsspy.def)
    msg %tsspy.postchan 10Speed:07 $tornstats.addComma(%tsspy.spd)
    msg %tsspy.postchan 10Dexterity:07 $tornstats.addComma(%tsspy.dex)
    msg %tsspy.postchan 10Total:07 $tornstats.addComma(%tsspy.tot)
  }
  else {
    if (%tsspy.err != $null) { msg %tsspy.postchan 04 $+ %tsspy.err }
    else { msg %tsspy.postchan 04Oops! A problem has occurred, check your settings! }
  }
  unset %tsspy.*
}

on *:SOCKOPEN:spyfind: {
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    var %a = /tornbot/someelusivebotscript.php?action=spy&step=search $+ %tsspy.find
    var %a = %a $+ &key= $+ $tornstats.uencode($readini($tornstats.cfgfile,n,Spy,key))
    sockwrite -nt $sockname GET %a HTTP/1.1
    sockwrite -nt $sockname User-Agent: Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)
    sockwrite -nt $sockname Host: www.tornstats.com
    sockwrite -nt $sockname Accept-Language: en-us
    sockwrite -nt $sockname Accept: */*
    sockwrite -nt $sockname $crlf
  }
}

on *:SOCKREAD:spyfind: {
  if ($sockerr && $readini($tornstats.cfgfile,n,Spy,sockerr) == yes) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr }
  else {
    :nextread
    sockread -f %tsspy.read
    ;write $qt($scriptdirtest $+ $gmt $+ .htm) %tsspy.read
    if ($sockbr == 0) { return }
    if (%tsspy.read == $null) { %tsspy.read = - }  
    if (Found: isin %tsspy.read) { tornstats.spypost $gettok(%tsspy.read,2,32) $gettok(%tsspy.read,3,32) }
    if (</p> isin %tsspy.read) {
      tornstats.spysearchres
      sockclose spyget
    }
    else { goto nextread }
  }
}

on *:SOCKCLOSE:spyfind:{ tornstats.spysearchres }

alias -l tornstats.spysearchres {
  if (%tsspy.postchan != $null) {
    if (%tsspy.post != $null) { msg %tsspy.postchan $left(%tsspy.post, $calc($len(%tsspy.post) - 1)) $+ $chr(46) }
    if (%tsspy.found != $null) { msg %tsspy.postchan 10Total:7 %tsspy.found 10spies! }
    else { msg %tsspy.postchan 04There were no spies found for that query. }
    unset %tsspy.*
  }
}


; ##################################################################### ALIAS CODE #####################################################################


alias -l tornstats.addComma { var %a, %b = $regsub($ticks,$1,/\G([+-]?\d+?)(?=(?:\d{3})++(?=\.\d++$|$))/g,\1 $+ $chr(44),%a) | return %a }

; strips all input ascii chars aside from decimals and numbers. Used to clean accidental/intentional letters in inputs that expect numbers.
alias -l tornstats.cleanN {
  var %a = 0 | var %b = $1
  while (%a < 255) { if (%a != 46 && %a !isnum 48-57) { var %b = $remove(%b, $chr(%a)) } | inc %a }
  return %b
}

; strips leading and trailing whitespace
alias -l tornstats.cleanW { var %a, %b = $regsub($ticks,$1,^[ \t]+|[ \t]+$,$null,%a) | return %a }  

alias -l tornstats.uencode {
  var %a = $replace($1-,$chr(37),$+($chr(37),25)), %ptr = 32
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

alias -l tornstats.spywrite {
  sockclose getinfo
  if ($timer(0.tornstats.spyscrape)) { .timer $+ 0.tornstats.spyscrape off }
  if (%tsspy.oldspy != $null) { var %table = tsspy. $+ %tsspy.oldspy }
  else { var %table = tsspy. $+ %tsspy.spy }
  if ($hget(%table,id) != $null) {
    msg %tsspy.postchan 10Saving spy entry for07 $hget(%table,name) 10[07 $+ $hget(%table,id) $+ 10] to tornstats.com...
    if (!$hfind(%table,fact) || $hget(%table,fact) == $null) { hadd -m %table fact N/A }
    if (!$hfind(%table,lvl) || $hget(%table,lvl) !isnum) { hadd -m %table lvl 0 }
    if ($readini($tornstats.cfgfile,n,Spy,cache) == yes) {
      var %savestring = $hget(%table,name) $+ $chr(9) $+ $hget(%table,id) $+ $chr(9) $+  $hget(%table,lvl) $+ $chr(9) $+ $hget(%table,fact) $+ $chr(9) $+ $hget(%table,spd) $+ $chr(9) $+ $hget(%table,str) $+ $chr(9) $+ $hget(%table,def) $+ $chr(9) $+ $hget(%table,dex) $+ $chr(9) $+ $hget(%table,totb) $+ $chr(9) $+ $hget(%table,man) $+ $chr(9) $+ $hget(%table,int) $+ $chr(9) $+ $hget(%table,end) $+ $chr(9) $+ $hget(%table,totw) $+ $chr(9) $+ $hget(%table,cash) $+ $chr(9) $+ %tsspy.spy $+ $chr(9) $+ $hget(%table,adate)
      if ($exists($tornstats.localfile)) {
        var %eof = $lines($tornstats.localfile), %ptr = 1
        while (%ptr <= %eof) {
          var %readid = $gettok($read($tornstats.localfile,%ptr),2,9)
          if (%readid == $hget(%table,id)) { var %spyold = 1 | write -dl $+ %ptr $tornstats.localfile | break }
          inc %ptr
        }
      }
      write $tornstats.localfile %savestring
    }    
    sockclose spysave
    sockopen -e spysave www.tornstats.com 443
  }
  else {
    msg %tsspy.postchan 04I somehow do not have the ID so I cannot save the spy. Cancelling.
    tornstats.spyclean
  }
}

alias -l tornstats.spysave {
  if (%tsspy.oldspy != $null) { var %table = tsspy. $+ %tsspy.oldspy }
  else { var %table = tsspy. $+ %tsspy.spy }
  if ($hget(%table,id) == $null) {
    msg %tsspy.postchan 04The ID was not successfully read. Paste your spy exactly how Torn sends it to you without modification.
    msg %tsspy.postchan 10Use:07 !addspy 10to see manual input help.
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
    if (!$hfind(%table,adate)) { hadd -m %table adate $gmt }
    sockclose getinfo
    sockopen -e getinfo www.torn.com 443
    msg %tsspy.postchan 06Attempting to get additional info from the API (5 seconds max)...
    .timer $+ 0.tornstats.spyscrape 1 5 tornstats.spywrite
  }
}

alias -l tornstats.spycancel {
  msg %tsspy.postchan 10Spy auto-entry cancelled.
  tornstats.spyclean
}

; called in a few places
alias -l tornstats.spyclean {
  if ($hget(tsspy. $+ %tsspy.spy)) { hfree tsspy. $+ %tsspy.spy }
  unset %tsspy.spy | unset %tsspy.postchan
}

alias -l tornstats.spypost {
  ; $1 is name, $2 is id
  if (%tsspy.found == $null) { set %tsspy.found 0 }
  if (%tsspy.ptr == $null) { set %tsspy.ptr 1 }
  set %tsspy.post %tsspy.post 07 $+ $1 10[07 $+ $2 $+ 10] $+ $chr(44)
  inc %tsspy.ptr    
  inc %tsspy.found
  if (%tsspy.ptr == 7) {
    msg %tsspy.postchan %tsspy.post
    set %tsspy.ptr 1
    set %tsspy.post $null
  }
}

alias -l tornstats.bq {
  ; query string builder for findspy
  var %src = $1, %query = $null, %count = $numtok(%src,32), %ptr = 1
  while (%ptr <= %count) {
    var %prop = $gettok(%src,%ptr,32)
    var %data = $right(%prop,-4)    
    if (%data == $null) {
      msg %tsspy.postchan 04Don't understand your query, use !findspy without params to see some examples
      unset %tsspy.*
      halt
    }
    if ($left(%prop,4) == Fac:) { var %query = %query $+ &faction= $+ %data }
    if ($left(%prop,3) == Str) {
      if ($mid(%prop,4,1) == $chr(60)) { var %param = l } | else { var %param = g }
      var %query = %query $+ &str= $+ $replace(%data,k,000,m,000000,b,000000000,t,000000000000) $+ &strd= $+ %param
    }
    if ($left(%prop,3) == Spd) {
      if ($mid(%prop,4,1) == $chr(60)) { var %param = l } | else { var %param = g }
      var %query = %query $+ &spd= $+ $replace(%data,k,000,m,000000,b,000000000,t,000000000000) $+ &spdd= $+ %param
    }
    if ($left(%prop,3) == Dex) {
      if ($mid(%prop,4,1) == $chr(60)) { var %param = l } | else { var %param = g }
      var %query = %query $+ &dex= $+ $replace(%data,k,000,m,000000,b,000000000,t,000000000000) $+ &dexd= $+ %param
    }
    if ($left(%prop,3) == Def) {
      if ($mid(%prop,4,1) == $chr(60)) { var %param = l } | else { var %param = g }
      var %query = %query $+ &def= $+ $replace(%data,k,000,m,000000,b,000000000,t,000000000000) $+ &defd= $+ %param
    }
    if ($left(%prop,3) == Tot) {
      if ($mid(%prop,4,1) == $chr(60)) { var %param = l } | else { var %param = g }
      var %query = %query $+ &tot= $+ $replace(%data,k,000,m,000000,b,000000000,t,000000000000) $+ &totd= $+ %param
    }
    inc %ptr
  }
  return %query
  ;msg #pmv src = %src | msg #pmv query = %query | halt
}

alias -l tornstats.addspyhelp { return 03Adds a spy to the bot. Manual usage: 07!addspy Name TornID Str Spd Dex Def Total 10Faction (optional). 10To add automatically, use: 07!addspy auto 10if you have the full paste, or 07!addspy Name TornID auto 10if you only have the stats portion. 06For automatic pre-RESPO reports, use: !addspy old }


; ##################################################################### CHANNEL CODE #####################################################################


on *:text:*:%tornstats.chanlist: {
  if ($readini($tornstats.cfgfile,n,Spy,enable) == yes) {
    tornstats.cfgcheck
    tokenize 32 $strip($replace($1-, $chr(9), $chr(32)))
    if ($eval($findfile,0) isin $1-) { halt }
    if ($nick isop #) {
      if (%tsspy.spy == $nick) {
        var %tablename = tsspy. $+ %tsspy.spy
        if ($istok(Strength: Speed: Defense: Dexterity: Total:,$1,32)) {
          .timer $+ 0.tornstats.spyauto off
          .timer $+ %tsspy.spy $+ .spy -co 1 5 tornstats.spysave
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
      }
      if (%tsspy.oldspy == $nick) {
        var %tablename = tsspy. $+ %tsspy.oldspy
        if ($istok(Name: Level: Strength: Speed: Defense: Dexterity: Total: Manual Intelligence: Endurance: Money:,$1,32)) {
          .timer $+ 0.tornstats.spyauto off
          .timer $+ %tsspy.spy $+ .spy -co 1 5 tornstats.spysave
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
        msg # 10The spy storage script should be 'unstuck' now. Sorry about that...
        unset %tsspy.*
      }

      if ($1 == !addspy || $1 == !storespy) {
        if (%tsspy.oldspy != $null || %tsspy.spy != $null) { msg # 04I'm already watching for a spy paste! | halt }
        elseif (($2 == $null) || (auto !isin $2- && $numtok($2-,32) < 7)) { msg # $tornstats.addspyhelp | halt }      
        elseif ($2 == old && $3 == $null) { var %spyoldauto = 1 }
        elseif ($3 == old && $4 == $null) { var %spyoldauto = 1 }
        elseif ($4 == old && $5 == $null) { var %spyoldauto = 1, %spyname = $remove($2,$chr(32)), %spyid = $remove($3,$chr(32),$chr(93),$chr(91)) }
        elseif ($2 == auto && $3 == $null) { var %spyauto = 1 }
        elseif ($3 == auto && $4 == $null) { msg # $tornstats.addspyhelp | halt }
        elseif ($4 == auto && $5 == $null) { var %spyauto = 1, %spyname = $remove($2,$chr(32)), %spyid = $remove($3,$chr(32),$chr(93),$chr(91)) }
        else {        
          set %tsspy.postchan #
          set %tsspy.spy $nick
          var %tablename = tsspy. $+ $nick
          var %chk = 1, %chkstr = $replace($2-,Strength:,$chr(32) $+ Strength:,Speed:,$chr(32) $+ Speed:,Dexterity:,$chr(32) $+ Dexterity:,Defense:,$chr(32) $+ Defense:,Total:,$chr(32) $+ Total:)
          while (%chk < $numtok(%chkstr,32)) {
            if ($gettok(%chkstr,%chk,32) == Speed:) { var %spd = $tornstats.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Strength:) { var %str = $tornstats.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Defense:) { var %def = $tornstats.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Dexterity:) { var %dex = $tornstats.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) }
            if ($gettok(%chkstr,%chk,32) == Total:) { var %tot = $tornstats.cleanN($gettok(%chkstr,$calc(%chk + 1),32)) | var %totmark = $calc(%chk + 1) }
            inc %chk
          }
          if (%spd == $null) {
            var %chkspd = $tornstats.cleanN($5)
            if (%chkspd isnum) { var %spd = %chkspd }
          }
          if (%str == $null) {
            var %chkstr = $tornstats.cleanN($4)
            if (%chkstr isnum) { var %str = %chkstr }
          }
          if (%def == $null) {
            var %chkdef = $tornstats.cleanN($7)
            if (%chkdef isnum) { var %def = %chkdef }
          }
          if (%dex == $null) {
            var %chkdex = $tornstats.cleanN($6)
            if (%chkdex isnum) { var %dex = %chkdex) }
          }
          if (%tot == $null) {
            var %chktot = $tornstats.cleanN($8)
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
          tornstats.spysave
        }
        if (%spyoldauto != $null) {
          set %tsspy.oldspy $nick
          set %tsspy.postchan #
          if (%tablename == $null) { var %tablename = tsspy. $+ $nick }
          if (%spyname != $null) { hadd -m %tablename name %spyname }
          if (%spyid != $null) { hadd -m %tablename id %spyid }
          .timer $+ 0.tornstats.spyauto 1 10 tornstats.spycancel
          msg # 10Go ahead and paste your pre-RESPO spy report07 $nick $+ 10, I'm watching for it...        
        }
        if (%spyauto != $null) {
          set %tsspy.spy $nick
          set %tsspy.postchan #
          if (%tablename == $null) { var %tablename = tsspy. $+ $nick }
          if (%spyname != $null) { hadd -m %tablename name %spyname }
          if (%spyid != $null) { hadd -m %tablename id %spyid }
          .timer $+ 0.tornstats.spyauto 1 10 tornstats.spycancel
          msg # 10Go ahead and paste your spy report7 $nick $+ 10, I'm watching for it...        
        }
      }

      if ($1 == !spy || $1 == !read) {
        if ($2 == $null) { msg # 03Displays stored spy record. 10Usage:07 !spy Name 10or07 !read ID | halt }
        if (!$timer(0.tornstats.spam)) {      
          set %tsspy.postchan #
          if ($1 == !read) { set %tsspy.search i $2 }
          else { set %tsspy.search n $2 }
          sockclose spyget
          sockopen -e spyget www.tornstats.com 443
          .timer $+ 0.tornstats.spam -o 1 10 noop
        }
        else { msg # 04No can do, too many requests too often. You can try again in07 $timer(0.tornstats.spam).secs 04seconds. }
      }

      if ($1 == !findspy || $1 == !spyfind) {
        if ($2 == $null || Fac: !isin $2-) {
          msg # 03Searches for a spy. 10Usage:07 !findspy Query 10- Param List:07 Fac(reqd) Str Dex Def Spd Tot
          msg # 06Example:13 !findspy Fac:test Str>100k Dex<2m Tot:6m 10will search for anyone in faction "test", over 100k str, under 2m dex, and over 6m total stats.
          halt
        }
        if (!$timer(0.tornstats.spam)) {
          set %tsspy.postchan #
          set %tsspy.find $tornstats.bq($2-)
          msg # 10Searching for spies, one sec...
          sockclose spyfind
          sockopen -e spyfind www.tornstats.com 443
          .timer $+ 0.tornstats.spam -o 1 5 noop
        }
        else { msg # 04No can do, too many requests too often. You can try again in07 $timer(0.tornstats.spam).secs 04seconds. }
      }

      if ($1 == !botinfo) { msg # 10 $+ www.tornstats.com Spy Database script made by: $+ 07 PMV 10[07157799310] v $+ 07 $+ $tornstats.botvers }
    }
  }
}

; --------------------------------- initial file generators and setup script ---------------------------------

alias -l tornstats.upg {
  writeini -n $tornstats.cfgfile Spy vers $tornstats.botvers
  if ($readini($tornstats.cfgfile,n,Spy,enable) == $null) { writeini -n $tornstats.cfgfile Spy enable no }
  if ($readini($tornstats.cfgfile,n,Spy,cache) == $null) { writeini -n $tornstats.cfgfile Spy cache no }
  if ($readini($tornstats.cfgfile,n,Spy,sockerr) == $null) { writeini -n $tornstats.cfgfile Spy sockerr no }
  flushini $tornstats.cfgfile
}
