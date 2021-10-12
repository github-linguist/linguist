; -=- Script Settings v1.3 (May 28 2016)
; This script will automatically use SSL if the proper libraries are installed. 
; To check: go to Tools - Options, Connect - Options. If the SSL button is greyed out, it is not active.

; API Key. Change to your API key.
alias -l prof.apikey { return apikey12 }

; Enabled Channel List. Separate by space.
alias -l prof.chanlist { return #pmvtest #pmv }

; Colours. Same order as the UTA bot. Change by pressing CTRL-K and two digit combination.
alias -l prof.col {
  if ($1 == 1) { return 10 } ; Primary 
  if ($1 == 2) { return 07 } ; Secondary (Highlights)
  if ($1 == 3) { return 03 } ; Help
  if ($1 == 4) { return 13 } ; Note
  if ($1 == 5) { return 04 } ; Error
  if ($1 == 6) { return 03 } ; Good/Positive
  if ($1 == 7) { return 04 } ; Bad/Negative
  if ($1 == 8) { return 06 } ; Subnote
}

; Do you want the link displayed after? if not, change to 0.
alias -l prof.linkafter { return 1 }

; ID File Locations. The default is usually fine. 
alias -l prof.idsfile { return $qt($scriptdir $+ id.hsh) }
alias -l prof.idsnamefile { return $qt($scriptdir $+ idname.hsh) }

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; -=- Script Code. You do not need to edit/set anything under this line unless you want to change it. -=-
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

on 1:start:{
  if ($hget(prof.ids)) { hfree prof.ids } 
  hmake prof.ids 100 
  if ($exists($prof.idsfile)) { hload prof.ids $prof.idsfile }  
  if ($hget(prof.idsname)) { hfree prof.idsname } 
  hmake prof.idsname 100 
  if ($exists($prof.idsnamefile)) { hload prof.idsname $prof.idsnamefile } 
}

on *:text:*:#: { 
  if ($istok($prof.chanlist,$chan,32)) {
    tokenize 32 $strip($replace($1-, $chr(9), $chr(32), $chr(194), $chr(32)))
    if ($1 == !addid) {
      if ($2 == $null) { 
        msg # $prof.col(3) $+ Adds an ID to the bot. $prof.col(1) $+ Usage: $+ $prof.col(2) !addid Name TornID 
        msg # $prof.col(4) $+ Note: $+ $prof.col(8) if your name displays wrong after adding to the bot, you can fix yourself with: $prof.col(4) $+ !setname Name $prof.col(8) $+ or get an op to use $prof.col(4) $+ !setmain Name ID
      }
      else {
        if ($2 isnum && $3 == $null) { var %pid = $prof.cleanN($2) | var %pnick = $nick }
        elseif ($3 isnum) { var %pid = $prof.cleanN($3) | var %pnick = $2 }
        else { msg # $prof.col(5) $+ Try again. Usage: $+ $prof.col(2) !addid Name TornID | halt }
        if (%pid != $null) { 
          var %retid = $prof.getid(%pnick)
          if (%retid == $null) { 
            hadd -m prof.ids %pnick %pid            
            hsave prof.ids $prof.idsfile
            msg # $prof.col(1) $+ Added $+ $prof.col(2) %pnick $prof.colid(%pid,1,2) to the database! If your name displays weird, fix with: $+ $prof.col(2) !setname Name
          }
          else { msg # $prof.col(5) $+ Whoops! $+ $prof.col(2) %pnick $prof.col(5) $+ already exists in the bot with ID $+ $prof.col(2) %retid $+ $prof.col(5) $+ .  }
        }
      }
    }    
    if ($1 == !delid) { 
      if ($2 == $null) { var %pnick = $nick }
      else { var %pnick = $2 }      
      var %retid = $prof.getid(%pnick)   
      if (%retid != $null) {
        hdel prof.ids %pnick
        hsave prof.ids $prof.idsfile
        if ($prof.getname(%retid) == %pnick) {
          if ($hget(prof.idsname,%retid) != $null) { 
            hdel prof.idsname %retid
            hsave prof.idsname $prof.idsnamefile
          }
        }
        msg # $prof.col(6) $+ Removed $+ $prof.col(2) %pnick $prof.col(6) $+ from the database! 
      }
      else { msg # $prof.col(5) $+ No can do, $+ $prof.col(2) %pnick $prof.col(5) $+ does not exist in the database! }
    }

    if ($1 == !setname) {
      if ($2 != $null) {
        var %id = $prof.getid($2), %idchk = $prof.getid($nick)
        if (%id != $null) {
          if (%idchk == %id) {           
            hadd -m prof.idsname %id $2
            hsave prof.idsname $prof.idsnamefile
            msg # $prof.col(6) $+ I have assigned $+ $prof.col(2) $2 $prof.col(6) $+ as the main name for ID $prof.colid(%id,6,2) $+ !
          } 
          else { msg # $prof.col(2) $+ $nick $+ $prof.col(5) and $prof.col(2) $+ $2 $+ $prof.col(5) do not have the same ID saved. }
        }
        else { msg # $prof.col(5) $+ I have no idea who $+ $prof.col(2) $2 $prof.col(5) $+ is, please make sure that it's added via !addid first }
      }
      else { msg # $prof.col(3) $+ Sets your main name (needs to be added via !addid). $prof.col(1) $+ Usage: $+ $prof.col(2) !setname Name }
    }

    if ($istok(!revive !id !bust !bail !bazaar !pstats !display !trade !cash !money !mail !msg !message !attack !atk !mug !bounty !friend !friendlist !flist !enemy !blacklist !blist,$1,32)) {
      if ($2 == $null) { var %searchnick = $nick }
      else { var %searchnick = $2 }
      var %retid = $prof.getid(%searchnick)
      if (%retid == $null) { msg # $prof.col(5) $+ Sorry, I have no idea who $+ $prof.col(2) %searchnick $prof.col(5) $+ is! } 
      else { 
        var %retnick = $prof.getname(%retid)
        if ($1 == !id) { 
          if (%searchnick != $nick) { msg # $prof.col(2) $+ %searchnick $+ $prof.col(1) $+ 's stored ID is $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) - Profile link: $+ $prof.col(2) http://www.torn.com/profiles.php?XID= $+ %retid }
          else { msg # $prof.col(1) $+ Your stored ID is $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) - Profile link: $+ $prof.col(2) http://www.torn.com/profiles.php?XID= $+ %retid }
        } 
        if ($1 == !revive) { msg # $prof.col(1) $+ Revive $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ ! - $+ $prof.col(2) http://www.torn.com/profiles.php?XID= $+ %retid } 
        if ($1 == !bust) { msg # $prof.col(1) $+ Bust $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) out of jail! - $+ $prof.col(2) http://www.torn.com/profiles.php?XID= $+ %retid } 
        if ($1 == !bail) { msg # $prof.col(1) $+ Bail $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) out of jail! - $+ $prof.col(2) http://www.torn.com/profiles.php?XID= $+ %retid } 
        if ($1 == !bazaar) { msg # $prof.col(1) $+ Visit $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ 's bazaar! - $+ $prof.col(2)  http://www.torn.com/bazaar.php#/p=bazaar&userID= $+ %retid } 
        if ($1 == !pstats) { msg # $prof.col(1) $+ View $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ 's personal stats! - $+ $prof.col(2) http://www.torn.com/personalstats.php?ID= $+ %retid } 
        if ($1 == !display) { msg # $prof.col(1) $+ View $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ 's display case! - $+ $prof.col(2) http://www.torn.com/displaycase.php?userID= $+ %retid } 
        if ($1 == !trade) { msg # $prof.col(1) $+ Trade with $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ ! - $+ $prof.col(2) http://www.torn.com/trade.php#step=start&userID= $+ %retid } 
        if ($1 == !cash || $1 == !money) { msg # $prof.col(1) $+ Send cash to $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ ! - $+ $prof.col(2) http://www.torn.com/sendcash.php#/XID= $+ %retid } 
        if ($1 == !mail || $1 == !msg || $1 == !message) { msg # $prof.col(1) $+ Send a message to $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ ! - $+ $prof.col(2) http://www.torn.com/messages.php#/p=compose&XID= $+ %retid } 
        if ($1 == !attack || $1 == !atk) { msg # $prof.col(1) $+ Attack $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ ! - $+ $prof.col(2) http://www.torn.com/attack.php?PID= $+ %retid } 
        if ($1 == !mug) { msg # $prof.col(1) $+ Mug $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ ! - $+ $prof.col(2) http://www.torn.com/attack.php?PID= $+ %retid } 
        if ($1 == !bounty) { msg # $prof.col(1) $+ Bounty $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) $+ ! - $+ $prof.col(2) http://www.torn.com/bounties.php#/p=add&XID= $+ %retid } 
        if ($1 == !friend || $1 == !friendlist || $1 == !flist) { msg # $prof.col(1) $+ Add $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) to your $prof.col(6) $+ friends list $+ $prof.col(1) $+ ! - $+ $prof.col(2) http://www.torn.com/friendlist.php#/p=add&XID= $+ %retid }
        if ($1 == !enemy || $1 == !blacklist || $1 == !blist) { msg # $prof.col(1) $+ Add $+ $prof.col(2) %retnick $prof.colid(%retid,1,2) to your $prof.col(7) $+ blacklist $+ $prof.col(1) $+ ! - $+ $prof.col(2) http://www.torn.com/blacklist.php#/p=add&XID= $+ %retid }
      }
    }
    if ($1 == !attackid || $1 == !atkid) { 
      if ($2 isnum) { 
        var %namechk = $prof.getname($2)
        if (%idchk != $null) { var %idpost = $prof.col(2) $+ %namechk }
        else { var %idpost = $prof.colid($2,1,2) }
        msg # $prof.col(1) $+ Attack %idpost $+ $prof.col(1) $+ ! - $+ $prof.col(2) http://www.torn.com/attack.php?PID= $+ $2 
      }
      else { msg # $prof.col(3) $+ Displays an attack link from an ID that you specify. $prof.col(1) $+ Usage: $+ $prof.col(2) !attackid ID }
    }

    if ($istok(!prof !profile !info !find,$1,32)) {
      if ($2 == $null) { var %name = $nick }
      if ($2 isnum) { var %id = $2 }
      else { if (%name == $null) { var %name = $2 } }
      if (%id == $null) { var %id = $prof.getid(%name) } 
      if (%id == $null) { msg # $prof.col(5) $+ There is no ID stored for $+ $prof.col(2) %name $+ $prof.col(5) $+ ! | halt }
      set %prof.find %id    
      set %prof.chan #              
      sockclose profile 
      if ($sslready) { sockopen -e profile api.torn.com 443 }
      else { sockopen profile api.torn.com 80 }
    } 
  }
}

; New getname/getid split call system from UTA 7
alias -l prof.getid { return $hget(prof.ids,$1) }
alias -l prof.getname { 
  if ($hget(prof.idsname,$1) != $null) { return $hget(prof.idsname,$1) }
  else { return $hfind(prof.ids,$1).data }
}

; Colors and sets a bracket around ID numbers. 
alias -l prof.colid { return $prof.col($2) $+ $chr(91) $+ $prof.col($3) $+ $1 $+ $prof.col($2) $+ $chr(93) }

; Strips all input ascii chars aside from decimals and numbers. 
; Used to clean accidental/intentional letters in inputs that expect numbers.
alias -l prof.cleanN { 
  var %a = 0 | var %b = $1
  while (%a < 255) { if (%a != 46 && %a !isnum 48-57) { var %b = $remove(%b, $chr(%a)) } | inc %a }
  return %b
}

; Strips HTML tags. 
alias -l prof.cleanH { return $regsubex($1,/^[^<]*>|<[^>]*>|<[^>]*$/g,$null) }

; Separates a number into neat little commas.
alias -l prof.addComma { var %a, %b = $regsub($ticks,$1,/\G([+-]?\d+?)(?=(?:\d{3})++(?=\.\d++$|$))/g,\1 $+ $chr(44),%a) | return %a }

; Profile String Builder
alias -l prof.outstr { 
  ; broke it up to make it easier to read.

  ; check for online/offline status (api doesnt return it, so determine it)
  if ($gettok(%prof.last,2,32) == minute || $gettok(%prof.last,2,32) == minutes) {
    if ($gettok(%prof.last,1,32) <= 15) { set %prof.ol $prof.col(6) $+ [On] }
    else { set %prof.ol $prof.col(7) $+ [Off] }
  }
  else { set %prof.ol $prof.col(7) $+ [Off] }

  ; fix a few odd chars
  set %prof.fact $replace(%prof.fact,&#33;,$chr(33))
  set %prof.fact $replace(%prof.fact,&#39;,$chr(33))
  set %prof.fact $replace(%prof.fact,&#40;,$chr(33))
  set %prof.fact $replace(%prof.fact,&#41;,$chr(33))

  ; then replace a few longer strings with shorter forms
  set %prof.status $replace(%prof.status,In hospital,Hosp)
  set %prof.status $remove(%prof.status,- Hospitalized)
  set %prof.status $replace(%prof.status,In jail,Jail)
  set %prof.status $replace(%prof.status,In federal jail,Fed)
  set %prof.status $replace(%prof.status,Was caught trying to break out,Caught busting)  
  set %prof.status $replace(%prof.status,currently okay,okay)
  set %prof.status $replace(%prof.status,1 hrs,1 hr)
  set %prof.status $replace(%prof.status,1 mins,1 min)
  set %prof.status $replace(%prof.status,$+($chr(34),$chr(44),$chr(34)),$+($chr(32),$chr(45),$chr(32)))
  set %prof.last $replace(%prof.last,minutes,mins,minute,min)
  set %prof.prop $replace(%prof.prop,Private Island,PI)

  if ($right(%prof.status,1) == $chr(45)) { set %prof.status $left(%prof.status,-1) }

  ; make the age display more useful
  if (%prof.age >= 365) { 
    var %yr = 0
    var %dy = %prof.age
    while (%dy >= 365) {
      inc %yr
      var %dy = $calc(%dy - 365)
    }
    if (%dy = 0) { var %agestring = $prof.col(6) $+ %yr $+ y }
    else { var %agestring = %yr $+ y %dy $+ d }
  }
  else { var %agestring = $prof.addComma(%prof.age) }

  ; add a percentage to life
  var %lifeperc $round($calc((%prof.lifecur / %prof.lifemax) * 100),0)

  ; colorize specific parts if needed
  if ((%prof.fact == None) && (%prof.facrank == None)) { 
    var %facstr $prof.col(7) $+ None 
  }
  else { 
    var %facstr $prof.col(2) $+ %prof.facrank $prof.col(1) $+ of $+ $prof.col(2) %prof.fact 
  }
  if (%prof.spname == None && %prof.spdur == 0) { var %spstr $prof.col(7) $+ None }
  else { var %spstr $prof.col(2) $+ %prof.spname $prof.col(8) $+ (for %prof.spdur $+ d) }
  if (%prof.lifecur == %prof.lifemax) { var %lifecol 6 }
  elseif (%prof.lifecur <= 1) { var %lifecol = 7 } 
  else { var %lifecol 2 }

  if (okay isin %prof.status) { set %prof.status $prof.col(6) $+ %prof.status }
  elseif (Jail for isin %prof.status) { set %prof.status $prof.col(7) $+ %prof.status }
  elseif (Fed for isin %prof.status) { set %prof.status $prof.col(7) $+ %prof.status }
  elseif (Hosp for isin %prof.status) { set %prof.status $prof.col(7) $+ %prof.status }
  else { set %prof.status $prof.col(2) $+ %prof.status }

  ; now format output string
  var %out = $prof.col(2) $+ %prof.name $prof.colid(%prof.id,1,2) %prof.ol  
  if (%prof.don == 1) { var %out = %out $prof.col(6) $+ $chr(42) }
  var %out = %out $+ $prof.col(1) ( $+ $prof.col(2) $+ %prof.sex
  var %out = %out $+ $prof.col(1) $+ , Level: $+ $prof.col(2) %prof.lvl  
  var %out = %out $+ $prof.col(1) $+ , Title: $+ $prof.col(2) %prof.rank
  var %out = %out $+ $prof.col(1) $+ , Age: $+ $prof.col(2) %agestring 
  if (%prof.age >= 365) { var %out = %out $prof.col(8) $+ ( $+ %prof.age $+ ) }
  var %out = %out $+ $prof.col(1) $+ , Spouse: %spstr
  var %out = %out $+ $prof.col(1) $+ , Prop: $+ $prof.col(2) %prof.prop
  var %out = %out $+ $prof.col(1) $+ , Fac: %facstr 
  var %out = %out $+ $prof.col(1) $+ , Life: $+ $prof.col(%lifecol) $prof.addComma(%prof.lifecur) $+ $prof.col(1) $+ / $+ $prof.col(%lifecol) $+ $prof.addComma(%prof.lifemax) $prof.col(8) $+ ( $+ %lifeperc $+ $chr(37) $+ )
  var %out = %out $+ $prof.col(1) $+ , A: $+ $prof.col(2) $+ %prof.award $+ $prof.col(1) $+ /F: $+ $prof.col(6) $+ %prof.friend $+ $prof.col(1) $+ /E: $+ $prof.col(7) $+ %prof.enemy 
  var %out = %out $+ $prof.col(1) $+ , Seen: $+ $prof.col(2) %prof.last
  var %out = %out $+ $prof.col(1) $+ , Status: %prof.status  
  var %out = %out $+ $prof.col(1) $+ )
  return %out
}

; -=- Socket Code

on *:sockopen:profile:{
  if ($sockerr) { echo -s 04Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    sockwrite -nt profile GET /user/ $+ %prof.find $+ ?selections=profile&key= $+ $prof.apikey HTTP/1.1
    sockwrite -nt profile Host: api.torn.com
    sockwrite -nt profile $crlf
  }
}

on *:sockread:profile:{
  if ($sockerr) { echo -s 4Socket Error. Socket: $sockname --- Message: $sock($sockname).wsmsg --- Num: $sock($sockname).wserr } 
  else {
    var %read | sockRead %read | var %readln $remove(%read, $chr(9))  
    ;write $qt($scriptdir $+ test $+ $gmt $+ .txt) %readln
    if ($regex(%readln,error":"(.+?)")) { set %prof.err $regml(1) }
    if ($regex(%readln,rank":"(.+?)")) { set %prof.rank $regml(1) }
    if ($regex(%readln,level":(.+?)")) { set %prof.lvl $prof.cleanN($regml(1)) }
    if ($regex(%readln,gender":"(.+?)")) { set %prof.sex $regml(1) }
    if ($regex(%readln,property":"(.+?)")) { set %prof.prop $regml(1) }
    if ($regex(%readln,status":\["(.+?)"\])) { set %prof.status $prof.cleanH($regml(1)) }
    if ($regex(%readln,awards":(.+?)")) { set %prof.award $prof.cleanN($regml(1)) }
    if ($regex(%readln,friends":(.+?)")) { set %prof.friend $prof.cleanN($regml(1)) }
    if ($regex(%readln,enemies":(.+?)")) { set %prof.enemy $prof.cleanN($regml(1)) }
    if ($regex(%readln,age":(.+?)")) { set %prof.age $prof.cleanN($regml(1)) }
    if ($regex(%readln,donator":1)) { set %prof.don 1 }
    if ($regex(%readln,player_id":(.+?)")) { set %prof.id $prof.cleanN($regml(1)) }
    if ($regex(%readln,name":"(.+?)")) { set %prof.name $regml(1) }    
    if ($regex(%readln,last_action":"(.+?)")) { set %prof.last $regml(1) }
    if ($regex(%readln,current":(.+?)")) { set %prof.lifecur $prof.cleanN($regml(1)) }
    if ($regex(%readln,maximum":(.+?)")) { set %prof.lifemax $prof.cleanN($regml(1)) }
    if ($regex(%readln,faction_name":"(.+?)")) { set %prof.fact $regml(1) }
    if ($regex(%readln,faction":{"position":"(.+?)")) { set %prof.facrank $regml(1) }
    if ($regex(%readln,spouse_name":"(.+?)")) { set %prof.spname $regml(1) }
    if ($regex(%readln,duration":(.+?)})) { set %prof.spdur $prof.cleanN($regml(1)) }
    if ($regex(%readln,}})) { prof.sockclose 1 }
  }
}

on *:sockclose:profile:{ prof.sockclose }

; calls on close socket or at the end of a string, whichever comes first.
; helps avoid the timeout glitch. If called with $1 == 1, it forces a sockclose.
alias -l prof.sockclose {
  if (%prof.err != $null) { msg %prof.chan $prof.col(5) $+ Error: $+ $prof.col(2) %prof.err } 
  else { 
    msg %prof.chan $prof.outstr 
    if ($prof.linkafter == 1) {
      msg %prof.chan $prof.col(1) $+ Link: $+ $prof.col(2) http://www.torn.com/profiles.php?XID= $+ %prof.id
    }
  }
  if ($1 == 1) { sockclose profile }
  unset %prof.*
}
