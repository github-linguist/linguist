;=====================================================================================
;====================================== HOSTBOT ======================================
;================================= TWITCH.TV/MRDOCDC =================================
;=============================== AUTO !HOSTME v0.1.4.3 ===============================
;=====================================================================================
;====== Please do not edit below this line unless you know what you are doing!! ======
;==                   This the basic release for Auto !HOSTME Bot                   ==
;==                This bot will type !HOSTME for you automatically!                ==
;==           Simple and open source. Questions/Support Contect me below:           ==
;==                               Twitter:   @MrDocDC                               ==
;==                      Discord:   https://discord.gg/ns2uTBS                      ==
;==                         Web: https://bluecat.live/faq/                          ==
;============================          Enjoy  :)          ============================
;=====================================================================================


;=======================================================
;=====================Custom stuff======================
;;add your custom scripts here if you would like.
; I advise not spamming channels as this will get you banned!
; I suggest spamming channels every 2 minutes (or 120 seconds)


;=======================================================
;===============CONNECTION AND DISCONNECT===============

ON *:CONNECT: {
  SET %version 0.1.4.3
  window -g1 @Logs
  echo -m @Logs $timestamp Auto Hostme Bot Successfully loaded. Version: %version
  echo -m @Logs $timestamp 3To check for the latest version:12 https://bluecat.live/update/
  echo -m @Logs $timestamp
  echo -m @Logs $timestamp
  IF ($server == tmi.twitch.tv) {
    timers off
    twitchpart
    CAP REQ :twitch.tv/commands twitch.tv/tags twitch.tv/membership
    VAR %x = 1
    ;; loop to join all the channels in the autojoin.txt file
    WHILE ($read(autojoin.txt, t,%x) != $null) {
      VAR %chan $chr(35) $+ $read(autojoin.txt, t, %x)
      .TIMER -m 1 $calc(%x * 333) JOIN %chan
      INC %x
    }
    if (%advertson) { startad }
    startclear
    SET %myName $me
    VAR %livecheck $livechecker(%myName)
  }
}
ON *:DISCONNECT:IF ($server == tmi.twitch.tv) twitchpart
alias twitchpart {
  VAR %chans $chan(0)
  WHILE (%chans) {
    VAR %chan $chan(%chans)
    PART %chan
    DEC %chans
  }
}

;=======================================================
;==================== menu aliases =====================

menu query,channel,status {
  $style(2) &Version  %version:$null 
  -
  Help
  .Trouble Shooting &Video: /url https://www.youtube.com/channel/UCI4C1vG7LMqnwfimJQe6VcA/playlists
  .Trouble Shooting &Link: /url https://bluecat.live/faq/
  -
  Check For &Updates: /url https://bluecat.live/update/
  -
  Get your &oAuth: /url https://twitchapps.com/tmi/
  -
  &Clear &Current Screen:/clear
}
menu @Logs  {
  Clear
  .Clear Current://clear
  .Clear All://clearall
}
;=======================================================
;===================== bot scripts =====================

;;waits for unhost, picks a random number between 1 and 45, posts !hostme in that (random)time in seconds
ON *:NOTICE:*Exited host mode*:#: {
  VAR %livecheck $livechecker(%myName)
  if (%livecheck) && (%livecheck != $false) {
    if ($chan == #p0sitivitybot) {
      timer $+ $chan 1 $rand(1,45) { msg $chan $chr(63) $+ HOSTME | echo -m @Logs $timestamp 9Entered into the raffle for channel  $+ $chan $+ . USED $chr(63) $+ HOSTME }
      return
    }
    elseif ($chan == #shouman) {
      timer $+ $chan 1 $rand(1,45) { msg $chan $chr(36) $+ HOSTME | echo -m @Logs $timestamp 9Entered into the raffle for channel  $+ $chan $+ . USED $chr(36) $+ HOSTME }
      return
    }
    else {
      timer $+ $chan 1 $rand(1,45) { msg $chan !hostme | echo -m @Logs $timestamp 9Entered into the raffle for channel  $+ $chan $+ . USED $chr(33) $+ HOSTME }
    }
  }
  else { echo -m @Logs $timestamp  4Not Entered into the raffle for $chan $+ . $chr(91) $+ not live $+ $chr(93) }
}
;;if you have posted the same message too soon, it will try again in 32 seconds.
;; this will only happen once as to not keep infinitely looping.
ON *:NOTICE:*identical to the previous*:#: {
  if (%advertson) && (%retry < 1) {
    VAR %retry 0
    SET %tryagain $chan
    INC %retry 1
    .timer.tryagain 1 32 { msg %tryagain !hostme | echo -m @Logs $timestamp 9Entered you were entered into the raffle for channel $+ %tryagain $+ . }
  }
}
;;notice if room is in followers-only mode.
ON *:NOTICE:*followers-only mode*:#: {
  echo -m @Logs $timestamp  4Not Entered into the raffle for $chan $+ . $chr(91) $+ not following $+ $chr(93) Click here and follow: 12https://twitch.tv/ $+ $remove($chan, $chr(35))
}
;;this will let the dev know if anyone is running an outdated version. dev will message you to update.
ON *:text:!version:#: {
  if ($nick == MrDocDC) {
    msg $nick I'm running the Auto !HOSTME Bot VERSION: %version
  }
}
;; timer to clear the channel windows every 999seconds to keep buffer down
alias startclear {
  .TIMER.CLEAR.* off
  VAR %xx 1
  WHILE ($read(autojoin.txt, t,%xx) != $null) {
    VAR %chan $chr(35) $+ $read(autojoin.txt, t, %xx)
    .TIMER.CLEAR. $+ %xx 0 999 clear %chan
    INC %xx
  }
}
;;this will call the alias to verify the streamer is live
;; in oter words, it will only send the message if you are live streaming!
alias livechecker {
  JSONOpen -uw livecheck https://api.twitch.tv/kraken/streams/ $+ $1 $+ ?nocache= $+ $ticks
  JSONHttpHeader livecheck Client-ID avm4vi7zv0xpjkpi3d4x0qzk8xbrdw8
  JSONHttpFetch livecheck
  VAR %x $IIF($json(livecheck,stream,created_at).value,$true,$false)
  JSONClose livecheck
  RETURN %x
}

;======================= end bot =======================
;=======================================================

;=======================================================
;===================== Change Logs =====================
;
; v 0.1.4.3 12MAR2019
;           + Minor changes to colors and menu
;           + Added notice of followers-only mode in @Logs window
;
; v 0.1.4.2 03MAR2019
;           + fixed live checker
;           + Need: notice of not following; follower-only chat
;
; v 0.1.4.1 03MAR2019
;           + added more channels to join
;           + patched
;
; v 0.1.4.0 06NOV2018
;           + changed channels
;			+ !hostme only sends if streamer is live
;			+ updated website and video
;
; v 0.1.3.0 14OCT2018
;           + changed channels
;			+ made !hostme only send if channel unhosts
;			+ updated video to reflect this version
;
;
; v 0.1.2.1 20JUN2018
;           + minor changes to autojoin channels due to not hosting 24/7
;
; v 0.1.2.0 12JUN2018
;           + updated files and added step to install mIRC separately
;			+ resolved issues with evaluation exired.
;				* wait for continue button to be clickable
;				* click register, close browser, wait for continue button to be clickable
;				* delete and stop using produce or register it.
;
; v 0.1.1.1 12JUN2018
;
;			+ Added links to new help desk! https://bluecat.live/support/           
;           + Auto/Manual Timers
;           + Wait timer for .unhost (electricalskateboard/hostraffles/more to come)
;           + Fix timing between msg channels
;           + Instructions on youtube
;           + Hosted files both initial install and update
;           + Added a help file online. Will link everything there eventually
;           + All-in-one zip for initial update
;           
;           
; v 0.1.1.0 10JUN2018
;           
;           Added mIRC.exe to the .zip file. Changed timers for notify on host mode.
;             - timers will help keep congestion down in channels.
;      
;      
; v 0.1.0.2 10JUN2018
;           
;           TODO: try to see if whole mIRC dir can be put in zip for fewer steps on install. +Done v0.1.1.1
;           Took out channels that only accept entries after .unhost
;           Added msg on unhost / made file to read those that will mass message only
;           Added repeat try if message identical
;           Changed timers from miliseconds to seconds, with 3 seconds between each one.
;      
;      
; v 0.1.0.1 09JUN2018
;           
;           Changed the below command to ON NOTICE. TEXT didn't respond.
;           Added ON TEXT to send !hostme if a channel has .unhost'd
;           Updated googledrive file.
;           Changed method for sending message from /amsg to a while loop for all channels in autojoin.
;      
; v 0.1.0.0 07JUN2018
;           
;           Uploaded files to youtube & drive
;           Updated raffle channels
;           Coded simple !hostme bot to say !hostme in all joined host raffle channels
;=======================================================
;=================== End script1.mrc ===================
