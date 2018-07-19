declare
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  fun {GetPage Url}
     F = {New Open.file init(url:Url)}
     Contents = {F read(list:$ size:all)}
  in
     {F close}
     Contents
  end

  fun {GetDateString Doc}
     case {Regex.search "<BR>([A-Za-z0-9:., ]+ UTC)" Doc}
     of match(1:S#E ...) then {List.take {List.drop Doc S} E-S+1}
     end
  end

  Url = "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
in
  {System.showInfo {GetDateString {GetPage Url}}}
