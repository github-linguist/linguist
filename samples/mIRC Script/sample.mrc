; Simple !hello response script

on *:text:!hello:#: { msg # Hello there! I am a simple response script. }

; Simple startup script
on 1:start:{
    echo -ag This is a start up script. The ag parameter makes it display in the active window (a) and prevents logging (g)

    var %localvar = This is a local variable

    var -g %globalvar = This is a global variable

    set %anotherglobalvar This is another global variable
    
    unset %anotherglobalvar ; this unsets %anotherglobalvar
  }

; sample string return (local scope - not runable by /command)
alias -l sample { return Have a great day! }

; sample if statement (local scope)
alias -l sampleif { 
  if ($variable != $null) { return The text stored in 'variable' is not null }
  else { return The text stored in 'variable' is null (empty) }
}

; sample filename return (local scope)
alias -l file { return $qt($scriptdir $+ config.ini) }

; sample 'public scope' script
alias sayhi {
  echo -ag Hi there, this is a simple echo response. To use this, type /sayhi in mirc.
}