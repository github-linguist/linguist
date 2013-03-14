##
# Functions... *MUCH* better than aliases, and they do more too!
# but they clutter the environment... (try typing just 'set' at the prompt)
##
# The reason that some of these are... odd... is because
# I had to convert them early because bash can't do positional
# arguments in aliases! functionName () { do something $@<-arguments ; }
 function ls    { command ls -Fh "$@"; }
        # 'command ls' to prevent loop; -A for .file, -F for dir/ link@,
        # -h for 5k 3m 1g, -o for printing flags (uchg)...
 function l     { ls -l "$@"; } # -l to list in long format...
 function ll    { l "$@" | less -XF ; } # pipe into 'more'

##
# Tips and Ticks... from http://www.caliban.org/bash/index.shtml
##
# The $CDPATH variable is so that you can be in /path/to/something and 'cd'
# to 'somethingElse' and end up in /not/the/same/path/to/somethingElse.
# iWould use it if it didn't ALWAYS echo the directory it changes to!
#CDPATH='.:~'
#
# HISTIGNORE="&:l:ls:ls *:l *:cd:cd *:[bf]g:exit:quit:q:sleep *"
        # History ignores commands that include any l/ls/cd etc
        # This kicks-ass! It drops repeats and other useless
        # things from the command history!
 HISTIGNORE="[bf]g:exit:quit:q:sleep *"
        # I want to see l/ls/cd in my history
 HISTCONTROL=ignoreboth
        # ignores both commands that start with a space or a tab, and duplicates
        # other options are as follows:
        # `ignorespace' means to not enter lines which begin with a space or tab into the history list.
        # `ignoredups' means to not enter lines which match the last entered line.
        # `ignoreboth' combines the two options.

 shopt -s cdspell extglob progcomp
        # Spell check for 'cd', extended globbing, programmable completion

##
# Bash Completion... Cannibalised from bash_completion 20030929
# Completion defaults... Yes, its long...
# Basically this sets up many useful defaults for command completion, these
#  should probly be built into bash. Use bash_completions itself if you want
#  more functionality and don't mind the hacks it uses.
 complete -f -X '!*.?(t)bz2' bunzip2 bzcat bzcmp bzdiff bzegrep bzfgrep bzgrep
 complete -f -X '!*.@(zip|ZIP|jar|JAR|exe|EXE|pk3|war|wsz|ear|zargo|xpi)' unzip zipinfo
 complete -f -X '*.Z' compress znew
 complete -f -X '!*.@(Z|gz|tgz|Gz|dz)' gunzip zcmp zdiff zcat zegrep zfgrep zgrep zless zmore
 complete -f -X '!*.Z' uncompress
 complete -f -X '!*.@(gif|jp?(e)g|tif?(f)|pn[gm]|p[bgp]m|bmp|xpm|ico|xwd|tga|GIF|JP?(E)G|TIF?(F)|PN[GM]|P[BGP]M|BMP|XPM|ICO|XWD|TGA)' ee  display
 complete -f -X '!*.@(gif|jp?(e)g|tif?(f)|png|p[bgp]m|bmp|x[bp]m|rle|rgb|pcx|fits|pm|GIF|JPG|JP?(E)G|TIF?(F)|PNG|P[BGP]M|BMP|X[BP]M|RLE|RGB|PCX|FITS|PM)' xv qiv
 complete -f -X '!*.@(@(?(e)ps|?(E)PS|pdf|PDF)?(.gz|.GZ|.Z))' gv ggv
 complete -f -X '!*.@(dvi|DVI)?(.@(gz|Z|bz2))' xdvi
 complete -f -X '!*.@(dvi|DVI)' dvips dviselect dvitype
 complete -f -X '!*.@(pdf|PDF)' acroread xpdf
 complete -f -X '!*.texi*' makeinfo texi2html
 complete -f -X '!*.@(?(la)tex|?(LA)TEX|texi|TEXI|dtx|DTX|ins|INS)' tex latex slitex jadetex pdfjadetex pdftex pdflatex texi2dvi
 complete -f -X '!*.@(mp3|MP3|m3u)' mpg123 mpg321
 complete -f -X '!*.@(mp?(e)g|MP?(E)G|wma|avi|AVI|asf|vob|bin|dat|vcd|ps|pes|fli|viv|rm|ram|yuv|mov|MOV|qt|QT|wmv|mp3|MP3|ogg|OGG|ogm|OGM|mp4|MP4|wav|WAV)' xine
 complete -f -X '!*.@(avi|asf|wmv)' aviplay
 complete -f -X '!*.@(rm|ram|smi?(l))' realplay
 complete -f -X '!*.@(mp?(e)g|avi|mov|qt)' xanim
 complete -f -X '!*.@(ogg|OGG|m3u)' ogg123
 complete -f -X '!*.@(mp3|MP3|ogg|OGG|pls|m3u)' gqmpeg freeamp
 complete -f -X '!*.@(mp[23]|MP[23]|ogg|OGG|wav|WAV|pls|m3u|xm|mod|s[3t]m|it|mtm|ult|flac)' xmms
 complete -f -X '!*.fig' xfig
 complete -f -X '!*.@(mid?(i))' timidity playmidi
 complete -f -X '.*|*.@(o|so|so.!(conf)|a|tar?(.@(gz|bz2))|tgz|tbz2|rpm|zip|ZIP|gif|GIF|jp?(e)g|JP?(E)G|mp3|MP3|mp?(e)g|MPG|avi|AVI|asf|ASF|ogg|OGG|class|CLASS)' vi vim gvim rvim view rview rgvim rgview gview emacs
 complete -f -X '!*.@(exe|EXE|com|COM|scr|SCR)' wine
 complete -f -X '!*.@(zip|ZIP|z|Z|gz|GZ|tgz|TGZ)' bzme
 complete -f -X '!*.@(?([xX]|[sS])[hH][tT][mM]?([lL]))' netscape mozilla lynx opera w3m galeon curl dillo elinks links
#
 complete -u su passwd groups # user commands see only users
 complete -A stopped -P '%' bg # bg completes with stopped jobs
 complete -j -P '%' fg jobs disown # other job commands
 complete -v readonly unset export # readonly and unset complete with shell variables
 complete -A setopt set # set completes with set options
 complete -A shopt shopt # shopt completes with shopt options
 complete -A helptopic help # helptopics
 complete -a unalias # unalias completes with aliases
 complete -A binding bind # bind completes with readline bindings (make this more intelligent)
 complete -c command type which man #sudo # type, which, man complete on commands
 complete -d pushd cd rmdir # Make directory commands see only directories
 complete -W ' ' alias # no filenames for alias,

##
# Set the prompt
##
 PS1="[\h:\w] \[\033[1;34m\]\u\[\033[0m\]\\$ "
        # My prompt line:       "[gaelicWizard:~/Documents] user$ " user is in blue
        # Or:                   "[gaelicWizard:~/Documents] root# " root is in blue
##
# Aliases:
##
# Aliases frequently used...
 alias ..='cd ..;l'
 alias cd..='cd ..'
 alias which='type' # 'which' in (t)csh is same(?) as 'type' in bash...
 alias quit='exit'
 alias q='quit' # and 'q' is even shorter! :-D
 alias v='vim'
 alias rehash='. ~/.bashrc;' # source ~/.bashrc after I edit it
 alias pg='ps -afe|grep -v grep|grep'
 alias make='gmake'
 alias patch='gpatch'
 alias sed='gsed'
 alias awk='nawk'
 alias diff='gdiff'
 alias grep='ggrep'
 alias find='gfind'
 alias ps='/usr/ucb/ps'
 alias whoami='/usr/ucb/whoami'
 alias ping='ping -s'
 alias man='GROFF_NO_SGR= TCAT="less -s" TROFF="groff -Tascii" man -t'


# The rest are uncategorised and fairly random... :-)
shopt -s histappend
PROMPT_COMMAND='echo -ne "\033]0;${USER} on ${HOSTNAME} at ${PWD}\007" && history -a'
export PATH=/usr/local/bin:/usr/local/sbin:/usr/xpg4/bin:/usr/sbin:/usr/bin:/usr/sfw/bin:/usr/ccs/bin:/usr/openwin/bin:/opt/mysql/current/bin
