##
# Environment...
##
# Set up some variables for 'screen'
if [ -z "${SCREENDIR}" ];then echo -n
        export SCREENDIR="${HOME}/.screen"
                # Save my screen sockets within my $HOME dir
fi
## PATH
export PATH=/usr/local/bin:/usr/local/sbin:/usr/xpg4/bin:/usr/sbin:/usr/bin:/usr/sfw/bin:/usr/ccs/bin:/usr/openwin/bin:/opt/mysql/current/bin
export MANPATH=/usr/local/man:/usr/share/man

## Random ENV...
# Set $TERM to 'vt100' (a safe default) if, for some
# reason, it is set to 'network' (which is not valid!)
if [ ${TERM} == 'network' ];then echo -n
        export TERM='vt100'
                # not 'nsterm' because if its 'network' we're
                # probly not in Terminal.app
fi
# Set $COLORTERM, all this does is trick *some* apps into
# using color in the terminal, which should happen anyway.
if [ -z "${COLORTERM}" ];then echo -n
        export COLORTERM="${TERM}"
fi
# another color option, this one for BSD's ls
if [ -z "${CLICOLOR}" ];then echo -n
        export CLICOLOR='1' # can be set to anything, actually
fi
# If $DISPLAY is not already set, set it!
if [ -z "${DISPLAY}" ];then echo -n
        export DISPLAY=':0'
fi
##
# Source the bash rc file
[ -r "${HOME}/.bashrc" ] && . "${HOME}/.bashrc"
