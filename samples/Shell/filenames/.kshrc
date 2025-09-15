case "$-" in
    *i*) ;;
    *) return 0 ;;
esac

export PAGER=less
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8

HISTFILE=$HOME/.ksh_history
HISTSIZE=20000
HISTCONTROL=ignoredups

set -o emacs

alias ll='ls -l'
alias la='ls -lA'

PS1='\A $? \w \$ '

set -A complete_sysctl -- $(sysctl | grep = | cut -d= -f1)
