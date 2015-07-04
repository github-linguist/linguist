# vim: set ft=sh:

# encoding
export LC_CTYPE='en_US.UTF-8'
# terminal color
export TERM=xterm-256color
# prompt setup
current_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}
parse_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
PS1='${debian_chroot:+($debian_chroot)}\[\e[00;32m\]\u@\h:\[\e[01;34m\]\W\[\033[01;35m\]$(parse_branch)\[\e[01;35m\]\[\e[0m\] $ '

google () {
  search=""
  echo "Googling: $@"
  for term in $@; do
      search="$search%20$term"
  done
  xdg-open "http://www.google.com/search?q=$search"
}

# alias
alias emacs='emacs -nw'
alias eshell='emacs --execute "(term \"`which zsh`\")"'
 - () {
  cd -
}
alias ..='cd ..'
alias ...='cd ../..'
alias _=sudo
alias afind='ack-grep -il'
alias c=clear
alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias cd.....='cd ../../../..'
alias d='dirs -v | head -10'
alias emacs='emacs -nw'
alias g=git
alias ga='git add'
alias gap='git add --patch'
alias gb='git branch'
alias gba='git branch -a'
alias gbr='git branch --remote'
alias gc='git commit -v'
alias 'gc!'='git commit -v --amend'
alias gca='git commit -v -a'
alias 'gca!'='git commit -v -a --amend'
alias gcl='git config --list'
alias gclean='git reset --hard && git clean -dfx'
alias gcln='git clone'
alias gcm='git checkout master'
alias gcmsg='git commit -m'
alias gco='git checkout'
alias gcount='git shortlog -sn'
alias gcp='git cherry-pick'
alias gcs='git commit -S'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdt='git difftool'
alias gg='git gui citool'
alias gga='git gui citool --amend'
alias ggpnp='git pull origin $(current_branch) && git push origin $(current_branch)'
alias ggpull='git pull origin $(current_branch)'
alias ggpur='git pull --rebase origin $(current_branch)'
alias ggpush='git push origin $(current_branch)'
alias gmpush='git push wkentaro $(current_branch)'
alias gignore='git update-index --assume-unchanged'
alias gignored='git ls-files -v | grep "^[[:lower:]]"'
alias git=hub
alias git-svn-dcommit-push='git svn dcommit && git push github master:svntrunk'
alias gk='gitk --all --branches'
alias gl='git pull'
alias glg='git log --stat --max-count=10'
alias glgg='git log --graph --max-count=10'
alias glgga='git log --graph --decorate --all'
alias glo='git log --oneline --decorate --color'
alias globurl='noglob urlglobber '
alias glog='git log --oneline --decorate --color --graph'
alias glp=_git_log_prettily
alias gm='git merge'
alias gmt='git mergetool --no-prompt'
alias gp='git push'
alias gpoat='git push origin --all && git push origin --tags'
alias gr='git remote'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'
alias grmv='git remote rename'
alias grrm='git remote remove'
alias grset='git remote set-url'
alias grt='cd $(git rev-parse --show-toplevel || echo ".")'
alias grup='git remote update'
alias grv='git remote -v'
alias gsd='git svn dcommit'
alias gsps='git show --pretty=short --show-signature'
alias gsr='git svn rebase'
alias gss='git status -s'
alias gst='git status'
alias gsta='git stash'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gsts='git stash show --text'
alias gts='git tag -s'
alias gunignore='git update-index --no-assume-unchanged'
alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
alias gup='git pull --rebase'
alias gvt='git verify-tag'
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'
alias gwip='git add -A; git ls-files --deleted -z | xargs -r0 git rm; git commit -m "--wip--"'
alias h=history
alias history='fc -l 1'
alias ipy=ipython
alias sl='ls'
alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias lsa='ls -lah'
alias md='mkdir -p'
alias py=python
alias please=sudo
alias po=popd
alias pu=pushd
alias pyfind='find . -name "*.py"'
alias pygrep='grep --include="*.py"'
alias rd=rmdir
alias v=vim
alias vi=vim
alias which-command=whence
# hub
if which hub >/dev/null 2>&1; then
  eval "$(hub alias -s)"
fi
# open
if which open >/dev/null 2>&1; then
  alias o='open'
  alias o.='open .'
elif which gnome-open >/dev/null 2>&1; then
  alias open='gnome-open'
  alias o='gnome-open'
  alias o.='gnome-open .'
fi
# ls
if which dircolors >/dev/null 2>&1; then
  eval `dircolors $HOME/.colorrc`
  alias ls='ls --color=auto'
fi
 [[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator 

cd () {
  if [[ "x$*" = "x..." ]]
  then
    cd ../..
  elif [[ "x$*" = "x...." ]]
  then
    cd ../../..
  elif [[ "x$*" = "x....." ]]
  then
    cd ../../../..
  elif [[ "x$*" = "x......" ]]
  then
    cd ../../../../..
  elif [ -d ~/.autoenv ]
  then
    source ~/.autoenv/activate.sh
    autoenv_cd "$@"
  else
    builtin cd "$@"
  fi
}
