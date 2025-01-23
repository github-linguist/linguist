#!/usr/bin/env bash

if (( ${rvm_ignore_rvmrc:=0} == 0 ))
then
  declare rvmrc

  rvm_rvmrc_files=("/etc/rvmrc" "$HOME/.rvmrc")
  if [[ -n "${rvm_prefix:-}" ]] && ! [[ "$HOME/.rvmrc" -ef "${rvm_prefix}/.rvmrc" ]]
     then rvm_rvmrc_files+=( "${rvm_prefix}/.rvmrc" )
  fi
  
  for rvmrc in "${rvm_rvmrc_files[@]}"
  do
    if [[ -f "$rvmrc" ]]
    then
      if GREP_OPTIONS="" \grep '^\s*rvm .*$' "$rvmrc" >/dev/null 2>&1
      then
        printf "%b" "
  Error:
    $rvmrc is for rvm settings only.
    rvm CLI may NOT be called from within $rvmrc.
    Skipping the loading of $rvmrc
"
        exit 1
      else
        source "$rvmrc"
      fi
    fi
  done
  unset rvm_rvmrc_files
  unset rvmrc
fi

export rvm_path
if [[ -z "${rvm_path:-}" ]]
then
  if (( UID == 0 )) && [[ -d "/usr/local/rvm" ]]
  then rvm_path="/usr/local/rvm"
  elif [[ -d "${HOME}/.rvm" ]]
  then rvm_path="${HOME}/.rvm"
  elif [[ -d "/usr/local/rvm" ]]
  then rvm_path="/usr/local/rvm"
  else echo "Can't find rvm install!" 1>&2 ; exit 1
  fi
fi

# allow disabling check temporary
: rvm_is_not_a_shell_function:${rvm_is_not_a_shell_function:=1}

# if to prevent fork-bomb
if source "${rvm_scripts_path:="$rvm_path/scripts"}/rvm"
then
  rvm "$@"
else
  echo "Error sourcing RVM!"  1>&2
  exit 1
fi
