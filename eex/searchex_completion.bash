#!/usr/bin/env bash

# searchex_completion.bash

# Tab-completion for `searchex`

# To install: copy this script to `/etc/bash_completion.d` (or equivalent)
# eg:
#    $ searchex completion > /etc/bash_completion.d/searchex_completion.bash
#    $ chmod a+rx /etc/bash_completion.d/searchex_completion.bash

_searchex() {
  COMPREPLY=()
  local word="${COMP_WORDS[COMP_CWORD]}"

  case $COMP_CWORD in 
    1)
      local commands="$(compgen -W "$(searchex all_commands)" -- "$word")"
      COMPREPLY=( $commands )
      ;;
    2)
      local words=("${COMP_WORDS[@]}")
      unset words[0]
      local cmd="${words[1]}"
      if [[ " $(searchex cfg_commands) " =~ "$cmd" ]] ; then
        local completions=$(searchex cfg_ls)
        COMPREPLY=( $(compgen -W "$completions" -- "$word") )
      fi
      ;;
  esac
}

complete -F _searchex searchex shx
