fins-which () {
  local cmd="$1"

  local exe
  exe=$(which "$cmd")

  if [[ "$exe" =~ "$HOME_D/fins/bin" ]]; then
      local fun
      fun=$(sed -n 's/source "\([^"]\+\)"/\1/p' "$exe")
      if (( ${+FINS_WHICH_ALL} )); then
          cat "$fun"
      else
          (source "$fun" && which "$cmd")
      fi
  else
      echo "$exe"
  fi
}

fins-which-all () {
    local cmd="$1"

    FINS_WHICH_ALL=1 fins-which "$cmd"
}

fins-gen () {
    local fins_gen=$HOME_D/fins/fins-gen
    $fins_gen
    local profile=$HOME_D/profile/fins
    if [[ -d $profile ]]; then
        FINSDIR=$profile $fins_gen
    fi
}
