fins-which () {
  local cmd=$1

  local exe
  exe=$(which "$cmd")

  if [[ "$exe" =~ "$HOME_D/fins/bin" ]]; then
      local fun
      fun=$(sed -n 's/source "\([^"]\+\)"/\1/p' "$exe")
      (source "$fun" && which "$cmd")
  else
      echo "$exe"
  fi
}
