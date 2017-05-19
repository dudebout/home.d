# Imperfect as it also returns functions defined in ~/.zprofile, etc.
fins-which () {
  local fun=$1

  (source ~/.home.d/fins/funs.d/* && which $fun)
}
