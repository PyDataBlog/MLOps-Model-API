if [[ ! -o interactive ]]; then
    return
fi

compctl -K _atlas atlas

_atlas() {
  local word words completions
  read -cA words
  word="${words[2]}"

  if [ "${#words}" -eq 2 ]; then
    completions="$(atlas commands)"
  else
    completions="$(atlas completions "${word}")"
  fi

  reply=("${(ps:\n:)completions}")
}
