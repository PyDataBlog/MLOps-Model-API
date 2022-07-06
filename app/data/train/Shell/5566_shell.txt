check() {
  if [ -d "~/.liquidprompt" ]; then
    return 1
  else
    return 0
  fi
}

install() {
  git clone https://github.com/nojhan/liquidprompt.git ~/.liquidprompt
}
