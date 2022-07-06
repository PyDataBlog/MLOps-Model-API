#generate a pseudorandom passphrase of $1 words

generate_passphrase() {
	shuf -n ${1:-5} /usr/share/dict/words | tr '\n' ' ' | head -c -1
	echo
}

