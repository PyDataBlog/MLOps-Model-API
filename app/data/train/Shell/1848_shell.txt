if test "$#" -ne 1; then
    result=${PWD##*/}
else
    result=$1
fi

echo -ne '\033]0; '$result' \007'
