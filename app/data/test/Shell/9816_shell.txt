#!/bin/bash
#
# Copyright (c) 2014 Alexandre Magno <alexandre.mbm@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# To install dependencies:
#   sudo apt-get install iconv dos2unix sed colordiff docdiff
#   sudo pip install csv2xlsx
#
# Tested on Ubuntu 14.04

touch google-docs-changed.txt
touch google-docs-suspect.txt

iconv -f ISO-8859-1 -t UTF-8 -o sisloc-target.csv sisloc-source.csv
dos2unix sisloc-target.csv &>/dev/null

cp sisloc-target.csv sisloc-todiff.csv

sed -i "s/\([ -\_]\)D\([oae]\)\([ -\_]\)/\1d\2\3/g" sisloc-target.csv
sed -i "s/  */ /g" sisloc-target.csv
sed -i "s/\t/ /g" sisloc-target.csv  # sem tabulações
sed -i "s/\([(\[]\) */\1/g" sisloc-target.csv
sed -i "s/\([)\]]\) */\1/g" sisloc-target.csv
sed -i "s/ [CDILMVXcdilmvx]* /\U&/g" sisloc-target.csv  # romanos?

#cat sisloc-target.csv

#colordiff -rupN sisloc-todiff.csv sisloc-target.csv | less -R
(
  echo "Passo 1 de 2 - Analise como as coisas mudaram:"
  echo "(copie linhas problemáticas para google-docs-changed.txt)"
  echo
  docdiff --char --tty sisloc-todiff.csv sisloc-target.csv |
    grep -n --color=always ""
) | less -R

function matched() {
  (grep -n --color=never -o -E \
    "([^-,.;0-9a-zA-Záéíóúâêîôûãõà\(\)]|[^ 0-9]\.[^ 0-9])" sisloc-target.csv |
    grep -v "^[0-9]*:[ \t]*$" |
    while read entry; do
      n=$(echo "$entry" | cut -d: -f1)
      s=$(echo "$entry" | cut -d: -f2)
      #echo "$n - $s"
      echo "$s"
    done) | sort -u
}

(
  echo "Passo 2 de 2 - Veja os POSSÍVEIS erros abaixo:"
  echo "(copie linhas problemáticas para google-docs-suspect.txt)"
  echo
  #echo "$(matched)"
  as_list=$(echo "$(matched)" | sed ':a;$!N;s/\n/|/;ta;')
  #echo "${as_list}"
  grep -n --color=always -E "(${as_list})" sisloc-target.csv
) | less -R

#  [CDILMVXcdilmvx ]+ [^AEOUBEFGHJNOPQRSTUZaeoubefghjnopqrstuz\-]+

echo "Não esqueça de salvar google-docs-changed.txt e google-docs-suspect.txt antes de continuar." | less

cp sisloc-target.csv sisloc-tdocs.csv
sed -i "s/$/\;no/g" sisloc-tdocs.csv
while read LINE; do
  n=$(echo "$LINE" | grep "^[ \t]*[0-9]*\:" | sed "s/^[ \t]*\([0-9]*\)\:.*/\1/g")
  [ -n "$n" ] && sed -i "${n}s/no$/changed/g" sisloc-tdocs.csv
done < google-docs-changed.txt
while read LINE; do
  n=$(echo "$LINE" | grep "^[ \t]*[0-9]*\:" | sed "s/^[ \t]*\([0-9]*\)\:.*/\1/g")
  [ -n "$n" ] && sed -i "${n}s/no$/suspect/g" sisloc-tdocs.csv
done < google-docs-suspect.txt
sed -i "1s/no$/revised\?/g" sisloc-tdocs.csv
sed -i "/^[ \t]*$/d" sisloc-tdocs.csv
cat sisloc-tdocs.csv | csv2xlsx --delimiter ";" UTF-8 data > sisloc-planilha.xlsx

echo "Faça upload de sisloc-planilha.xlsx no Google Drive optando por convertê-lo"
