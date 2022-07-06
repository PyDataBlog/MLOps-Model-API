#!/bin/bash
# AUTHOR: Léo Perrin <leoperrin@picarresursix.fr>
# Time-stamp: <2013-04-16 22:35:22 leo>

# Append a pdf to the end of another.

if (( $# < 1 ))
then
    echo "Usage:"
    echo "   concat-pdf.sh 1.pdf 2.pdf"
    echo "Creates a pdf called 1-2.pdf made of the concatenation of the"\
         "content of 1.pdf and 2.pdf"
    echo ""
    echo "   concat-pdf.sh pdf-to-append.pdf"
    echo "Prompts for a pdf file and appends pdf-to-append.pdf at the end"\
         "of it. The result is stored in a new file."
elif (( $# < 2 ))
then
    main_pdf=$(zenity --text="The file "$1" will appended to:" \
        --file-selection --file-filter="*.pdf" --filename=$(pwd)/)
    out=$(sed "s/\.[^\.]*//" <<< $main_pdf)-$(sed "s/\.[^\.]*//" <<< $1).pdf
    pdftk $main_pdf $1 cat output $out
else
    out=$(sed "s/\.[^\.]*//" <<< $1)-$(sed "s/\.[^\.]*//" <<< $2).pdf
    pdftk $1 $2 cat output $out
fi
    

    
