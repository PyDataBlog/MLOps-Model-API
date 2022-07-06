#!/bin/bash

. build.properties

clear

CURRENT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
BACKEND_DIR="$CURRENT_DIR/asciidoctor-backends/slim/dzslides"

echo "Copying DZSlides backend files..."
cp -r dzslides $1

echo "Asciidoctor compilation..."
cd $1
asciidoctor -b dzslides -T $BACKEND_DIR -a linkcss -a data-uri slides.adoc

echo "Exporting presentation to PDF..."
$deck2pdf/deck2pdf --profile=dzslides --width=954 --height=714 slides.html slides.pdf

echo "Removing DZSlides backend files..."
rm -rf dzslides
rm asciidoctor.css
cd -