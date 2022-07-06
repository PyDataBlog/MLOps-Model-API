#!/bin/bash
for filename in public/comparison/pdfs/*.html; do
  pdffilename=${filename/.html/.pdf}
  pdffilename=${pdffilename/public/source}
  echo "👉  Generating PDF for $filename..."

  wkhtmltopdf --margin-left 0 --margin-right 0 --margin-top 0 --zoom 3 $filename $pdffilename

  echo "✅  PDF pdffilename generated!"
done
