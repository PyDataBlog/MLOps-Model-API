#!/bin/bash


# resume export -f md buhay_john_resume-md
resume test || exit 1

#  Make all themes in html
# for e in $(echo elegant paper kendall flat modern classy class short slick kwan onepage); do resume export -f html -t $e buhay_john_resume-$e; done
for e in $(echo elegant kendall); do resume export -F -f html -t $e buhay_john_resume-$e; done

#  Make all themes in pdf
# for e in $(echo elegant paper kendall flat modern classy class short slick kwan onepage); do resume export -F -f pdf -t $e buhay_john_resume-$e; done


#  Fix css in kendall
sed -i.bak 's/\/\/maxcdn/http:\/\/maxcdn/g' buhay_john_resume-kendall.html
