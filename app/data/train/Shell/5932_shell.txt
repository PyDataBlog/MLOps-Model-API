#This shell script is used to generate cscope.out for gcc project
#especially for the nRF51 project above the SDK 8.
#Call the script at the Makefile directory.
#Don't forget add below script at the end of Makefiel:
#print-%:
#    @echo '$($*)'

#!/bin/bash

current_path=`pwd`

inc_paths=`make -f Makefile print-INC_PATHS | sed -e 's/-I/ /g'`

find $inc_paths -type f -name "*.h" | sed -e "s#^#${current_path}\/#g" > ./cscope.files

c_source_files=`make -f Makefile print-C_SOURCE_FILES`
#use find here is to add newline as each file separator
find $c_source_files -type f -name "*.c" -or -name "*.cpp" | sed -e "s#^#${current_path}\/#g" >> ./cscope.files

cscope -bq
