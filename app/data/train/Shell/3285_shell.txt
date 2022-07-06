#!/bin/bash
#
# run_test.sh
#
# This script is use to test C-c compiler.
# Usage: ./run_test <PARTICULAR_TEST_INPUT>
#
# Author: cyng93
# Email:  cyng93<at>gmail.com


INPUT=$1
let VERBOSE=1
OUTPUT="${INPUT}.$(whoami)"
GOLD_ANS="${INPUT}.gold"

TEST_AVAILABLE=$(find . -iname "*.cm")
USAGE="\
 ~> invalid syntax !\n\n\
 Usage: ./run_test.sh <PARTICULAR_TEST>\n\
 Available test: \n\
${TEST_AVAILABLE} \n\n\
 To run all test, simply: \n\
   $ ls | grep .cm$ | awk '{system(\"./run_test.sh \"\$1)}'\
 "


if [ "$#" -ne 1 ]; then
    echo -e "${USAGE}"
    exit -1
fi

# Create soft link to parser if not exist
if [ ! -f 'parser' ]; then
    echo -e " ~> parser not found, trying to make it from upper directory..."
    WORKSPACE=$(pwd)
    cd ../../
    UPPER_DIR=$(pwd)
    make parser && ln -s ${UPPER_DIR}/parser ${WORKSPACE}/ && cd ${WORKSPACE}
fi

# Check if input file exist
if [ ! -f "${INPUT}" ]; then
    echo -e " ~>${INPUT} not found!"
    exit -1
fi

# Remove existed output file
if [ -f "${OUTPUT}" ]; then
    echo -e " ~> Please remove pre-existing '${OUTPUT}' !"
    rm -i ${OUTPUT}
fi

# Use parser to parse INPUT
./parser "${INPUT}" > "${OUTPUT}"

# Check if output.gold exist
if [ ! -f "${GOLD_ANS}" ]; then
    echo -e " ~> ${GOLD_ANS} not found!"
    echo -e "   ~> 'mv ${OUTPUT} ${GOLD_ANS}' if it's the desired answer for this particular test."
    exit -1
fi

echo -e " ~> Cross-check '${OUTPUT}' with '${GOLD_ANS}'..."
diff "${OUTPUT}" "${GOLD_ANS}"
retval=$?

if [ "${retval}" -eq "0" ]; then
    echo -e " ~> PASS!"
    rm ${OUTPUT}
else
    echo -e " ~> FAIL! ' vimdiff ${OUTPUT} ${GOLD_ANS} ' for detail !"
fi
