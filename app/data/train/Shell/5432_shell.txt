#!/bin/bash
# Author: Eason Yi
# Date: 2017-05-17

echo "==============================================="
echo "= Usage: $0 [src_dir] src_file_ext1 ext2 ..."
echo "= e.g. $0 java js c h cpp lua go php cs"
echo "==============================================="
echo

if [[ -d "$1" ]];then
  src_dir=$1
  shift 1
  for p in $@; do
    src_file_ext="$src_file_ext -name \"*.${p}\" -o "
  done
  src_file_ext=${src_file_ext%-*}
else
  src_dir="."
  src_file_ext="java"
fi

cmd_files="find $src_dir -type f ${src_file_ext}|wc -l|tail -1|awk '{print \$1}'"
cmd_lines="find $src_dir -type f ${src_file_ext}|xargs wc -l|sort|tail -1|awk '{print \$1}'"

total_src_files=$(eval $cmd_files)
total_src_lines=$(eval $cmd_lines)

echo "Summary:"
echo "- 1) $cmd_files"
echo "      >>> Total Files: $total_src_files"
echo "  2) $cmd_lines"
echo "-     >>> Total Lines: $total_src_lines"

