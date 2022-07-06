#!/bin/bash

############################################################
#  Delete table before exporting data from Hive.           #
# ---------------------------------------------------- --- #
# Sqoop version: 1.4.6  +                                  #
# @author Hefei Li
# @since Feb. 29, 2016
############################################################


# check parameters: 
if (("$#" != 4))
then
  echo "$(date) error: Schema and start, end range must be assigned. And the start must be less then the end."
  echo "Usage: ./<table_name> <partition_column> <1> <1>"
  exit 1
fi

table=$1
dt_column=$2
start=$3
end=$4

echo "====$(date) Range is [${start}, ${end}] ===="

for ((i=${start}; i<=${end}; i++))
do
  dt=$(date +%Y%m%d -d -${i}day)

  # delete MySQL table data before export data.
  echo "$(date) delete data to MySQL table [${table}] where date in [${dt}].... "
  sqoop eval --connect jdbc:mysql://10.148.10.66:3306/phone_dashboard \
	  --username scloud-dev --password {MY_PASSWORD}  \
	  --query "delete from ${table} where ${dt_column}='${dt}'"
done