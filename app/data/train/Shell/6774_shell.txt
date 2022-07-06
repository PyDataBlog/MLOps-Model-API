"START TIME:2015-12-01 04:13:15
END TIME:2015-12-01 04:12:15
START TIME:2015-12-01 04:12:15
END TIME:2015-12-01 04:10:15"

cat abc |cut -d':' -f2- | awk  '{system("date +\"%s\" --date=\"" $0 "\"")}' | paste -d"-" -  - |  bc

