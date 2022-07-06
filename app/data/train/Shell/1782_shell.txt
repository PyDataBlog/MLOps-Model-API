echo "Please input page size (KB): "
read page
echo "Please input reference address : "
read address
echo "Page: $page ; Reference Address: $address"
p=$((page * 1024))
echo "Page: $p ; Reference Address: $address"
pagenumber=$((address / p))
offset=$(( address - pagenumber*p))
fragmentation=$((p - offset))


echo "Page Number   : $pagenumber"
echo "Offset        : $offset"
echo "Fragmentation : $fragmentation"


