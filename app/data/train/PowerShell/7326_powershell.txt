$i = 1
do {
    python create-gdb.py -n file-gdb$i -t FILEGDB -p "C:\\temp\\databases\\loading-lines"
    python create-gdb.py -n sqlite-gdb$i -t ST_GEOMETRY -p "C:\\temp\\databases\\loading-lines"
    python create-gdb.py -n spatialite$i -t SPATIALITE -p "C:\\temp\\databases\\loading-lines"
    $i++
}
until($i -gt 3)