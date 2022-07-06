mkdir -p ..\frogs
vipsthumbnail -c -o ..\frogs/%s-128x128.jpg[Q=30] *.jpg
vipsthumbnail --size=180x90 -c -o ..\frogs/%s-180x90.jpg[Q=30] *.jpg
                                         
