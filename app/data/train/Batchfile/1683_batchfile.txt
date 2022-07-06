rmdir /q /s build
del preload\dir1\dir1.cpp
del preload\dir1\dir1.cp35-win_amd64.pyd
rmdir /q /s preload\dir1\__pycache__
del preload\dir2\dir2.cpp
del preload\dir2\dir2.cp35-win_amd64.pyd
rmdir /q /s preload\dir2\__pycache__
rmdir /q /s preload\__pycache__
del dir1.i
del dir2.i
