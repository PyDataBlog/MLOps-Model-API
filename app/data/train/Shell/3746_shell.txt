#!/bin/sh
export GRAS_ROOT=/home/aviral/GNU-Niyantran
export PATH=/home/aviral/GNU-Niyantran/build/grex/network:$PATH
export LD_LIBRARY_PATH=/home/aviral/GNU-Niyantran/build/lib:/home/aviral/GNU-Niyantran/build/PMC/lib:/home/aviral/GNU-Niyantran/build/gr36/volk/lib:$LD_LIBRARY_PATH
export PYTHONPATH=/home/aviral/GNU-Niyantran/build/python:/home/aviral/GNU-Niyantran/build/python/gras:/home/aviral/GNU-Niyantran/build/python/gras/Release:/home/aviral/GNU-Niyantran/build/PMC/python:/home/aviral/GNU-Niyantran/build/PMC/python/PMC:/home/aviral/GNU-Niyantran/build/PMC/python/PMC/Release:$PYTHONPATH
export GRAS_MODULE_PATH=:/home/aviral/GNU-Niyantran/build/grex/math/libgrex_math.so:/home/aviral/GNU-Niyantran/build/grex/misc/libgrex_misc.so:/home/aviral/GNU-Niyantran/build/grex/network/libgrex_network.so
/usr/bin/python -B /home/aviral/GNU-Niyantran/grex/network/serialize_test.py 
