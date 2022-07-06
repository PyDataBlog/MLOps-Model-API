#!/bin/bash

ALEPHDIR=/cvmfs/aleph.cern.ch
# GCCDIR=/usr/lib/gcc/x86_64-redhat-linux/4.1.1/32
GCCDIR=/usr/lib/gcc/i386-redhat-linux/3.4.6

g77 -m32 -w -o MIT $ALEPHDIR/Linux/phy/qmain.o $ALEPHDIR/Linux/phy/qusig.o .//MIT.o -I$ALEPHDIR/reference/cvs/inc -I$ALEPHDIR/reference/cvs/alpha126/inc -Wl,-u,ftrack_,-u,hmfind_,-u,mpredg_,-u,mpredm_,-u,muass_ -Wl,-u,aboldr_,-u,babend_,-u,dafrds_ -L$ALEPHDIR/Linux/lib -L$ALEPHDIR/Linux/phy -L$ALEPHDIR/Linux/jul -L$ALEPHDIR/Linux/gen -L $GCCDIR -lalpha126 -ljulia -lalephlib -lbos77 `cernlib  mathlib  packlib_noshift `
