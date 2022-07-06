FC := gfortran
DEBUGWARNINGS := -Wall -Wextra -Wconversion-extra -Wcharacter-truncation -Wimplicit-interface -Wimplicit-procedure -Wno-tabs -Wfunction-elimination -Warray-bounds -Wcharacter-truncation -Wrealloc-lhs-all
FFLAGS := -g -O3 -ffree-form -ffree-line-length-none -fimplicit-none -fcheck=all -std=f2008 -pedantic $(DEBUGWARNINGS) $(FFLAGS)

test.x: test.F90
	$(FC) $(FFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -vf $(wildcard *.x) $(wildcard *.o) $(wildcard *.mod) $(wildcard *.a) $(wildcard *.so)
	rm -rvf $(wildcard *.x.dSYM)
