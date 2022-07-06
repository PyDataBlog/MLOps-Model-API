OBJS = calc tempcalc testbuild 
p1: $(OBJS)
calc:
	g++ calc.cc -o calc
	./calc
tempcalc:
	g++ tempcalc.cc -o tempcalc
	./tempcalc
testbuild:test.cc
	g++ test.cc -o testbin
	./testbin 
clean:
	rm -f testbin
	rm -f *.o
	rm -f *~  
	rm -f calc tempcalc
.PHONY:calc tempcalc

