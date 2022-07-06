CC = clang
CFLAGS  = -lstdc++

# typing 'make' will invoke the first target entry in the file 
# (in this case the default target entry)
# you can name this target entry anything, but "default" or "all"
# are the most commonly used names by convention
#
default: mapper_driver

# To create the executable file count we need the object files
# customer.o, lease.o, and asset.o:
mapper_driver: customer.o lease.o asset.o pricing_mapper.o pricing_strategy.o mapper_driver.o
	$(CC) $(CFLAGS) -o mapper_driver customer.o lease.o asset.o pricing_mapper.o pricing_strategy.o mapper_driver.o

# To create the object file customer.o, we need the source file customer.cc
customer.o: customer.cc
	$(CC) $(CFLAGS) -c customer.cc

# To create the object file lease.o, we need the source file lease.cc
lease.o:  lease.cc 
	$(CC) $(CFLAGS) -c lease.cc

# To create the object file asset.o, we need the source file asset.cc 
asset.o:  asset.cc
	$(CC) $(CFLAGS) -c asset.cc

# To create the object file asset.o, we need the source file pricing_mapper.cc 
pricing_mapper.o: pricing_mapper.cc
	$(CC) $(CFLAGS) -c pricing_mapper.cc	

# To create the object file pricing_strategy.o, 
# we need the source file pricing_strategy.cc 
pricing_strategy.o: pricing_strategy.cc
	$(CC) $(CFLAGS) -c pricing_strategy.cc

# To create the object file asset.o, we need the source file pricing_mapper.cc 
mapper_driver.o: mapper_driver.cc
	$(CC) $(CFLAGS) -c mapper_driver.cc		

# To start over from scratch, type 'make clean'.  This
# removes the executable file, as well as old .o object files.
clean: 
	$(RM) mapper_driver *.o