
LIBCONFIG_OBJ = libconfig.o grammar.o scanctx.o scanner.o strbuf.o
LIBCONFIG_H = libconfig.h grammar.h parsectx.h scanctx.h scanner.h strbuf.h wincompat.h



#####################################################################
.PHONY : all clean help

all: $(LIBCONFIG_OBJ)

clean:
	@echo "	CLEAN	libconfig"
	@rm -rf *.o

help:
	@echo "possible targets are 'all' 'clean' 'help'"
	@echo "'all'    - builds $(LIBCONFIG_OBJ)"
	@echo "'clean'  - deletes $(LIBCONFIG_OBJ)"
	@echo "'help'   - outputs this message"

#####################################################################

%.o: %.c $(LIBCONFIG_H)
	@echo "	CC	$<"
	@gcc -g -O2 -DMAXCONN=16384 -pipe -ffast-math -Wall -march=i686 -DHAS_TLS -Wno-switch -fno-strict-aliasing -DHAVE_SETRLIMIT -DHAVE_STRNLEN  -Wno-pointer-sign -DHAVE_MONOTONIC_CLOCK -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -D__EXTENSIONS__=1 -D_ALL_SOURCE=1 -D_GNU_SOURCE=1 -D_POSIX_PTHREAD_SEMANTICS=1 -D_TANDEM_SOURCE=1 -DHAVE_USELOCALE=1 -DHAVE_NEWLOCALE=1 -DHAVE_FREELOCALE=1 -DHAVE_XLOCALE_H=1 -DHAVE_LIBZ=1 -DHAVE_LIBPTHREAD=1 -DHAVE_LIBPTHREAD=1 -DHAVE_LIBPTHREAD=1 -DHAVE_LIBPTHREAD=1 -DHAVE_LIBPTHREAD=1 -DHAVE_LIBPTHREAD=1 -DHAVE_LIBPTHREAD=1  -I../common -I/usr/include -c $(OUTPUT_OPTION) $<
