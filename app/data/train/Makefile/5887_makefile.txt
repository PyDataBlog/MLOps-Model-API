include ../../../Makefile.inc

CLC_ROOT = ../../../inc

all: exe_src exe_src_m2s exe_bin amd_compile m2c_compile run_m2s run_native check_result

exe_src:
	@-$(CC) $(CC_FLAG) *src.c -o exe_src $(CC_INC) $(CC_LIB) > equal_uint16uint16.exe_src.log 2>&1

exe_src_m2s:
	@-$(CC) $(CC_FLAG) -m32 *src.c -o exe_src_m2s $(CC_INC) $(M2S_LIB) > equal_uint16uint16.exe_src_m2s.log 2>&1

exe_bin:
	@-$(CC) $(CC_FLAG) *bin.c -o exe_bin $(CC_INC) $(CC_LIB) > equal_uint16uint16.exe_bin.log 2>&1

amd_compile:
	@-$(AMD_CC) --amd --amd-dump-all --amd-device 11 equal_uint16uint16.cl > equal_uint16uint16.amdcc.log 2>&1
	@-rm -rf /tmp/*.clp && rm -rf /tmp/*_amd_files

m2c_compile:
	@-python compile.py

run_m2s:
	@-M2S_OPENCL_BINARY=./equal_uint16uint16.opt.bin $(M2S) --si-sim functional ./exe_src_m2s > m2s.log 2>&1

run_native:
	@-./exe_src > native.log 2>&1

check_result:
	@-diff exe_src.result exe_src_m2s.result > check.log 2>&1

clean:
	rm -rf exe_src exe_src_m2s exe_bin *.ll *.log *files *bin *.s *.bc *.result
