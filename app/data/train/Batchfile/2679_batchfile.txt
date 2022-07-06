@echo off
set xv_path=C:\\Xilinx\\Vivado\\2016.4\\bin
call %xv_path%/xelab  -wto 99d627bb7b3e47b6991e89c6529e9713 -m64 --debug typical --relax --mt 2 -L xil_defaultlib -L secureip --snapshot ieee754_fp_to_uint_behav xil_defaultlib.ieee754_fp_to_uint -log elaborate.log
if "%errorlevel%"=="0" goto SUCCESS
if "%errorlevel%"=="1" goto END
:END
exit 1
:SUCCESS
exit 0
