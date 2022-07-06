@echo off
set xv_path=C:\\Xilinx\\Vivado\\2016.4\\bin
call %xv_path%/xelab  -wto afbbc5bd067f4394946d7d70b6458e04 -m64 --debug typical --relax --mt 2 -L xil_defaultlib -L secureip --snapshot uint_to_ieee754_fp_behav xil_defaultlib.uint_to_ieee754_fp -log elaborate.log
if "%errorlevel%"=="0" goto SUCCESS
if "%errorlevel%"=="1" goto END
:END
exit 1
:SUCCESS
exit 0
