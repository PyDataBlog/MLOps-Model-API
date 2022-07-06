REM the following works for networked drives with spaces!!!
REM Net use p: "\\myNetwork\HPS Classes$\computer programming 2011\Assignments" /persistent:no


REM dir/x gives the short file names.
REM you need to get these paths correct for nodejs, ARM, spark-cli, dfu files
REM NPM_PATH and SparkCLI_PATH may be the same or different.

set NODE_PATH=c:\progra~1\nodejs;
set NPM_PATH=c:\progra~1\APPLIC~1\npm;
set SPARKCLI_PATH=c:\progra~1\APPLIC~1\npm;
set DFU_FILES_PATH=c:\progra~1\AEE653~1\cp12\spark\dfu;
set ARM_PATH= c:\AEE653~1\cp12\spark\df


set PATH=C:\Windows;C:\Windows\System32;$NODE_PATH;$ARM_PATH;$SPARKCLI_PATH;$DFU_FILES_PATH


REM note the next line overwrites the myoutput using > normally >> adds to the file

echo "*************************************" > myoutput.txt 2>&1
echo "$PATH" >> myoutput.txt 2>&1
$PATH    >> myoutput.txt 2>&1





echo "*************************************" > myoutput.txt 2>&1
echo "node --version" >> myoutput.txt 2>&1
node --version    >> myoutput.txt 2>&1





echo "*************************************" >> myoutput.txt 2>&1
echo "npm --version " >> myoutput.txt 2>&1
npm --version    >> myoutput.txt 2>&1




echo "*************************************" >> myoutput.txt 2>&1
echo "spark-cli -version " >> myoutput.txt 2>&1
spark-cli --version   >> myoutput.txt 2>&1




echo "*************************************" >> myoutput.txt 2>&1
echo "$DFU_FILES_PATH/dir " >> myoutput.txt 2>&1
$DFU_FILES_PATH/dir    >> myoutput.txt 2>&1



echo "*************************************" >> myoutput.txt 2>&1
echo "GCC ???? " >> myoutput.txt 2>&1
dir    >> myoutput.txt 2>&1





echo "*************************************" >> myoutput.txt 2>&1
echo "spark flash --usb cc3000 " >> myoutput.txt 2>&1
spark flash --usb cc3000    >> myoutput.txt 2>&1



