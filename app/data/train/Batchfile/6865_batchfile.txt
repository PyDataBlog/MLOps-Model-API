@ECHO on
java -jar \encoder.jar -i "%~1"
copy inject.bin \
pause