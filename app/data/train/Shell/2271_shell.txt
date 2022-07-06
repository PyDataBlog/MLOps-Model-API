#!/bin/bash
#Se busca para compilar MESOS

#Para tener idea de a que hora paso algo
fechaLog=`date "+%Y_%m_%d_%H_%M"`
echo "Iniciando a las ${fechaLog}"

formatoH="+%Y/%m/%d %H: %M: %S"

touch LogMesos_${fechaLog}.txt
echo `date "${formatoH}"` ": iniciando Descarga" >> LogMesos_${fechaLog}.txt

echo "iniciando Descarga"
wget http://www.apache.org/dist/mesos/0.27.1/mesos-0.27.1.tar.gz

echo `date "${formatoH}"` ": Finalizada la Descarga" >> LogMesos_${fechaLog}.txt


echo "extrac pakage"
tar -xzf mesos-*.tar.gz
rm mesos-*.tar.gz ; mv mesos-* mesos

echo `date "${formatoH}"` ": Extraido" >> LogMesos_${fechaLog}.txt

cd mesos

echo "Buildin Dir Buil"
mkdir build
cd build
sh ../configure
echo `date "${formatoH}"` ": Finalizado configure" >> ../../LogMesos_${fechaLog}.txt


echo "making pakage"
#con make -j 2 # para compilar con n procesodores
make -j 2
echo `date "${formatoH}"` ": Realizado Make" >> ../../LogMesos_${fechaLog}.txt

#Para poder colocar la contraseña sin tener que escribir 
#expect
make check
echo "making check"

echo `date "${formatoH}"` ": Realizado check" >> ../../LogMesos_${fechaLog}.txt

sudo make install

#para que me cargue las librerias
sudo ldconfig
echo "Install end --"

echo `date "${formatoH}"` ": Insalado" >> ../../LogMesos_${fechaLog}.txt
