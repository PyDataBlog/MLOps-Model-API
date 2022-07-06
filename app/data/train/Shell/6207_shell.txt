#Instalar Webmin
#Si es basado en RPM entonces
MESSAGE="Instalar Webmin para administrar nuestro equipo de manera remota" ; cyan_message
touch /etc/yum.repos.d/webmin.repo
MESSAGE="Generando repositorio para Webmin" ; cyan_message
cat >/etc/yum.repos.d/webmin.repo<<EOF
[Webmin]
name=Webmin Distribution Neutral
#baseurl=http://download.webmin.com/download/yum
mirrorlist=http://download.webmin.com/download/yum/mirrorlist
gpgcheck=0
enabled=1
EOF
#Luego descargamos la verificación PGP
wget ttp://www.webmin.com/jcameron-key.asc && rpm --import jcameron-key.asc
#Instalamos posteriormente el webmin
yum install webmin
#Si es basado en DEB entonces
