# prolog
sudo apt-get -y install software-properties-common
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt-get -y update
sudo apt-get -y install swi-prolog

# mono
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
sudo apt-get -y update
sudo apt-get -y install mono-devel