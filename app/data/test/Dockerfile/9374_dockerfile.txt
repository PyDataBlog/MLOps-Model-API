FROM centos:latest

# Install development tools
RUN yum -y update \
 && yum -y groupinstall 'Development Tools' 
 
# Create a non-root user
RUN useradd -m -p password gambler
 
# Change to non-root user
USER gambler
ENV HOME /home/gambler
WORKDIR $HOME

# Install and build CDataFile  
RUN git clone https://github.com/dmaddalone/CDataFile.git CDataFile 
WORKDIR $HOME/CDataFile
RUN make -f CDataFile.cbp.mak.unix clean \
 && make -f CDataFile.cbp.mak.unix all

# Install and build CrapSim
WORKDIR $HOME
RUN git clone https://github.com/dmaddalone/CrapSim.git CrapSim 
WORKDIR $HOME/CrapSim
RUN make -f CrapSim.cbp.mak.unix clean \
 && make -f CrapSim.cbp.mak.unix all

# Execute CrapSim
CMD $HOME/CrapSim/bin/Release/CrapSim $HOME/CrapSim/CrapSim.ini