FROM dnhsoft/confluence-base:5.x

ADD https://www.atlassian.com/software/confluence/downloads/binary/atlassian-confluence-5.5.7-x64.bin /confluence-install.bin

RUN /assets/install.sh

