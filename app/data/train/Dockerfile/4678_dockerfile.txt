###### QNIBTerminal child
FROM qnib/terminal:fd22

ADD ./rpms/ /tmp/
RUN dnf install -y /tmp/openvenus-1.9.19-0.fedora22.x86_64.rpm

