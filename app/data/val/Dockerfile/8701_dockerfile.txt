FROM simplexsys/debian-zulu-jdk
MAINTAINER qsys <kurt.sys@gmail.com>

ENV LEIN_ROOT true
RUN curl -L -s https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein >  /usr/local/bin/lein &&\ 
		chmod 0755 /usr/local/bin/lein &&\
	   mkdir /var/cache/stuffiedo-config &&\
		lein upgrade 


WORKDIR /var/cache/stuffiedo-config
COPY ./ ./

ENTRYPOINT ["lein", "run"]
