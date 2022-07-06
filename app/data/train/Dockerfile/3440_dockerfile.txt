FROM	java:latest

EXPOSE  8080
VOLUME	/git

RUN	cd / && curl -Lks -o gitblit-1.7.1.tar.gz "https://bintray.com/artifact/download/gitblit/releases/gitblit-1.7.1.tar.gz" && tar xzf gitblit-1.7.1.tar.gz && rm gitblit-1.7.1.tar.gz

WORKDIR	/gitblit-1.7.1
ADD	gitblit.properties data/
CMD	["java", "-jar", "gitblit.jar", "--baseFolder", "/gitblit-1.7.1/data"]
