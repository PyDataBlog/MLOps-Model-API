FROM ubuntu:14.04
RUN sudo apt-get update -y && sudo apt-get install --no-install-recommends -y -q curl
RUN mkdir /nodejs && curl http://nodejs.org/dist/v0.10.36/node-v0.10.36-linux-x64.tar.gz | tar xvzf - -C /nodejs --strip-components=1
ENV PATH $PATH:/nodejs/bin
ENV DIIA_PORT 80

COPY package.json .
COPY web/* web
COPY server.js .

RUN npm install
EXPOSE 80

#CMD node -v
CMD npm start
