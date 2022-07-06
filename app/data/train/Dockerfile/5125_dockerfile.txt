FROM develop/npm

RUN npm install -g grunt-cli

ENTRYPOINT ["grunt"]
