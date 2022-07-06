FROM nodesource/node:4.0    

RUN apt-get update
RUN apt-get -y install tree
RUN apt-get -y install tmux
RUN npm install pm2 -g

ADD package.json package.json

RUN npm install

ADD . .

EXPOSE 3000

## Start with npm
#CMD ["npm","start"]

## Start using PM2
CMD pm2 start --no-daemon app.js

# reference: https://nodesource.com/blog/dockerizing-your-nodejs-applications/
# usage: 
# docker build -t "docker-sails-app:0.1.0" .
# docker run -it -p 3000:3000 "docker-sails-app"
