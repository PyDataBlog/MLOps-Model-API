FROM node:6.9-alpine

RUN apk update && apk upgrade
RUN apk add --update build-base
RUN apk add --update python python-dev py-pip
RUN apk --no-cache add ca-certificates && update-ca-certificates
# ENV NODE_ENV production
ENV NODE_ENV development
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

ARG NODE_ENV
ENV NODE_ENV $NODE_ENV
COPY package.json /usr/src/app/
RUN npm install node-gyp
RUN npm install && npm cache clean
COPY . /usr/src/app

CMD [ "npm", "start" ]
