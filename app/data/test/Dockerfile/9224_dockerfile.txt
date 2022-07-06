FROM node:8

WORKDIR /usr/src/app

RUN apt-get update && apt-get install -y vim \
  && curl -o- -L https://yarnpkg.com/install.sh | bash \
  && npm install pm2 -g

#Copy all files into the working dir
COPY . ./

EXPOSE 3030

CMD ["/usr/src/app/deploy-app.sh"]