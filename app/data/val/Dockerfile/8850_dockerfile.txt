FROM node:8.2.1
RUN mkdir -p /SyedBrand1FE
RUN     apt-get -y update && apt-get -y install \
        vim
RUN npm install -g serve
RUN chown -R node:node /SyedBrand1FE
USER node
COPY ./package.json /SyedBrand1FE/package.json
RUN cd /SyedBrand1FE && npm install
WORKDIR /SyedBrand1FE
COPY ./ /SyedBrand1FE
RUN npm run build
CMD ["npm","run","serve"]
EXPOSE 5000
