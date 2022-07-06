FROM nginx
MAINTAINER Dengcheng Yan "yandengcheng@gmail.com"

ADD default.conf /etc/nginx/conf.d/default.conf

ENV HEXO_VERSION 3.1.1

RUN apt-get update && apt-get install -y curl
RUN curl -sL https://deb.nodesource.com/setup | bash - && apt-get update && apt-get install -y curl git nodejs
RUN npm install -g hexo@${HEXO_VERSION}

RUN hexo init /usr/share/nginx/hexo
WORKDIR /usr/share/nginx/hexo
RUN npm install
RUN git clone https://github.com/iissnan/hexo-theme-next themes/next

VOLUME /usr/share/nginx/hexo

CMD hexo generate && nginx -g "daemon off;"