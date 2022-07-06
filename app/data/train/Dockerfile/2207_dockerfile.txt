#runadock-itworks
#docker build -t runadock/itworks .
#docker run -d --name itworks -p 80:80 -p 443:443 runadock/itworks

FROM nginx
COPY src /usr/share/nginx/html
