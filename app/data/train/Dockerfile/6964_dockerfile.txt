FROM golang:1.8-jessie

RUN mkdir /app

ADD main/ /app/

WORKDIR /app

RUN go build -o main .

CMD ["/app/main"]

EXPOSE 8000
