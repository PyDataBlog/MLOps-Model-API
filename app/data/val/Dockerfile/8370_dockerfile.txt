FROM golang:alpine

ADD src/github.com/ciena/maas-flow src/github.com/ciena/maas-flow
ENV GO15VENDOREXPERIMENT 1
RUN go install github.com/ciena/maas-flow

ENTRYPOINT [ "./bin/maas-flow" ]


