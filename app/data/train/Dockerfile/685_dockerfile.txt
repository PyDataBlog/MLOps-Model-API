
FROM golang:1.4.0-wheezy
MAINTAINER EMCCODE

WORKDIR /output

ENTRYPOINT go clean -i net && go install -tags netgo std && CGO_ENABLED=0 go get -a -ldflags '-s' $REPO_PATH && cp -f $GOPATH/bin/* /output/.



