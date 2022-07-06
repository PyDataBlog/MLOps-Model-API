# Start from a Debian image with the latest version of Go installed
# and a workspace (GOPATH) configured at /go.
FROM golang:1.4.2

RUN apt-get update && apt-get install -y \
	pkg-config rsync devscripts libkrb5-dev liblzma-dev libcap-dev
RUN apt-get install -y sudo

CMD ["bash"]

