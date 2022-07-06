# docker-ubuntu-dev

Docker image with Ubuntu and essentials for C/C++ programming.

Automated builds on Docker Hub: https://hub.docker.com/r/mszg/docker-ubuntu-dev

## Build the image

```
docker build . -t mszg/docker-ubuntu-dev
```

## Download the image from the Docker Hub

```
docker pull mszg/docker-ubuntu-dev
```

## Run the image

```
docker run -it mszg/docker-ubuntu-dev:latest bash
```

## Run and mount the current directory

The current folder is mounted into the `mydir` director

```
docker run -it -v $(pwd):/mydir mszg/docker-ubuntu-dev:latest bash
```
