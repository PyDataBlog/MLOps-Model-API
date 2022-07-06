# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.205.2/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version (use hirsuite or bionic on local arm64/Apple Silicon): hirsute, focal, bionic
ARG VARIANT="hirsute"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN echo 'deb http://security.ubuntu.com/ubuntu bionic-security main' >> /etc/apt/sources.list \
    && apt update && apt-cache policy libssl1.0-dev \
    && apt-get install -y libssl1.0-dev