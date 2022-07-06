FROM debian:jessie
MAINTAINER John Downey <jdowney@gmail.com>

RUN apt-get update && apt-get install -y \
  build-essential \
  ca-certificates \
  curl \
  file \
  sudo \
  --no-install-recommends \
  && rm -rf /var/lib/apt/lists/*

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y -v
RUN /root/.cargo/bin/rustup target add x86_64-unknown-linux-musl

ENV PATH="/root/.cargo/bin:${PATH}"
