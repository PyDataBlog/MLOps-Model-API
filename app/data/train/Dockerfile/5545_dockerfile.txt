FROM igeolise/ubuntu:16.04

RUN apt-get update && \
    apt-get install -y build-essential && \
    curl -sL https://deb.nodesource.com/setup_6.x | bash - && \
    apt-get install -y nodejs && \
    apt-get install -y --no-install-recommends git && \
    rm -rf /var/lib/apt/lists/*

RUN git clone --branch v3.0.0-beta.25 --depth 1 https://github.com/uikit/uikit /home/user/uikit/ && \
    cd /home/user/uikit/ && \
    npm install
