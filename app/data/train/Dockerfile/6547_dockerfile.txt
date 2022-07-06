From alpine:latest
MAINTAINER edward <edward@lxh.tech>
ENV LANG=C.UTF-8 \
LC_ALL=C.UTF-8 \
TIMEZONE=Asia/Shanghai \
ALPINE_VERSION=v3.6 \
#MAX_RAM=1GB \
MONGO_VERSION=3.4.4-r0 \
MONGO_TOOLS_VERSION=3.4.4-r2

ENV MONGO_CONF=https://github.com/mongodb/mongo/blob/r${MONGO_VERSION}/rpm/mongod.conf

RUN echo "http://mirrors.aliyun.com/alpine/${ALPINE_VERSION}/main" > /etc/apk/repositories && \
echo "http://mirrors.aliyun.com/alpine/${ALPINE_VERSION}/community" >> /etc/apk/repositories && \
echo "https://mirror.tuna.tsinghua.edu.cn/alpine/${ALPINE_VERSION}/community" >> /etc/apk/repositories && \
echo "https://mirror.tuna.tsinghua.edu.cn/alpine/${ALPINE_VERSION}/main" >> /etc/apk/repositories && \
echo "install apk software" && \
apk upgrade --update-cache && \
apk add --no-cache --update-cache bash curl ca-certificates libstdc++ tzdata wget mongodb=${MONGO_VERSION} mongodb-tools=${MONGO_TOOLS_VERSION} && \
echo "export LANG=${REGION}.${REGION_CODE}" > /etc/profile.d/startup.sh && \
echo "export LC_ALL=${REGION}.${REGION_CODE}" >> /etc/profile.d/startup.sh && \
cp /usr/share/zoneinfo/${TIMEZONE} /etc/localtime && \
echo "${TIMEZONE}" >  /etc/timezone && \
mkdir -p  /data/db && \
mkdir -p  /data/conf && \
echo "net.core.somaxconn = 3072" >> /etc/sysctl.conf && \
echo "net.ipv4.tcp_max_syn_backlog = 4096" >> /etc/sysctl.conf && \
echo "net.ipv4.conf.default.rp_filter = 0" >> /etc/sysctl.conf && \
echo "net.ipv4.tcp_keepalive_time = 120" >> /etc/sysctl.conf && \
echo "fs.file-max = 2097152" >> /etc/sysctl.conf && \
chmod -R 775 /data/db && \
#chmod -R 775 /data/db && \
touch /data/conf/mongod.conf

#curl -Lo  /data/conf/mongod.conf ${MONGO_CONF}
ENV TIMEZONE= \
ALPINE_VERSION= \
MONGO_VERSION= \
MONGO_TOOLS_VERSION= \
MONGO_CONF= 

EXPOSE 27017 28017
CMD ["mongod","--config","/data/conf/mongod.conf"]

