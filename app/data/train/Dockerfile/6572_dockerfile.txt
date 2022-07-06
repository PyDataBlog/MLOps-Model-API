FROM alpine:3.2

# 依赖安装
RUN apk update \
    && apk add rsync \
    && rm -rf /var/cache/apk/*

# 配置
ADD rsyncd.conf /etc/rsyncd.conf
ADD entrypoint.sh /entrypoint.sh

# rsync 服务端口
EXPOSE 873

VOLUME ["/var/lib/clamav/"]

ENTRYPOINT ["/entrypoint.sh"]
