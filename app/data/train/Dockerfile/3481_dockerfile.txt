# DOCKER-VERSION 1.2.0
# VERSION 0.2

FROM zooniverse/kafka
MAINTAINER Edward Paget <ed@zooniverse.org>

RUN echo "#!/bin/bash \n./opt/kafka_$SCALA_VERSION-$KAFKA_VERSION/bin/kafka-topics.sh \$@" > /usr/local/bin/kafka-topics && \
    chmod +x /usr/local/bin/kafka-topics

ENTRYPOINT ["kafka-topics"]
