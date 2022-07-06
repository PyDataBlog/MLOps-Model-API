FROM python:2.7-slim
ADD . /src
WORKDIR /src
#ENV DEBIAN_FRONTEND noninteractive
#RUN apt-get update && \
#    apt-get -y install gcc mono-mcs && \
#    rm -rf /var/lib/apt/lists/*
RUN pip install -r requirements.txt
CMD python ./bot/app.py
