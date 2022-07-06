FROM develop/javac

RUN apt-get update && apt-get -y install ant && rm /var/lib/apt/lists/* -rf

ENTRYPOINT ["ant"]
