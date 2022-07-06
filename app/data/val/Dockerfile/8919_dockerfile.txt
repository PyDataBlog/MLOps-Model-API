FROM nyxcharon/docker-django-1.7
MAINTAINER Barry Martin - nyxcharon@gmail.com
RUN apt-get -y update && apt-get -y install \
    postgresql \
    redis-server
RUN pip install Celery pillow Coverage django-grappelli
EXPOSE 8000


