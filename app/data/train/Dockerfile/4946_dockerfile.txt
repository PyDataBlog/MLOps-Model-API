# Dockerfile for a Rails application using Apache and Passenger

FROM bhzunami/rails_base:latest
MAINTAINER N. Mauchle "nmauchle@gmail.com" P. Steinger 

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install git -y

USER rails_app
WORKDIR /home/rails_app
RUN git clone https://github.com/bhzunami/umweltpaedagogik.git app

# Do the RAILS STUFF
WORKDIR /home/rails_app/app
#ENV RVM_PATH /home/rails_app/.rvm/gems/ruby-2.0.0-p481@global/bin/
ENV PATH /home/rails_app/.rvm/gems/ruby-2.0.0-p481/bin:/home/rails_app/.rvm/gems/ruby-2.0.0-p481@global/bin:/home/rails_app/.rvm/rubies/ruby-2.0.0-p481/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/home/rails_app/.rvm/bin:/home/rails_app/.rvm/bin
ENV RAILS_ENV=production

RUN bundle install --deployment --without development test
RUN bundle exec rake db:migrate 
RUN bundle exec rake assets:precompile
RUN touch tmp/restart.txt

USER root
ENV PATH /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
WORKDIR /root
CMD ["/sbin/my_init", "--", "bash", "-l"]
