FROM odk211/iruby-notebook

COPY Gemfile .
RUN bundle install