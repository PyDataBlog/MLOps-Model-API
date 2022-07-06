FROM yehohanan7/clisp
USER root

RUN apt-get -qq update
RUN apt-get install -y inotify-tools

WORKDIR /lisp-koans

CMD ["sbcl", "--script", "contemplate.lsp"]
