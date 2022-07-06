FROM buildpack-deps:sid-curl

RUN apt-get update && apt-get install -y ldap-utils && rm -rf /var/lib/apt/lists/*
ADD ldapsearch-test.sh /bin/
CMD /bin/ldapsearch-test.sh

