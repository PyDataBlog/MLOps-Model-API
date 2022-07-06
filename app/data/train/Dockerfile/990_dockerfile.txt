FROM scratch

COPY helm-test /
ENTRYPOINT ["/helm-test"]
