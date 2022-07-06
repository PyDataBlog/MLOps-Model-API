FROM docku/build
MAINTAINER Jon Chen <bsd@voltaire.sh>

ENV BINTRAY_USER "jchen"
ENV BINTRAY_TOKEN ""
ENV BINTRAY_REPO "docku"

RUN pacman -Syu --needed --noconfirm go
ADD build_makefile /root/Makefile
CMD ["make", "-f", "/root/Makefile", "all"]
