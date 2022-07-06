FROM fedora:20
MAINTAINER "Andy Thompson" <andy@webtatic.com>
ENV container docker

# Update all base packages to keep them fresh
RUN yum -y update; yum clean all

# Turn off most systemd services
RUN (cd /lib/systemd/system/sysinit.target.wants/; for i in *; do [ $i == systemd-tmpfiles-setup.service ] || rm -f $i; done); \
rm -f /etc/systemd/system/*.wants/*;\
(cd /lib/systemd/system/multi-user.target.wants/; for i in *; do [ $i == systemd-user-sessions.service ] || rm -f $i; done); \
rm -f /lib/systemd/system/local-fs.target.wants/*; \
rm -f /lib/systemd/system/sockets.target.wants/*udev*; \
rm -f /lib/systemd/system/sockets.target.wants/*initctl*; \
rm -f /lib/systemd/system/basic.target.wants/*;\
rm -f /lib/systemd/system/anaconda.target.wants/*;

# Mount cgroups by default
VOLUME [ "/sys/fs/cgroup" ]

# Set the init command to run at boot by default
CMD ["/usr/sbin/init"]
