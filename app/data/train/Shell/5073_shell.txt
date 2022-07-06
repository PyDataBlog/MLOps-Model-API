echo -n "Customomizacoes ..."

mkdir -p ${DESTDIR}/TMPROOT/root/.ssh
mkdir -p ${DESTDIR}/TMPROOT/proc
mkdir -p ${DESTDIR}/TMPROOT/spool

cp /tmp/tmpnodefiles/passwd ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/shadow ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/interfaces ${DESTDIR}/TMPROOT/etc/network/
cp /tmp/tmpnodefiles/hosts ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/os-release ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/localtime ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/rc.local ${DESTDIR}/TMPROOT/etc/rc.local
cp /tmp/tmpnodefiles/inittab ${DESTDIR}/TMPROOT/etc/inittab
cp /tmp/tmpnodefiles/fstab_node ${DESTDIR}/TMPROOT/etc/fstab
cp /tmp/tmpnodefiles/ssh_root.pub ${DESTDIR}/TMPROOT/root/.ssh/authorized_keys
cp /tmp/tmpnodefiles/ssh_init ${DESTDIR}/TMPROOT/etc/init.d/ssh
cp /tmp/tmpnodefiles/motd ${DESTDIR}/TMPROOT/etc/motd
cp /tmp/tmpnodefiles/motd ${DESTDIR}/TMPROOT/etc/motd.tail
cp /tmp/tmpnodefiles/ntpdate ${DESTDIR}/TMPROOT/etc/default/ntpdate
cp /tmp/tmpnodefiles/crontab ${DESTDIR}/TMPROOT/etc/default/crontab
cp /tmp/tmpnodefiles/gmond.conf ${DESTDIR}/TMPROOT/etc/ganglia/
cp /tmp/tmpnodefiles/torque.conf ${DESTDIR}/TMPROOT/var/spool/torque/mom_priv/config
cp /tmp/tmpnodefiles/torque-mom ${DESTDIR}/TMPROOT/etc/init.d/pbs_mom
cp /tmp/tmpnodefiles/snd.conf ${DESTDIR}/TMPROOT/etc/modprobe.d/
cp /tmp/tmpnodefiles/nouveau.conf ${DESTDIR}/TMPROOT/etc/modprobe.d/

#Nvidia
cp /tmp/tmpnodefiles/nvidia_libs_inc.tar.gz ${DESTDIR}/TMPROOT/

#NFS
cp /tmp/tmpnodefiles/idmapd.conf ${DESTDIR}/TMPROOT/etc/

#LDAP
cp /tmp/tmpnodefiles/common-account ${DESTDIR}/TMPROOT/etc/pam.d/
cp /tmp/tmpnodefiles/common-auth ${DESTDIR}/TMPROOT/etc/pam.d/
cp /tmp/tmpnodefiles/common-password ${DESTDIR}/TMPROOT/etc/pam.d/
cp /tmp/tmpnodefiles/common-session ${DESTDIR}/TMPROOT/etc/pam.d/
cp /tmp/tmpnodefiles/common-session-noninteractive ${DESTDIR}/TMPROOT/etc/pam.d/
cp /tmp/tmpnodefiles/ldap.conf ${DESTDIR}/TMPROOT/etc/ldap/
cp /tmp/tmpnodefiles/nsswitch.conf ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/pam_ldap.conf ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/libnss-ldap.conf ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/nsswitch.conf ${DESTDIR}/TMPROOT/etc/
cp /tmp/tmpnodefiles/nvidia*.ko ${DESTDIR}/TMPROOT/lib/modules/3.2.0-4-amd64/kernel/drivers/video/

rm -rf ${DESTDIR}/TMPROOT/boot
rm -rf ${DESTDIR}/TMPROOT/initrd.img
rm -rf ${DESTDIR}/TMPROOT/vmlinuz
rm -rf ${DESTDIR}/TMPROOT/var/spool/torque/server_name
rm -rf ${DESTDIR}/TMPROOT/var/spool/torque/spool

/usr/sbin/chroot ${DESTDIR}/TMPROOT/ depmod -a
/usr/sbin/chroot ${DESTDIR}/TMPROOT/ /usr/sbin/update-rc.d pbs_mom defaults
/usr/sbin/chroot ${DESTDIR}/TMPROOT/ /usr/sbin/update-rc.d ganglia-monitor defaults
/usr/sbin/chroot ${DESTDIR}/TMPROOT/ /usr/sbin/update-rc.d ssh defaults
/usr/sbin/chroot ${DESTDIR}/TMPROOT/ ln -sf /spool/cluster_ada_spool /var/spool/torque/spool && chmod o+t /var/spool/torque/spool
/usr/sbin/chroot ${DESTDIR}/TMPROOT/ tar zxfv nvidia_libs_inc.tar.gz
/usr/sbin/chroot ${DESTDIR}/TMPROOT/ rm nvidia_libs_inc.tar.gz

echo " feito !"
