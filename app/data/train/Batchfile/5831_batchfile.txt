cmd_e2fsprogs/tune2fs.o := arm-linux-musleabihf-gcc -Wp,-MD,e2fsprogs/.tune2fs.o.d   -std=gnu99 -Iinclude -Ilibbb  -include include/autoconf.h -D_GNU_SOURCE -DNDEBUG -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -D"BB_VER=KBUILD_STR(1.22.1)" -DBB_BT=AUTOCONF_TIMESTAMP  -Wall -Wshadow -Wwrite-strings -Wundef -Wstrict-prototypes  -Wunused-function -Wunused-value -Wmissing-prototypes -Wmissing-declarations -Wno-format-security -Wdeclaration-after-statement -Wold-style-definition -fno-builtin-strlen -finline-limit=0 -fomit-frame-pointer -ffunction-sections -fdata-sections -fno-guess-branch-probability -funsigned-char -static-libgcc -falign-functions=1 -falign-jumps=1 -falign-labels=1 -falign-loops=1 -fno-unwind-tables -fno-asynchronous-unwind-tables -g -O0     -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(tune2fs)"  -D"KBUILD_MODNAME=KBUILD_STR(tune2fs)" -c -o e2fsprogs/tune2fs.o e2fsprogs/tune2fs.c

deps_e2fsprogs/tune2fs.o := \
  e2fsprogs/tune2fs.c \
    $(wildcard include/config/feature/clean/up.h) \
  include/libbb.h \
    $(wildcard include/config/feature/shadowpasswds.h) \
    $(wildcard include/config/use/bb/shadow.h) \
    $(wildcard include/config/selinux.h) \
    $(wildcard include/config/feature/utmp.h) \
    $(wildcard include/config/locale/support.h) \
    $(wildcard include/config/use/bb/pwd/grp.h) \
    $(wildcard include/config/lfs.h) \
    $(wildcard include/config/feature/buffers/go/on/stack.h) \
    $(wildcard include/config/feature/buffers/go/in/bss.h) \
    $(wildcard include/config/feature/ipv6.h) \
    $(wildcard include/config/feature/seamless/xz.h) \
    $(wildcard include/config/feature/seamless/lzma.h) \
    $(wildcard include/config/feature/seamless/bz2.h) \
    $(wildcard include/config/feature/seamless/gz.h) \
    $(wildcard include/config/feature/seamless/z.h) \
    $(wildcard include/config/feature/check/names.h) \
    $(wildcard include/config/feature/prefer/applets.h) \
    $(wildcard include/config/long/opts.h) \
    $(wildcard include/config/feature/getopt/long.h) \
    $(wildcard include/config/feature/pidfile.h) \
    $(wildcard include/config/feature/syslog.h) \
    $(wildcard include/config/feature/individual.h) \
    $(wildcard include/config/echo.h) \
    $(wildcard include/config/printf.h) \
    $(wildcard include/config/test.h) \
    $(wildcard include/config/kill.h) \
    $(wildcard include/config/chown.h) \
    $(wildcard include/config/ls.h) \
    $(wildcard include/config/xxx.h) \
    $(wildcard include/config/route.h) \
    $(wildcard include/config/feature/hwib.h) \
    $(wildcard include/config/desktop.h) \
    $(wildcard include/config/feature/crond/d.h) \
    $(wildcard include/config/use/bb/crypt.h) \
    $(wildcard include/config/feature/adduser/to/group.h) \
    $(wildcard include/config/feature/del/user/from/group.h) \
    $(wildcard include/config/ioctl/hex2str/error.h) \
    $(wildcard include/config/feature/editing.h) \
    $(wildcard include/config/feature/editing/history.h) \
    $(wildcard include/config/feature/editing/savehistory.h) \
    $(wildcard include/config/feature/tab/completion.h) \
    $(wildcard include/config/feature/username/completion.h) \
    $(wildcard include/config/feature/editing/vi.h) \
    $(wildcard include/config/feature/editing/save/on/exit.h) \
    $(wildcard include/config/pmap.h) \
    $(wildcard include/config/feature/show/threads.h) \
    $(wildcard include/config/feature/ps/additional/columns.h) \
    $(wildcard include/config/feature/topmem.h) \
    $(wildcard include/config/feature/top/smp/process.h) \
    $(wildcard include/config/killall.h) \
    $(wildcard include/config/pgrep.h) \
    $(wildcard include/config/pkill.h) \
    $(wildcard include/config/pidof.h) \
    $(wildcard include/config/sestatus.h) \
    $(wildcard include/config/unicode/support.h) \
    $(wildcard include/config/feature/mtab/support.h) \
    $(wildcard include/config/feature/devfs.h) \
  include/platform.h \
    $(wildcard include/config/werror.h) \
    $(wildcard include/config/big/endian.h) \
    $(wildcard include/config/little/endian.h) \
    $(wildcard include/config/nommu.h) \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/limits.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/features.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/limits.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/byteswap.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/stdint.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/alltypes.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/stdint.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/endian.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/stdbool.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/unistd.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/posix.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/ctype.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/dirent.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/errno.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/errno.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/fcntl.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/fcntl.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/inttypes.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/netdb.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/netinet/in.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/socket.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/socket.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/setjmp.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/setjmp.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/signal.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/signal.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/stdio.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/stdlib.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/alloca.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/stdarg.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/stddef.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/string.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/strings.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/libgen.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/poll.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/ioctl.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/ioctl.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/mman.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/mman.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/stat.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/stat.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/time.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/select.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/types.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/sysmacros.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/wait.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/resource.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/resource.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/termios.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/termios.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/time.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/param.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/pwd.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/grp.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/mntent.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/statfs.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/sys/statvfs.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/bits/statfs.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/utmp.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/utmpx.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/locale.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/arpa/inet.h \
  include/pwd_.h \
  include/grp_.h \
  include/shadow_.h \
  include/xatonum.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/linux/fs.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/linux/limits.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/linux/ioctl.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm/ioctl.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm-generic/ioctl.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/linux/types.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm/types.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm-generic/int-ll64.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm/bitsperlong.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm-generic/bitsperlong.h \
    $(wildcard include/config/64bit.h) \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/linux/posix_types.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/linux/stddef.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm/posix_types.h \
  /Volumes/ELFSOSX/tools/lib/gcc/arm-linux-musleabihf/4.7.3/../../../../arm-linux-musleabihf/include/asm-generic/posix_types.h \
  include/bb_e2fs_defs.h \

e2fsprogs/tune2fs.o: $(deps_e2fsprogs/tune2fs.o)

$(deps_e2fsprogs/tune2fs.o):
