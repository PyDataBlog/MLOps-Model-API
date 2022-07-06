Kernel Dev Notes
================

Misc
----
cat /proc/kmsg
make mandocs; make installmandocs

make coccicheck M=drivers/staging

* Trivial Patch Monkey
trivial@kernel.org

Quotes
------
* Fire up your editor and come join us; you will be more than welcome.

* "Printf should not be used for chit-chat"

* If I could give you one piece of advice: never sleep with anyone crazier than
yourself. But if I had to give you advice on locking: keep it simple.

* Deadlocks are problematic, but not as bad as data corruption. Code which grabs
  a read lock, searches a list, fails to find what it wants, drops the read
  lock, grabs a write lock and inserts the object has a race condition.

 If you don't see why, please stay the fuck away from my code.

* This conversation is unreal. Just remove the misguided assert and
  you're good. After that, please have someone explain basic reference
  counting to you.

	Thanks,
	Andreas

* 


Source code
-----------
# Greg KH's tree
`$ git clone -b staging-testing \
   git://git.kernel.org/pub/scm/linux/kernel/git/gregkh/staging.git`

# Torvalds' tree
git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git
   

Documentation 
-------------
See src/HOWTO for list of prerequisite reading.

int types
---------
Never use int8_t and friends. Use u8, u16, s32 etc

'int' is a return type, and for loops and other things that you know
will fit in that size.

For values that cross the user/kernel boundry, add '__' to the front of
the variable
	__u8
	__u16
	__u32
and so on.  NEVER use 'int' or 'long' crossing that boundry, it's not
going to work properly.

Patches
-------

* Who to send patch to
$ scripts/get_maintainer.pl mm/memory.c

* Patch from last commit (add extra ~ for more commits)
$ git format-patch HEAD~ 

_remember to check commit format, use `lfile <file>`_

* Patch set
$ git format-patch -s -X -o /home/tobin/scratch/outgoing --cover-letter <!-- set X -->


Development Tools
-----------------
_see Documentation/4.Coding_
run code with lockdep
Documentation/fault-injection/fault-injection.txt
sparse - add C=1 to the make command
_see https://sparse.wiki.kernel.org/index.php/Main_Page_
Documentation/coccinelle.txt


Comments
--------
Certain things should always be commented.  Uses of memory barriers should
be accompanied by a line explaining why the barrier is necessary.  The
locking rules for data structures generally need to be explained somewhere.
Major data structures need comprehensive documentation in general.
Non-obvious dependencies between separate bits of code should be pointed
out.  Anything which might tempt a code janitor to make an incorrect
"cleanup" needs a comment saying why it is done the way it is.  And so on.

TODO
----
learn how to cross compile http://www.kernel.org/pub/tools/crosstool/
read Documentation/kernel-doc-nano-HOWTO.txt
* git
  http://git-scm.com/
  http://www.kernel.org/pub/software/scm/git/docs/user-manual.html
  read git book again

review some patches
 phrase review: comments as questions rather than criticisms.  Asking "how does
 the lock get released in this path?" will always work better than stating "the
 locking here is wrong."

* start looking for bugs
 build kernel using torvalds-4.6-rc7.config, it has debug options turned on.

Reading List
------------
Linux Kernel Build
==================

* which tree
`$ cd $KERNEL_TREE`

* clean
`$ make mrproper`

* get runnin config file
`$ cp /boot/config-blah .config`

* update config
`$ make nconfig`

* build
`$ make -j8 EXTRA-CFLAGS=-W` <!-- get extra compiler warnings -->

* make modules and run Ubuntu install script
`# make modules_install install`

* build emacs TAGS file
`# make TAGS`

Qemu
====

https://www.collabora.com/news-and-blog/blog/2017/01/16/setting-up-qemu-kvm-for-kernel-development/

mount image using chroot to make changes (file system is read only
when booted with qemu)
https://help.ubuntu.com/community/Installation/FromLinux

root password: pass

