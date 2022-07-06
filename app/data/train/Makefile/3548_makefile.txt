
default:	build

clean:
	rm -rf Makefile objs

build:
	$(MAKE) -f objs/Makefile
	$(MAKE) -f objs/Makefile manpage

install:
	$(MAKE) -f objs/Makefile install

upgrade:
	/data0/nginx160//sbin/nginx -t

	kill -USR2 `cat /data0/nginx160//logs/nginx.pid`
	sleep 1
	test -f /data0/nginx160//logs/nginx.pid.oldbin

	kill -QUIT `cat /data0/nginx160//logs/nginx.pid.oldbin`
