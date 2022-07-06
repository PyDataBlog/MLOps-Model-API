do-tar:
	tar -czf SOURCES/test.tgz src/* 
clean-tar:
	rm -rf SOURCES/test.tgz
clean:
	rm -rf SOURCES/*
	rm -rf RPMS/*
	rm -rf SRPMS/*
	rm -rf BUILD/*
	rm -rf BUILDROOT/*
build:
	rpmbuild -vv -ba SPECS/rpm.spec
all:

