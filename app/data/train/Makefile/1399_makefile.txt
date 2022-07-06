DOCROOT = www
CHECKSUM = sha256
CHECKSUM_BIN = $(CHECKSUM)sum

INSTALL_SCRIPT = install.sh

S3CMD = /usr/bin/s3cmd
SYNC = $(S3CMD) sync

REPONAME = ypcs
BUCKET = ypcs-repository
BUCKET_PREFIX = repos/

TARGET = s3://$(BUCKET)/$(BUCKET_PREFIX)$(REPONAME)

all:

clean:
	find $(DOCROOT)/ -type f -name index.html -exec {} \; 2>/dev/null

$(INSTALL_SCRIPT).$(CHECKSUM): $(INSTALL_SCRIPT)
	$(CHECKSUM_BIN) $(INSTALL_SCRIPT) |tee $(INSTALL_SCRIPT).$(CHECKSUM)

$(INSTALL_SCRIPT).$(CHECKSUM).asc: $(INSTALL_SCRIPT).$(CHECKSUM)
	gpg -a --sign $(INSTALL_SCRIPT).$(CHECKSUM

$(DOCROOT)/index.html:
	python dirlist.py $(DOCROOT)

sync-misc: $(INSTALL_SCRIPT) $(INSTALL_SCRIPT).$(CHECKSUM) $(INSTALL_SCRIPT).$(CHECKSUM).asc
	$(SYNC) $(INSTALL_SCRIPT) $(TARGET)/
	$(SYNC) $(INSTALL_SCRIPT).$(CHECKSUM) $(TARGET)/
	$(SYNC) $(INSTALL_SCRIPT).$(CHECKSUM).asc $(TARGET)/
	
	$(SYNC) logs $(TARGET)/
	$(SYNC) conf $(TARGET)/
	cd www/debian && $(SYNC) index.html $(TARGET)/ && cd ../..

sync-db:
	$(SYNC) db $(TARGET)/

sync-pool:
	cd www/debian && $(SYNC) pool $(TARGET)/

sync-dists:
	cd www/debian && $(SYNC) dists $(TARGET)/
	$(S3CMD) --recursive modify --add-header="Cache-Control: max-age=300" $(TARGET)/dists/

sync: clean $(DOCROOT)/index.html sync-misc sync-db sync-pool sync-dists
