all:
	$(MAKE) -C .. opalcoin_test
clean:
	$(MAKE) -C .. opalcoin_test_clean
check:
	$(MAKE) -C .. opalcoin_test_check
