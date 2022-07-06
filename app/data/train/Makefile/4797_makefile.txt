# Rakemaker
# Makefile which attempts to forward everything to Rake.

# Need at least 1 target, so match all (will map to 'default' in rake)
all: .___message
	@rake

# Match everything
% : .___message
	@rake $@

# Make sure .___message always runs
.PHONY: .___message

# Print message suggesting direct use of Rake.
.___message:
	@echo "Attempting to forward to 'rake'."
	@echo "Please it directly in the future,"
	@echo "as THIS MAY NOT WORK AT ALL."
	@echo "#################################"
