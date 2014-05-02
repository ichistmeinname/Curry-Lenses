# Generic Makefile for Spicey applications

# Definition of Curry installation bin directory to be used:
CURRYBIN=/Users/ichistmeinname/Documents/programming/kics2/bin

.PHONY: all
all:
	@echo "make: deploy compile load clean?"

# Deploy the generated Spicey application, i.e., install it in the
# web pages:
.PHONY: deploy
deploy:
	scripts/deploy.sh

# Compile the generated Spicey application:
.PHONY: compile
compile:
	scripts/compile.sh

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load:
	scripts/load.sh

# clean up generated programs
.PHONY: clean
clean: 
	$(CURRYBIN)/cleancurry -r
