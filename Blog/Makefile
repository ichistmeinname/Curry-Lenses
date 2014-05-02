# Generic Makefile for Spicey applications

# Definition of Curry installation bin directory to be used:
CURRYBIN=/Users/ichistmeinname/Documents/programming/kics2/bin

.PHONY: all
all:
	@echo "make: putDeploy combDeploy putLoad combLoad putCompile CombCompile"

# Deploy the generated Spicey application, i.e., install it in the
# web pages:

.PHONY: putCompile
putCompile:
	scripts/putCompile.sh

.PHONY:	putLoad
putLoad:
	scripts/putLoad.sh


.PHONY:	putDeploy
putDeploy:
	scripts/putDeploy.sh


.PHONY:	combCompile
combCompile:
	scripts/combCompile.sh

.PHONY: combLoad
combLoad:
	scripts/combLoad.sh

.PHONY: combDeploy
combDeploy:
	scripts/combDeploy.sh

# clean up generated programs
.PHONY: clean
clean: 
	$(CURRYBIN)/cleancurry -r
