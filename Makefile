TAG := $(shell git describe --tags --always)

.PHONY: colonizza
colonizza: dotfiles.cfg
	./colonizza.py

.PHONY: release
release:
	git archive --format=tar.gz --prefix=Preferences/ --output=Preferences_$(TAG).tar.gz HEAD
