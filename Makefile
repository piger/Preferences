.PHONY: release colonizza

TAG := `git symbolic-ref HEAD 2> /dev/null | cut -b 12-`-`git log --pretty=format:%h -1`

colonizza: dotfiles.cfg
	./colonizza.py

release:
	git archive --format=tar.gz --prefix=Preferences/ --output=Preferences_$(TAG).tar.gz HEAD
