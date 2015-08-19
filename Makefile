.PHONY: release colonizza

TAG := `git symbolic-ref HEAD 2> /dev/null | cut -b 12-`-`git log --pretty=format:%h -1`

colonizza: dotfiles.cfg
	./colonizza.py

emacs-settings.html: emacs-settings.org
	emacs emacs-settings.org --batch -f org-html-export-to-html --kill

release:
	git archive --format=tar.gz --prefix=Preferences/ --output=Preferences_$(TAG).tar.gz HEAD
