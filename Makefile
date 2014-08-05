EMACS = emacs
FILES = ycmacs.el
LOADFILE = ycmacs-load.el
THIRDPARTY = -L third-party/request

emacs:
	$(EMACS) -Q -L . $(THIRDPARTY) -l $(LOADFILE)
