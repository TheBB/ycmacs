EMACS = emacs
FILES = ycmacs.el
LOADFILE = ycmacs-load.el
THIRDPARTY = -L third-party/request -L third-party/popup

emacs:
	$(EMACS) -Q -L . $(THIRDPARTY) -l $(LOADFILE)
