EMACS = emacs
FILES = ycmacs.el
EVALS = --eval "(require 'ycmacs)" \
	--eval "(ycm/hello)" \
	--eval "(switch-to-buffer \"*ycmacs*\")"

emacs:
	$(EMACS) -Q -L . $(EVALS)
