EMACS = emacs
FILES = ycmacs.el
EVALS = --eval "(switch-to-buffer \"*Messages*\")" \
	--eval "(require 'ycmacs)" \
	--eval "(ycm/hello)"

emacs:
	$(EMACS) -Q -L . $(EVALS)
