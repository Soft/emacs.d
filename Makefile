EMACS:=emacs
LISP=$(shell find -type f -iname "*.el")
COMPILED=$(LISP:%.el=%.elc)

all: $(COMPILED)

%.elc: %.el
	$(EMACS) --batch -f batch-byte-compile $<

.PHONY: clean

clean:
	rm -f $(COMPILED)


