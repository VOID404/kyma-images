LISP ?= sbcl

all: test

run:
	rlwrap $(LISP) --load run.lisp

dependencies:
	$(LISP)	--non-interactive \
		--load kyma-images.asd \
		--eval '(ql:quickload :kyma-images)'

build:
	$(LISP)	--non-interactive \
		--load kyma-images.asd \
		--eval '(ql:quickload :kyma-images)' \
		--eval '(asdf:make :kyma-images)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
