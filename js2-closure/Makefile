test:
	emacs -Q -batch -l ert -L . -f package-initialize \
		$(addprefix -l ,$(wildcard *-test.el)) \
		-f ert-run-tests-batch-and-exit
