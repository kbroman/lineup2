.PHONY: doc test

# R_OPTS: --vanilla without --no-environ
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

# build package documentation
doc:
	R -e 'devtools::document()'

# Run tests
test:
	R -e 'devtools::test()'
