all: doc vignette data
.PHONY: doc test vignette data

# R_OPTS: --vanilla without --no-environ
R_OPTS=--no-save --no-restore --no-init-file --no-site-file

# build package documentation
doc:
	R -e 'devtools::document()'

# Run tests
test:
	R -e 'devtools::test()'

data: data/lineup2ex.RData

data/lineup2ex.RData: inst/scripts/create_lineup2ex.R
	cd $(<D);R CMD BATCH $(R_OPTS) $(<F)

vignette: docs/lineup2.html

docs/lineup2.html: vignettes/lineup2.Rmd data/lineup2ex.RData
	[ -d docs ] || mkdir docs
	R $(R_OPTS) -e "rmarkdown::render('$<')"
	mv vignettes/lineup2.html $@
