quick:
	make doc;
	rm twitterreport_*&
	R CMD build --no-resave-data --no-build-vignettes --no-manual .;
	R CMD REMOVE twitterreport &
	R CMD INSTALL twitterreport_*
build:
	make doc;
	rm twitterreport_* &
	R CMD REMOVE twitterreport_* &
	R CMD build --no-resave-data .
check:
	make build;
	R CMD check --as-cran twitterreport_*
doc:
	Rscript -e 'roxygen2::roxygenise()'
