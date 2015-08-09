quick:
	make doc;
	rm twitterreport_*&
	R CMD build --no-resave-data --no-build-vignettes --no-manual .&
	make inst
inst:
	R CMD REMOVE twitterreport ;
	R CMD INSTALL twitterreport_*
build:
	make doc;
	rm twitterreport_* &
	R CMD build --no-resave-data .
	make inst
check:
	make build;
	R CMD check --as-cran twitterreport_*
doc:
	rm *.o twitterreport.so &
	Rscript -e 'roxygen2::roxygenise()' ;
	Rscript -e 'Rcpp::compileAttributes(verbose=TRUE)'
