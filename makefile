build:
	make doc;
	R CMD build --resave-data .
check:
	make build;
	R CMD check --as-cran twitterreport_*
doc:
	Rscript -e 'roxygen2::roxygenise()'
