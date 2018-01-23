
all: document install test

test: 
	Rscript --vanilla -e 'devtools::test()'

document: man-roxygen/*
	Rscript --vanilla -e 'devtools::document()'
	
install: 
	Rscript --vanilla -e 'devtools::install()'