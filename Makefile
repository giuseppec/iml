
all: build test

test: 
	Rscript --vanilla -e 'devtools::test()'
	
	
build: 
	Rscript --vanilla -e 'devtools::build()'