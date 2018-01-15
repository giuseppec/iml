
all: build

test: 
	Rscript --vanilla -e 'devtools::test()'
	
	
build: test
	Rscript --vanilla -e 'devtools::build()'