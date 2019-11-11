
all: document install test vignette

test: 
	Rscript --vanilla -e 'suppressMessages(devtools::test())'

document: man-roxygen/*
	Rscript --vanilla -e 'devtools::document()'
	
install: 
	Rscript --vanilla -e 'devtools::install()'

readme: 
	Rscript --vanilla -e 'rmarkdown::render("README.Rmd", output_format = "github_document")'

vignette:
	Rscript --vanilla -e 'devtools::build_vignettes()'

check:
	Rscript --vanilla -e 'devtools::check(cran = TRUE)'
