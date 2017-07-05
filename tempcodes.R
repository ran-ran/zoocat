
devtools::load_all()
devtools::document()
devtools::build()
devtools::install(dependencies = FALSE)


library(testthat)
getwd()
setwd('..')
test_examples('./man/')
test_dir('./tests/testthat')
