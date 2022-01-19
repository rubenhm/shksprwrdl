library(usethis)
library(roxygen2)
library(devtools)


usethis::use_data_raw('plays_text')
usethis::use_data_raw('plays_tokens')

usethis::use_r('master_mind.R')

usethis::use_readme_md()
usethis::use_description()

usethis::use_roxygen_md()
usethis::use_pipe(export = TRUE)
usethis::use_package('dplyr')
usethis::use_package('tibble')
usethis::use_package('shiny')
usethis::use_package('shinyjs')
usethis::use_package('DT')

devtools::document()
