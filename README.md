
# shksprwrdl

<!-- badges: start -->
<!-- badges: end -->

`shksprwrdl` is a `wordle`  clone that uses words from Shakespeare's 37 plays.
Text originally available in `xml` format from:
<http://www.ibiblio.org/xml/examples/shakespeare/>

## Installation

You can install the development version of `shksprwrdl` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rubenhm/shksprwrdl")
```

## Usage


``` r
# load package
library(shksprwrdl)

# launch shiny app
launch_app()
```


## Download and run from GitHub

```r
shiny::runUrl("https://github.com/rubenhm/shksprwrdl/archive/master.tar.gz",
  subdir = "inst/shiny/")
```
