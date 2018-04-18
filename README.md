# farsr

------

[![Travis-CI Build Status](https://travis-ci.org/slava-kohut/R-Building-R-packages.svg?branch=master)](https://travis-ci.org/slava-kohut/R-Building-R-packages)

Farsr is a set of tools for working with the Fatality Analysis Reporting System data.

## Installation

------

The package can be installed as follows:
``` r
# Github
devtools::install_github(repo = 'slava-kohut/R-Building-R-packages')  
```

## Example

------

Package workflow involves several basic steps such as 1) loading data, 2) analyzing data, and 3) plotting data.
Below is an example of a simple task that can be accomplished using farsr. 

``` r
# read data
setwd('./raw-data/')
fars_data <- fars_read(make_filename(2012))

# summary of accidents for 2013
fars_summarize_years(years = 2013)
```
