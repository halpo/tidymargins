
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidymargins

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/halpo/tidymargins.svg?branch=master)](https://travis-ci.org/halpo/tidymargins)
[![Test
coverage](https://codecov.io/gh/halpo/tidymargins/branch/master/graph/badge.svg)](https://codecov.io/gh/halpo/tidymargins?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidymargins)](https://CRAN.R-project.org/package=tidymargins)
<!-- badges: end -->

The goal of `tidymargins` is to provide the `with_margins()` adverb
function, as well as the related `spread_each()` function.

## Installation

You can install the released version of tidymargins from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidymargins")
```

## Basic Usage

`with_margins` is very simple to use. It is an adverb, i.e. it accepts
and returns a function. The function, passed in, we’ll call it
`original`, should accept a [table like](https://tibble.tidyverse.org/)
data object as it’s first argument and return a similar object. The
resulting function, we’ll call it `modified`, will also accept a data
argument as it’s first argument however that argument is expected to be
a [grouped data
frame](https://dplyr.tidyverse.org/reference/group_by.html). Other
arguments to `modified` are passed along to the `original` function.
When `modified` is called with a grouped data frame `original` is called
for every possible subset of grouping variables, including the full set
of all original grouping variables and the empty set implying no
grouping, The results are bound together into a single data frame and
returned.  
The example below shows this as an example.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidymargins)
data(mtcars)

# our original function here is count
modified <- with_margins(count)

# modified is a function 
class(modified)
#> [1] "function"

modified(group_by(mtcars, Cylinders = cyl, Gears = gear))
```

| Cylinders | Gears |  n |
| :-------- | :---- | -: |
| 4         | 3     |  1 |
| 4         | 4     |  8 |
| 4         | 5     |  2 |
| 6         | 3     |  2 |
| 6         | 4     |  4 |
| 6         | 5     |  1 |
| 8         | 3     | 12 |
| 8         | 5     |  2 |
| 4         | (All) | 11 |
| 6         | (All) |  7 |
| 8         | (All) | 14 |
| (All)     | 3     | 15 |
| (All)     | 4     | 12 |
| (All)     | 5     |  5 |
| (All)     | (All) | 32 |

See that the output of modified is the counts grouped by; both `cyl` and
`gear`, by `cyl` alone, by `gear` alone, and a grand total (no groups),
all in a single data frame. When a variable is not used as a grouping
variable it is replaced with the value `"(All)`", indicating that all
levels of that variable are included. This label can be changes by
specifying the `all.name` argument when calling `with_margins`, shown in
the next example.

There is no need to store the intermediate function returned by
`with_margins`, but since the returned function may also accept other
arguments care should be taken to what is passing to which function.
Care should also be taken with the pipe.

``` r
mtcars %>% 
    select(cyl, gear, mpg, disp, hp, qsec) %>% 
    group_by(cyl, gear) %>% 
    # <---------- with_margins ----------------><-- summarise --->
    with_margins(summarise_if, all.name='Total')(is.numeric, mean) %>% 
    # making pretty
    rename( "Miles/(US) gallon" = "mpg"
          , "Cylinders" = "cyl"
          , "Displacement (cu.in.)" = "disp"
          , "Gross horsepower" = "hp"
          , "1/4 mile time" = "qsec"
          , "Number of forward gears" = "gear"
          )
```

| Cylinders | Number of forward gears | Miles/(US) gallon | Displacement (cu.in.) | Gross horsepower | 1/4 mile time |
| :-------- | :---------------------- | ----------------: | --------------------: | ---------------: | ------------: |
| 4         | 3                       |          21.50000 |              120.1000 |         97.00000 |      20.01000 |
| 4         | 4                       |          26.92500 |              102.6250 |         76.00000 |      19.61250 |
| 4         | 5                       |          28.20000 |              107.7000 |        102.00000 |      16.80000 |
| 6         | 3                       |          19.75000 |              241.5000 |        107.50000 |      19.83000 |
| 6         | 4                       |          19.75000 |              163.8000 |        116.50000 |      17.67000 |
| 6         | 5                       |          19.70000 |              145.0000 |        175.00000 |      15.50000 |
| 8         | 3                       |          15.05000 |              357.6167 |        194.16667 |      17.14250 |
| 8         | 5                       |          15.40000 |              326.0000 |        299.50000 |      14.55000 |
| 4         | Total                   |          26.66364 |              105.1364 |         82.63636 |      19.13727 |
| 6         | Total                   |          19.74286 |              183.3143 |        122.28571 |      17.97714 |
| 8         | Total                   |          15.10000 |              353.1000 |        209.21429 |      16.77214 |
| Total     | 3                       |          16.10667 |              326.3000 |        176.13333 |      17.69200 |
| Total     | 4                       |          24.53333 |              123.0167 |         89.50000 |      18.96500 |
| Total     | 5                       |          21.38000 |              202.4800 |        195.60000 |      15.64000 |
| Total     | Total                   |          20.09062 |              230.7219 |        146.68750 |      17.84875 |
