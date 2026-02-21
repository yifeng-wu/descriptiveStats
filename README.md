
<!-- README.md is generated from README.Rmd. Please edit that file -->

# descriptiveStats

<!-- badges: start -->

<!-- badges: end -->

`DescriptiveStats` is a lightweight R package providing basic
descriptive statistics functions with robust input validation and
informative messaging. It is designed to handle common edge cases such
as missing values, empty vectors, and non-numeric inputs gracefully.

## Installation

You can install the development version directly from GitHub:

``` r
# install.packages("devtools") # if not already installed
# devtools::install_github("yifeng-wu/DescriptiveStats")
```

## Usage

``` r
library(descriptiveStats)
x <- c(1, 2, 3, 4, 5)
```

**Arithmetic Mean**

``` r
calc_mean(x)
#> [1] 3
```

**Median**

``` r
calc_median(x)
#> [1] 3
```

**Statistical Mode (Most Frequent Value)**

``` r
# Single mode
calc_mode(c(1, 2, 2, 3))
#> [1] 2

# Multiple modes (tie)
calc_mode(c(1, 1, 2, 2))
#> [1] 1 2

# All values unique (all returned)
calc_mode(c(1, 2, 3))
#> [1] 1 2 3
```

**First Quartile (Q1) and Third Quartile (Q3)**

``` r
calc_q1(x)
#> [1] 2
calc_q3(x)
#> [1] 4
```

**Interquartile Range (IQR)**

``` r
calc_iqr(x)
#> [1] 2
```

**Handling of NA (missing) values**

``` r
calc_mean(c(x, NA))
#> `x` contains 1 missing value.
#> ! 1 missing value will be removed before calculation.
#> [1] 3
```
