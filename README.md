
# MFO

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Travis build
status](https://travis-ci.com/JorgeDelro/MFO.svg?branch=master)](https://app.travis-ci.com/github/JorgeDelro/MFO/jobs/543650025/config)
[![Codecov test
coverage](https://codecov.io/gh/JorgeDelro/MFO/branch/master/graph/badge.svg)](https://app.codecov.io/gh/JorgeDelro/MFO?branch=master)
<!-- badges: end -->

## Overview

The **MFO** package have been designed to calculate the Maximal Fat
Oxidation (MFO), the exercise intensity that elicits MFO (Fatmax) and
the SIN model to represent the fat oxidation kinetics. Three variables
can be obtained from the SIN model: dilatation (d), symmetry (s) and
traslation (t).Additionally, the package allows to calculate MFO and
Fatmax of multiple subjects.

## Resources

  - [Application of MFO and
    Fatmax](https://www.tandfonline.com/doi/abs/10.1080/17461391.2020.1788650?journalCode=tejs20)
    (European Journal of Sport Science)
  - [MFO kinetics
    basis](https://journals.lww.com/acsm-msse/Fulltext/2009/08000/A_Mathematical_Model_to_Describe_Fat_Oxidation.11.aspx)
    (Medicine & Science in Sport & Exercise)

## Example

This is a basic example which shows you how to use the MFO package:

``` r
library(devtools)
install_github("JorgeDelro/MFO")
library(MFO)
```

First, we have to load the data which consists in 3 databases: -
basal\_df: basal metabolism database. - MFO\_df: MFO test database. -
VO2max\_df: results of a graded exercise test of which the VO2max of the
subject is going to be extracted.

``` r

# Read dfs
data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")
# Convert to data.frame
basal_df <- data.frame(basal_df)
MFO_df <- data.frame(MFO_df)
VO2max_df <- data.frame(VO2max_df)
```

Then, we can used the function MFO

``` r
result_MFO <- MFO(step_time = 20,
                  db_MFO = MFO_df,
                  db_basal = basal_df,
                  db_graded = VO2max_df,
                  cv_var = "RER",
                  author = "Frayn",
                  VO2max = NULL)
```

and the MFO can be plotted

``` r
print(result_MFO$MFO_plot)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

MFO kinetics are calculated using a database returned from MFO function
called MFO\_db.

``` r
result_MFO_kinetics <- MFO_kinetics(result_MFO$MFO_db)
```

And again the function returns a plot with the results calculated

``` r
print(result_MFO_kinetics$MFO_kinetics_plot)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
