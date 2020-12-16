
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Diagnose Bioconductor package build failures

<!-- badges: start -->

<!-- badges: end -->

The purpose of this repository is to collect functionality for
diagnosing build failures in Bioconductor packages. Contributions from
the community are welcome - please fork this repository and make a pull
request with your contributions. Also add your name among the
contributors below.

For more information, refer to the corresponding entry in the
[BiocChallenges](https://github.com/kevinrue/BiocChallenges/)
repository.

## Contributors

  - Charlotte Soneson (@csoneson)
  - Lluís Revilla Sancho (@llrs)

## Installation

You can install the released version of BiocBuildFailureDiagnosis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("csoneson/BiocBuildFailureDiagnosis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(BiocBuildFailureDiagnosis)
explainBiocBuildFailure("Rhisat2")
```
