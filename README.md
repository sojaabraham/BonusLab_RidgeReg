
**Ridge Regression using QR Decomposition in R**

<!-- badges: start -->
[![R-CMD-check](https://github.com/sojaabraham/BonusLab_RidgeReg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sojaabraham/BonusLab_RidgeReg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Features

- Fit ridge regression models using QR decomposition.
- Automatically standardizes predictors (except intercept).
- Provides S3 methods:
  - `print()` – neatly display coefficients.
  - `predict()` – predict fitted values or on new data.
  - `coef()` – extract regression coefficients.
- Specify ridge penalty parameter `lambda`.

---

## Installation

You can install the development version of LinearRegression from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sojaabraham/BonusLab_RidgeReg")
```

## Example
This is a basic example which shows you how this package work:

``` r
library(LinearRegression)

data(iris)
model <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
print(model)
coef(model)
predict(model)

```

