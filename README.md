
# Linear Regression Model

<!-- badges: start -->
[![R-CMD-check](https://github.com/sojaabraham/Lab_4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sojaabraham/Lab_4/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of this package is to implementation of linear regression model 
using QR decomposition.
We will also implement an object oriented system to handle special functions 
such as print(), plot(), resid(), pred(), coef() and summary().

## Installation

You can install the development version of LinearRegression from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sojaabraham/Lab_4")
```

## Example
This is a basic example which shows you how this package work:

``` r
library(LinearRegression)

data(iris)
model <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
print(model)
summary(model)
plot(model)
coef(model)
resid(model)
pred(model)

```

