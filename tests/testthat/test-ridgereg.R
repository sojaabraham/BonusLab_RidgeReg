test_that("Ridgereg returns correct structure and class", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 1)

  expect_s3_class(model, "ridgereg")
  expect_true(is.numeric(model$coefficients))
  expect_true(is.numeric(model$fitted))
  expect_true(is.numeric(model$residuals))
})

test_that("Ridgereg coefficients are similar to MASS::lm.ridge", {
  library(MASS)

  data(iris)
  formula <- Sepal.Length ~ Sepal.Width + Petal.Length
  lambda <- 1

  model <- ridgereg(formula, data = iris, lambda = lambda)
  model_mass <- lm.ridge(formula, data = iris, lambda = lambda)

  beta_mass <- coef(model_mass)

  expect_equal(
    as.vector(model$coefficients),
    as.vector(beta_mass),
    tolerance = 1e-3
  )
})

test_that("print.ridgereg executes without error", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 1)

  expect_output(print(model), "Coefficients")
  expect_output(print(model), "Sepal.Length")
})

test_that("Coef method returns named numeric vector", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 1)

  cf <- coef(model)
  expect_true(is.numeric(cf))
  expect_equal(names(cf), names(model$coefficients))
  expect_equal(cf, model$coefficients)
})

test_that("Predict returns fitted values when newdata is NULL", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 1)

  pred <- predict(model)
  expect_equal(pred, model$fitted)
})

test_that("Invalid lambda throws an error", {
  data(iris)
  expect_error(ridgereg(Sepal.Length ~ Sepal.Width, data = iris, lambda = -1))
})

test_that("Data validation", {
  data(iris)

  expect_error(ridgereg(Sepal.Length ~ Petal.Length, data = as.matrix(iris), lambda = 1),
               "Input data must be a data frame")
})


