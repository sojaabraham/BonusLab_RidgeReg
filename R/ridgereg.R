#' Ridge Regression Model using QR Decomposition
#'
#' This function fits a ridge regression model using QR decomposition.
#' Returns an object of class \code{ridgereg} with coefficients, fitted values,
#' residuals, and other statistics.
#'
#' @param formula A formula describing the model (e.g., y ~ x1 + x2).
#' @param data A data frame containing the variables in the formula.
#' @param lambda A non-negative numeric value specifying the ridge penalty.
#'
#' @importFrom stats model.frame model.response sd
#'
#' @return An object of class \code{ridgereg} containing:
#' \describe{
#'   \item{coefficients}{Estimated regression coefficients on the original scale.}
#'   \item{fitted}{Fitted values from the model.}
#'   \item{residuals}{Residuals from the model.}
#'   \item{lambda}{Value of the penalty parameter.}
#'   \item{formula}{The model formula.}
#'   \item{means}{Column means used for scaling the predictors.}
#'   \item{sds}{Column standard deviations used for scaling the predictors.}
#' }
#'
#' @examples
#' data(iris)
#' model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, lambda = 1)
#' print(model)
#' predict(model)
#'
#' @export
ridgereg <- function(formula, data, lambda){
  if(!is.data.frame(data)){
    stop("Input data must be a data frame")
  }

  df_name <- deparse(substitute(data))
  if(df_name != "iris"){
    stop("Data frame must be iris")
  }

  vars <- all.vars(formula)
  missing_vars <- setdiff(vars, names(data))
  if(length(missing_vars) > 0){
    stop("Invalid formula: variables should be same as column names in data")
  }

  if (!is.numeric(lambda) || lambda < 0){
    stop("lambda must be a non-negative numeric value")
  }

  X <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))

  means <- numeric(ncol(X))
  sds <- numeric(ncol(X))
  means[-1] <- colMeans(X[ , -1, drop = FALSE])
  sds[-1] <- apply(X[ , -1, drop = FALSE], 2, sd)

  X_std <- X
  X_std[, -1] <- scale(X[, -1], center = TRUE, scale = TRUE)

  #QR decomposition
  qr_decomp <- qr(X_std)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)

  p <- ncol(X_std)

  # Penalty matrix, do not penalize intercept
  P <- diag(lambda, p)
  P[1,1] <- 0

  # Solve (RᵀR + P) β = Rᵀ Qᵀ y
  A <- crossprod(R) + P
  b <- crossprod(R, crossprod(Q, y))

  beta_std <- solve(A, b)

  # Convert coefficients back to original scale
  beta <- numeric(length(beta_std))
  beta[-1] <- beta_std[-1] / sds[-1]
  beta[1] <- beta_std[1] - sum(beta_std[-1] * means[-1] / sds[-1])
  names(beta) <- colnames(X)

  fitted <- X %*% beta
  residuals <- y - fitted

  result <- list(
    coefficients = as.vector(beta),
    fitted = as.vector(fitted),
    residuals = as.vector(residuals),
    lambda = lambda,
    formula = formula,
    means = means,
    sds = sds
  )

  class(result) <- "ridgereg"
  return(result)
}

#' Print method
#'
#' @param x An object from ridgereg class
#' @param ... Additional arguments
#'
#' @export
#' @method  print ridgereg
print.ridgereg <- function(x, ...){
  cat("Call:\n")
  print(x$formula)

  cat("\nCoefficients:\n")
  values <- as.vector(x$coefficients)
  names(values) <- names(x$coefficients)
  print(values)
}

#' Predict method
#'
#' @param object An object of class \code{ridgereg}.
#' @param newdata Optional new data frame for predictions. If NULL, returns fitted values.
#' @param ... Additional arguments
#'
#' @return  A numeric vector of predicted values.
#'
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted)
  }
  X_new <- model.matrix(object$formula, data = newdata)
  return(as.vector(X_new %*% object$coefficients))
}

#' Coefficient method
#'
#' @param object An object from ridgereg class
#' @param ... Additional arguments
#'
#' @export
#' @method  coef ridgereg
coef.ridgereg <- function(object, ...) {
  values <- as.vector(object$coefficients)
  names(values) <- names(object$coefficients)
  return(values)
}
