#' Linear Regression Model
#'
#' This function fits a linear regression model using QR Decomposition.
#' Returns an object of class \code{linreg} with coefficients, fitted values,
#' residuals, variance-covariance matrix, and other statistics.
#'
#' @importFrom stats model.matrix pt symnum
#' @importFrom patchwork wrap_plots
#'
#' @param formula A formula describing the model (e.g., y ~ x1 + x2).
#' @param data A data frame containing the variables in the formula.
#'
#' @return An object of class \code{linreg} containing:
#' \describe{
#'   \item{call}{The matched function call.}
#'   \item{coefficients}{Estimated regression coefficients.}
#'   \item{fitted}{Fitted values.}
#'   \item{residuals}{Residuals.}
#'   \item{df}{Residual degrees of freedom.}
#'   \item{residual_variance}{Estimated variance of residuals.}
#'   \item{var_coefficients}{Variance-covariance matrix of coefficients.}
#'   \item{t_values}{t-statistics for coefficients.}
#'   \item{formula}{The model formula.}
#' }
#'
#' @examples
#' data(iris)
#' model <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' print(model)
#' summary(model)
#' plot(model)
#'
#' @export
linreg <- function(formula, data){
  #Input validation
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }

  df_name <- deparse(substitute(data))
  if(df_name != "iris"){
    stop("Data frame must be iris")
  }

  vars <- all.vars(formula)
  if(!any(vars %in% names(data))){
    stop("Invalid formula: variables should be same as column names in data")
  }

  x <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]

  #QR decomposition
  qr_decomp <- qr(x)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)

  #Calculate regression coefficient
  beta_hat <- solve(R) %*% t(Q) %*% y

  #Fitted value
  y_hat <- x %*% beta_hat

  #residuals
  e_hat <- y - y_hat

  #Degrees of freedom
  n <- nrow(x)
  p <- ncol(x)
  df <- n-p

  #residuals variance
  sigma_Sq_hat <- as.numeric(t(e_hat) %*% e_hat)/ df

  #Variance of the regression coefficient
  var_beta_hat <- sigma_Sq_hat * solve(t(x) %*% x)

  #t-values for each coefficients
  t_beta <- as.vector(beta_hat)/sqrt(diag(var_beta_hat))

  result <- list(
    call = match.call(),
    coefficients = beta_hat,
    fitted = y_hat,
    residuals = e_hat,
    df = df,
    residual_variance = sigma_Sq_hat,
    var_coefficients = var_beta_hat,
    t_values = t_beta,
    formula = formula
  )

  class(result) <- "linreg"
  return(result)

}

#' Print method
#'
#' @param x An object from linreg class
#' @param ... Additional arguments
#'
#' @export
#' @method  print linreg
print.linreg <- function(x, ...){
  cat("Call:\n")
  if(!is.null(x$call)){
    print(x$call)
  } else {
    print(x$formula)
  }

  cat("\nCoefficients:\n")
  values <- as.vector(x$coefficients)
  names(values) <- colnames(x$var_coefficients)
  print(values)
}

#' Residual method
#'
#' @param x An object from linreg class
#' @param ... Additional arguments
#'
#' @export
resid <- function(x, ...) {
  UseMethod("resid")
}

#' @export
#' @method resid linreg
resid.linreg <- function(x, ...){
  x$residuals
}

#' Coefficient method
#'
#' @param object An object from linreg class
#' @param ... Additional arguments
#'
#' @export
#' @method  coef linreg
coef.linreg <- function(object, ...){
  values <- as.vector(object$coefficients)
  names(values) <- colnames(object$var_coefficients)
  values
}

#' Predict method
#'
#' @param x An object for which predictions are to be made.
#' @param ... Additional arguments
#'
#' @export
pred <- function(x, ...) {
  UseMethod("pred")
}

#' @export
#' @method pred linreg
pred.linreg <- function(x, ...){
  x$fitted
}

#' Summary method
#'
#' @param object An object from linreg class
#' @param ... Additional arguments
#'
#' @export
#' @method summary linreg
summary.linreg <- function(object, ...){
  standard_error <- sqrt(diag(object$var_coefficients))
  t_value <- as.vector(object$coefficients) / standard_error
  p_value <- 2 * stats::pt(-abs(t_value), df = object$df)

  stars <- symnum(p_value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))

  summary_coeff <- data.frame(
    Estimate = as.vector(object$coefficients),
    Standard_Error = standard_error,
    t_value = t_value,
    p_value = p_value,
    Signif = stars,
    row.names = colnames(object$var_coefficients)
  )

  cat("Call:\n")
  print(object$formula)
  cat("\nCoefficients:\n")
  print(summary_coeff)
  cat("\nResidual standard error:", round(sqrt(object$residual_variance), 4),
      "on", object$df, "degrees of freedom\n")

}

#' Plot method
#'
#' @param x An object from linreg class
#' @param ... Additional arguments
#'
#' @export
#' @method plot linreg
plot.linreg <- function(x, ...) {

  fr <- data.frame(fitted = x$fitted, residuals = x$residuals)

  plot1 <- ggplot2::ggplot(fr, ggplot2::aes(x = fr$fitted, y = fr$residuals)) +
            ggplot2::geom_point(shape = 1) +
            ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
            ggplot2::theme_classic() +
            ggplot2::ggtitle("Residuals vs Fitted") +
            ggplot2::xlab(paste("Fitted values\n", deparse(x$formula))) +
            ggplot2::ylab("Residuals")

  sqrt_sr <- sqrt(abs(x$residuals / stats::sd(x$residuals)))
  fitted <- x$fitted

  plot2 <- ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(x = fitted, y = sqrt_sr), shape = 1) +
            ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
            ggplot2::theme_classic() +
            ggplot2::ggtitle("Scale-Location") +
            ggplot2::xlab(paste("Fitted values\n", deparse(x$formula))) +
            ggplot2::ylab(expression(sqrt("|Standardized Residuals|")))

  combined_plot  <- plot1 / plot2
  combined_plot

}
