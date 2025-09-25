#' Fit a Linear Regression Model
#'
#' This function fits a linear regression model using the least squares method
#' and returns an object of class \code{linreg} with coefficients, fitted values,
#' residuals, and other statistics.
#'
#' @importFrom stats model.matrix fitted residuals sd pt
#' @importFrom stats resid pt
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline theme_classic ggtitle xlab ylab
#' @importFrom patchwork wrap_plots
#' @importFrom stats symnum
#'
#' @param formula A formula describing the model (e.g., y ~ x1 + x2).
#' @param data A data frame containing the variables in the formula.
#'
#' @return An object of class \code{linreg} with the following components:
#' \item{coefficients}{Estimated regression coefficients}
#' \item{fitted}{Fitted values}
#' \item{residuals}{Residuals}
#' \item{df}{Degrees of freedom}
#' \item{residual_variance}{Residual variance estimate}
#' \item{var_coefficients}{Variance-covariance matrix of coefficients}
#' \item{t_values}{t-statistics of the coefficients}
#' \item{formula}{The model formula}
#'
#' @export
linreg <- function(formula, data){
  x <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]

  #Calculate regression coefficient
  beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y

  #Fitted value
  y_hat <- x %*% beta_hat

  #residuals
  e_hat <- y - y_hat

  #Degrees of freedom
  n <- nrow(x)
  p <- ncol(x)
  df <- n-p

  #Residual variance
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

#' @export
#' @method resid linreg
resid.linreg <- function(object, ...){
  object$residuals
}

#' @export
#' @method  coef linreg
coef.linreg <- function(object, ...){
  values <- as.vector(object$coefficients)
  names(values) <- colnames(object$var_coefficients)
  values
}

pred <- function(x, ...) {
  UseMethod("pred")
}

#' @export
#' @method pred linreg
pred.linreg <- function(x, ...){
  x$fitted
}

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

#' @export
#' @method plot linreg
plot.linreg <- function(x, ...) {

  fr <- data.frame(fitted = x$fitted, residuals = x$residuals)

  plot1 <- ggplot(fr, aes(x = fitted, y = residuals)) +
            geom_point(shape = 1) +
            geom_smooth(method = "lm", se = TRUE, color = "red") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
            theme_classic() +
            ggtitle("Residuals vs Fitted") +
            xlab(paste("Fitted values\n", deparse(x$formula))) +
            ylab("Residuals")

  sqrt_sr <- sqrt(abs(x$residuals / sd(x$residuals)))
  fitted <- x$fitted

  plot2 <- ggplot() +
            geom_point(aes(x = fitted, y = sqrt_sr), shape = 1) +
            geom_smooth(method = "lm", se = TRUE, color = "red") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
            theme_classic() +
            ggtitle("Scale-Location") +
            xlab(paste("Fitted values\n", deparse(x$formula))) +
            ylab(expression(sqrt("|Standardized residuals|")))

  combined_plot  <- plot1 / plot2
  combined_plot

}
