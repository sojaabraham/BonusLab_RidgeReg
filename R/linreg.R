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
  print(x$formula)
  cat("Coefficients:\n")
  values <- as.vector(x$coefficients)
  names(values) <- colnames(x$var_coefficients)
  print(values)
  invisible(x)
}
