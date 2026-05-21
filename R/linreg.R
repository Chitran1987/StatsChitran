#' Fit a multiple linear regression model using the normal equations
#'
#' `linreg()` estimates the coefficients of a multiple linear regression model
#' using the normal-equation form of ordinary least squares. The input must be a
#' data frame in which the last column is treated as the dependent/response
#' variable and all preceding columns are treated as independent/predictor
#' variables.
#'
#' The function constructs the normal-equation matrix manually and solves for the
#' regression coefficients. An intercept term is included automatically. If the
#' normal-equation matrix is computationally singular, which can occur for
#' example when the predictor variables suffer from strong multicollinearity, the
#' function returns `NULL`.
#'
#' @param df A data frame containing the regression data. The last column is
#'   interpreted as the dependent/response variable. All preceding columns are
#'   interpreted as independent/predictor variables.
#'
#' @return A numeric vector containing the estimated regression coefficients.
#'   The first coefficients correspond to the predictor variables in the order
#'   in which they appear in `df`. The final coefficient is the intercept term.
#'   If the normal-equation matrix is computationally singular, the function
#'   returns `NULL`.
#'
#' @details
#' This function implements ordinary least squares by explicitly constructing and
#' solving the normal equations. Because the method relies on matrix inversion,
#' the calculation may fail or become unstable when the predictor variables are
#' linearly dependent or nearly linearly dependent. In such cases, the determinant
#' of the normal-equation matrix becomes very small and the function returns
#' `NULL`.
#'
#' @examples
#' data('reg_data1')
#' linreg(reg_data1)
#'
#' # The dataset reg_data1 is included with the StatsChitran package.
#' # Example output:
#' # [1] 2.950339 5.052787 0.999999 9.599983
#'
#' data('reg_data2')
#' linreg(reg_data2)
#'
#' # The dataset reg_data2 is included with the StatsChitran package.
#' # This example returns NULL when the input data suffer from multicollinearity.
#'
#' @export
linreg <- function(df) {
  m <- matrix(NA, ncol = dim(df)[2], nrow = dim(df)[2])

  for (i in 1:(dim(m)[1])) {
    for (j in 1:(dim(m)[1])) {
      if (i != dim(m)[1] & j != dim(m)[2]) {
        m[i, j] <- sum((df[, i]) * (df[, j]))
      } else {
        if (i == dim(m)[1] & j == dim(m)[1]) {
          m[i, j] <- dim(df)[1]
        } else {
          if (i == dim(m)[1]) {
            m[i, j] <- sum(df[, j])
          } else {
            m[i, j] <- sum(df[, i])
          }
        }
      }
    }
  }

  # creating the vector
  v <- vector(mode = "numeric", length = dim(m)[1])

  for (i in 1:length(v)) {
    if (i != length(v)) {
      v[i] <- sum((df[, i]) * (df[, length(v)]))
    } else {
      v[i] <- sum(df[, i])
    }
  }

  b <- matrix(v, ncol = 1)

  if (det(m) < 10^(-15)) {
    message("matrix computationally singular")
    return(NULL)
  } else {
    res <- as.vector(solve(m, b))
    return(res)
  }
}
