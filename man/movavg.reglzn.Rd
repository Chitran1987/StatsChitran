\name{movavg.reglzn}
\alias{movavg.reglzn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Iterative moving average smoother with a roughness penalty over a 2D X vs Y dataset
}
\description{
Applies repeated moving average smoothing to noisy data, by using an \code{ord} term like \link{movavg}.\cr
The optimizer searches for a minima in the objective function \eqn{\mathcal{O(n)}} or performs a grid search over a supplied set for this minima. See \emph{details}\cr
}
\usage{
movavg.reglzn(X, Y, bn, fn, lambda, ord.min, ord.max, pl, grid.search)
movavg.reglzn(X, Y, bn, fn, lambda, ord.max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  X-component of the X-Y dataset
  }
  \item{Y}{
  Y-component of the X-Y dataset
  }
  \item{bn, fn}{
  backward forward window sizes. Please see \link{movavg}
  }
  \item{lambda}{
  Co-efficient for the regularization penalty shown in the \emph{description} as \eqn{\lambda}\cr
  At \eqn{\lambda = 0}, the \code{ord} parameter will converge to the \code{ord.min} parameter since the MSE term is lowest there\cr
  For very large \eqn{\lambda}, the smoothing will overshadow the nature of the data. So \eqn{\lambda} should be picked carefully.
  }
  \item{ord.min, ord.max}{
  The minimum and maximum values of the order of the moving average \code{ord}. See \link{movavg}\cr
  Must be positive integers \cr
  \code{ord.min} defaults to one, if not provided
  }
  \item{pl}{
  Plots the relevant final moving average over the noisy X-Y data after completing the optimization search for the value of \eqn{n}. See \emph{description}\cr
  Defaults to \code{TRUE}
  }
  \item{grid.search}{
  \bold{If \code{TRUE}} \cr
  Performs a grid search for the least values of \eqn{\mathcal{O}(n)} over \eqn{n}\cr
  Grid search perfeormed between integer values of \code{ord.min} and \code{ord.max}\cr
  Defaults to \code{TRUE} and is recommended\cr

  \bold{If \code{FALSE}} \cr
  Performs a \link[rgenoud]{genoud} optimization to minimize \eqn{\mathcal{O}(n)}\cr
  Is not recommended for slower processors or large datasets. Process is slow and power hungry \cr
  }
}
\details{
Herein, \eqn{f(n,x)} and \eqn{\mathcal{O}(n)} are the \link{movavg} and objective functions respectively \cr
\deqn{f(n, x) = \code{movavg(X, Y, bn, fn, ord = n, pl)}} please see \link{movavg} \cr
\deqn{\mathcal{O}(n) = \{\sum_i (y_i - f(n, x_i))^2)\}/N + \lambda \int \{\ddot{f}(x)\}^2 \mathrm{d}x} \cr
\deqn{\min_{n}\mathcal{O}(n)}
}
\value{
Returned value is always a 3-element list

\bold{If \code{grid.search = TRUE}}\cr
List contains \cr
1) \code{GridOfVaraiance}: A dataframe showing the values of \eqn{\mathcal{O}(n)} against values of \eqn{n} \cr
2) \code{smoothed_dataframe}: Containing the X and Y values after smoothing \cr
3) \code{par}: The parameter or value of \eqn{n} for which the function \eqn{\mathcal{O}(n)} was found to be minimum

\bold{If \code{grid.search = FALSE}}\cr
List contains \cr
1) \code{optim-data}: The result/return list of the \link[rgenoud]{genoud} optimization \cr
2) and 3) are the same for the \code{grid.search = TRUE} case
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{movavg}}
}
\examples{
###Load the dataset
##Import the noisy dataset and plot it
df <- noisy.gaussians
plot(df$X, df$Y)

##Call the movavg function to see whether the smoothing is moving away from the original data
df1 <- movavg(df$X, df$Y, bn = 3, fn = 3, ord = 10)
df2 <- movavg(df$X, df$Y, bn = 3, fn = 3, ord = 100)
lines(df1$X, df1$Y, col = 'green', lwd = 2)
lines(df2$X, df2$Y, col = 'red', lwd = 2)
legend(x = 'topleft', legend = c('data', 'movavg, ord = 10', 'movavg, ord = 100'), fill = c('white', 'green', 'red'))

##It is clear that ord = 100 is moving away from the data while ord = 1 is too noisy
##Pick a very low value of lambda and then minimize O(n) by calling the movavg.reglzn function
L <- movavg.reglzn(df$X, df$Y, bn = 3, fn = 3, lambda = 0.005, ord.min = 10, ord.max = 100)
df3 <- L$smoothed_dataframe
##Figure what the optimized order parameter is (given the specified value of lambda)
L$par

##Compare the plots of the ord = 10, ord = L$par, ord = 100
plot(df$X, df$Y)
lines(df1$X, df1$Y, col = 'green', lwd = 2)
lines(df2$X, df2$Y, col = 'red', lwd = 2)
lines(df3$X, df3$Y, col = 'blue', lwd = 2)
legend(x = 'topleft', legend = c('data', 'movavg, ord = 10', 'movavg, ord = 100', 'movavg, optimized'), fill = c('white', 'green', 'red', 'blue'))

##Once converged plot the function O(n) versus n to verify L$par
df_reg <- L$GridOfVariance
plot(df_reg$ord.val, df_reg$ord.reg)
abline(v = L$par, col = 'gray', lwd = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
