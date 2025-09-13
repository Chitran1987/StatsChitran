\name{gauss.avg.reglzn}
\alias{gauss.avg.reglzn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Iterative gaussian average smoother with a roughness penalty over a 2D X vs Y dataset
}
\description{
Applies a repeated gaussian average smoothening to noisy data\cr
Iterates till the combined MSE + penalty based objective function \eqn{\mathcal{O}(n)} finds a minima. See \emph{details}
}
\usage{
gauss.avg.reglzn.Rd(X, Y, bn, fn, sig, lambda, ord.min = 1, ord.max, pl = T, grid.search = T)
gauss.avg.reglzn.Rd(X, Y, bn, fn, sig, lambda, ord.max)
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
Backward and Forward window sizes. Please see \code{\link{gauss.avg}}
}
  \item{lambda}{
  Co-efficient for the regularization penalty shown in the \emph{description} as \eqn{\lambda}\cr
  At \eqn{\lambda = 0}, the \code{ord} parameter will converge to the \code{ord.min} parameter since the MSE term is lowest there\cr
  For very large \eqn{\lambda}, the smoothing will overshadow the nature of the data. So \eqn{\lambda} should be picked carefully.
}
  \item{sig}{
The standard deviation, \eqn{\sigma}, of the gaussian kernel used. Please see \code{\link{gauss}}, \code{\link{gauss.avg}}
}
  \item{ord.min, ord.max}{
Min and max values of the \code{ord} variable of the gaussian moving average. Please see \code{\link{gauss.avg}}\cr
Must be positive integers
}
  \item{pl}{
Plots the relevant final gaussian average dataset over the noisy data when \code{TRUE} \cr
Defaults to true
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
The function runs an optimization, either through \code{grid.search} or through \link[rgenoud]{genoud}, to estimate \eqn{n} \cr
Herein, \eqn{f(n, \sigma, x)} and \eqn{\mathcal{O}(n)} are the \code{\link{gauss.avg}} and objective functions respectively \cr
\deqn{\displaystyle f(n, \sigma, x) = \code{gauss.avg(X, Y, bn, fn, sig, ord = n, pl)}}
\deqn{\displaystyle \mathcal{O}(n) = \frac{1}{N}\sum_i \Big\{ y_i - f(n, \sigma, x_i) \Big\}^2 + \lambda \int_{\mathbb{R}} \{\ddot{f}(x, \sigma, n)\}^2 \mathrm{d}x}
\deqn{\displaystyle \min_{n}\mathcal{O}(n)}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Import the noisy dataset and plot it
data("noisy.gaussians")
df <- noisy.gaussians
plot(df$X, df$Y)

##Call the gauss.avg function to see whether the smoothing is moving away from the original data
df1 <- gauss.avg(df$X, df$Y, bn = 3, fn = 3, sig = 1, pl = F, ord = 10)
df2 <- gauss.avg(df$X, df$Y, bn = 3, fn = 3, sig = 1, pl = F, ord = 100)
lines(df1$X, df1$Y, col = 'green', lwd = 2)
lines(df2$X, df2$Y, col = 'red', lwd = 2)
legend(x = 'topleft', legend = c('data', 'gauss.avg, ord = 10', 'gauss.avg, ord = 100'), fill = c('white', 'green', 'red'))

##It is clear that ord = 100 is moving away from the data while ord = 1 is too noisy
##Pick a very low value of lambda and then minimize O(n) by calling the gauss.avg.reglzn function
L <- gauss.avg.reglzn(df$X, df$Y, bn = 3, fn = 3, sig = 1, lambda = 0.005, ord.min = 10, ord.max = 100)
df3 <- L$smoothed_dataframe
##Figure what the optimized order parameter is (given the specified value of lambda)
L$par

##Compare the plots of the ord = 10, ord = L$par, ord = 100
plot(df$X, df$Y)
lines(df1$X, df1$Y, col = 'green', lwd = 2)
lines(df2$X, df2$Y, col = 'red', lwd = 2)
lines(df3$X, df3$Y, col = 'blue', lwd = 2)
legend(x = 'topleft', legend = c('data', 'gauss.avg, ord = 10', 'gauss.avg, ord = 100', 'gauss.avg, optimized'), fill = c('white', 'green', 'red', 'blue'))

##Once converged plot the function O(n) versus n to verify L$par
df_reg <- L$GridOfVariance
plot(df_reg$ord.val, df_reg$ord.reg, xlab = 'n', ylab = 'O(n)', main = 'Objective function versus smoothing order')
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
