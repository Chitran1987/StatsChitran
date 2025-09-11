\name{num.integrate.riemann}
\alias{num.integrate.riemann}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the 2D Riemann summation/integration of a curve within specified limits
}
\description{
The function returns a single numeric scalar as a result for the Riemann summation approximation of the integration between upper and lower bounds
}
\usage{
num.integrate.riemann(X, Y, xmin, xmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A numeric vector containing the X values of the curve you want to integrate
}
  \item{Y}{
A numeric vector containing the Y values of the curve you want to integrate
}
  \item{xmin}{
The lower bound of the integral.\cr
  Caution: \code{xmin >= min(X)}, otherwise an error will be thrown.
}
  \item{xmax}{
The upper bound of the integral\cr
  Caution: \code{xmax <= max(X)}, otherwise an error will be thrown.
}
}
\details{
The X vector should include the bounds of xmax and xmin or else an error will be thrown. Look at the arguments section above.\cr
The no. of points within the bounds of xmax and xmin should at least be 4. \cr
If the no. of data points in X or Y are \eqn{n \geq 5 \times 10^5}, then the Rcpp backend uses all cores of the machine to parallelize the computaion.\cr
The function simply solves for the integral using the Riemann summation shown below.\cr
\deqn{\displaystyle \int_{x_{min}}^{x_{max}}y(x)\,\mathrm{d}x \approx \sum_{i = 1}^{n-1}y_i\cdot (x_{i+1} - x_i)}
}
\value{
The function always returns a numeric scalar\cr
}
\references{
\url{https://en.wikipedia.org/wiki/Riemann_integral}
}
\author{
Chitran Ghosal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{num_integrate}}
}
\examples{
#Call the important libraries
library(StatsChitran)
X <- seq(-10, 10, by = 10^-4)
Y <- gauss(X, probability = T)
num.integrate.riemann(X, Y, -1, 1)
num.integrate.riemann(X, Y, -2, 2)
num.integrate.riemann(X, Y, -3, 3)
num.integrate.riemann(X, Y, -4, 4)
num.integrate.riemann(X, Y, -5, 5)
num.integrate.riemann(X, Y, -10, 10)
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
