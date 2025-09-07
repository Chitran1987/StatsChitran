\name{num_integrate}
\alias{num_integrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the 2D trapezoid integration of a curve within specified limits
}
\description{
The function returns a single numeric scalar as a result for the trapezoidal approximation of the integration between upper and lower bounds
}
\usage{
num_integrate(X,Y, xmin=2, xmax=5)
num_integrate(X,Y,xmin=2, xmax=5, type='avg')
num_integrate(X,Y, xmin=2, xmax=5, type='left')
num_integrate(X,Y, xmin=2, xmax=5, type='right')
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
  \item{type}{
  The type defines the kind of reimann integral sum and is ordinaarily set to "avg".\cr
  The other possible options are "left" and "right". Defines the placement of the       reimann rectangles w.r.t. the vertical y-axis height being considered.\cr
  The "avg" input averages between the left and right reimann integrals.
}
}
\details{
The X vector should include the bounds of xmax and xmin or else an error will be thrown. Look at the arguments section above.\cr
The no. of points within the bounds of xmax and xmin should at least be 4. \cr
If the no. of data points in X or Y are \eqn{n \geq 5 \times 10^5}, then the Rcpp backend uses all cores of the machine to parallelize the computaion.\cr
The function simply solves for the integral shown below.\cr
\deqn{\displaystyle \int_{x_{min}}^{x_{max}}y(x)\,\mathrm{d}x \approx \sum_{i = 1}^{n-1}\frac{y_i + y_{i+1}}{2}\cdot (x_{i+1} - x_i)}
}
\value{
The function always returns a numeric scalar\cr
}
\references{
\url{https://en.wikipedia.org/wiki/Riemann_integral}\cr
\url{https://en.wikipedia.org/wiki/Trapezoidal_rule}
}
\author{
Chitran Ghosal
}
\note{
The \code{type} argument has been phased out of the present implementation and has no effect on the output. \cr
It is simply made available for backward compatibility.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{num.integrate.riemann}}
}
\examples{
X <- seq(0,10, by=0.001)
Y <-2*X
num_integrate(X, Y, xmin=2, xmax=8)
num_integrate(X, Y, xmin=2, xmax=8, type='avg')
num_integrate(X, Y, xmin=2, xmax=8, type='left')
num_integrate(X, Y, xmin=2, xmax=8, type='right')
num_integrate(X,Y, xmin=2, xmax=2)
num_integrate(X,Y, xmin=6, xmax=6)
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
