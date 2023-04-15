\name{num_integrate}
\alias{num_integrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the 2D reimann integration of a curve within specified limits
}
\description{
The function returns a single numeric scalar as a result for the integration between upper and lower bounds
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
  Caution: xmin >= min(X), otherwise an error will be thrown.
}
  \item{xmax}{
  The upper bound of the integral\cr
  Caution: xmax <= max(X), otherwise an error will be thrown.
}
  \item{type}{
  The type defines the kind of reimann integral and is ordinaarily set to "avg".\cr
  The other possible options are "left" and "right". Defines the placement of the       reimann rectangles w.r.t. the vertical y-axis height being considered.\cr
  The "avg" input averages between the left and right reimann integrals.
}
}
\details{
The X vector should include the bounds of xmax and xmin or else an error will be thrown. Look at the arguments section above.\cr
The funcion is by default set to type='avg'. Hence, if needed, this factor can be changed to 'left' or 'right'.\cr
The no. of points within the bounds of xmax and xmin should at least be 4. \cr
The sampling density in the dataset should allow on an average for at least 4 points within the xmax and xmin window length. \cr
}
\value{
The function always returns a numeric scalar\cr
}
\references{
\link{https://en.wikipedia.org/wiki/Riemann_integral}{Reimann Integration}\cr
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
