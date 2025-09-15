\name{dot.product}
\alias{dot.product}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the dot product
}
\description{
Calculates the dot product between two vectors
}
\usage{
dot.product(X, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Second numeric Vector
}
\item{Y}{
Second numeric vector
}
}
\details{
If \eqn{\bar{X}} and \eqn{\bar{Y}} are the vectors, then \eqn{X\cdot Y} is defined as below
\deqn{\displaystyle \bar{X}\cdot\bar{Y} = \sum_{i = 1}^n x_i y_i}
}
\value{
Returns a numeric scalar
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
## Create the X and Y vectors
X = c(1, 0, 1)
Y = c(0, 1, 0)
dot.product(X, Y)
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
