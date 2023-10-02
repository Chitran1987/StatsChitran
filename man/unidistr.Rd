\name{unidistr}
\alias{unidistr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
returns a vector with a uniform distribution aginst the input vector X
}
\description{
The function takes in a vector "X"" against which it returns a vector with a uniform distribution, given the other boundary parameters "a" and "b".
}
\usage{
unidistr(X, a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Describes the (numeric) vector against which you need the uniformly distributed vector
}
  \item{a}{
Describes the lower bound of X below which the function returns a zero value
}
  \item{b}{
Describes the upper bound of X above which the function returns a zero value
}
}
\details{
1. The lower bound should be LESS THAN(NOT EQUAL TO)LOWER BOUND. \cr
2. If a is NOT less than b, function throws an error(careful using constrained optimization). \cr
}
\value{
The returned value is always a numeric vector.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
1. Unlike the "gauss(X, sig,...)" function, the function does not have a probability term. \cr
2. a equal to b makes no sense since then the entire function collapses to something like a \href{https://en.wikipedia.org/wiki/Dirac_delta_function}{Dirac delta} function.\cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
X <- seq(-10, 10, by= 0.05)
Y1 <- unidistr(X, a = -1, b = 1)
Y2 <- unidistr(X, a = -0.5, b = 0.6)
Y3 <- unidistr(X, a = -0.2, b = 0.4)
plot(X, Y3, type='l', col=rgb(0,0,1, 0.5))
lines(X, Y2, type = 'l', col=rgb(0,1,0, 0.5))
lines(X, Y1, type = 'l', col=rgb(1,0,0, 0.5))
legend(x='topleft', legend = c('Y3', 'Y2', 'Y1'), fill = c(rgb(0,0,1, 0.5), rgb(0,1,0, 0.5), rgb(1,0,0, 0.5)))
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
