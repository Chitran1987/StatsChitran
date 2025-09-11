\name{is.prime.large}
\alias{is.prime.large}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a vector indicating which elements of the large input vector are primes
}
\description{
Returns a vector indicating which elements of the large input vector are primes.\cr
Uses c++ \code{uint64_t} data type under the hood. Use if you have very large vectors or integers.\cr
}
\usage{
is.prime.large(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
The large numeric vector of very large positive integers.\cr
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a boolean vector of the same length as the input
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
#Call the important library
library(StatsChitran)

#build the large vector
X <- floor(runif(10^7, min = 1, max = 10^10))
Y <- is.prime.large(X)
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
