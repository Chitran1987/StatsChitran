\name{prime.till}
\alias{prime.till}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a boolean/logical indicating prime nos till a given integer \eqn{N}
}
\description{
Returns a boolean/logical indicating prime nos till a given integer \eqn{N}\cr
Efficient for very large \eqn{N}
}
\usage{
prime.till(N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
The integer, \eqn{N}, till which you want the primes returned
}
}
\details{
For \eqn{N \geq 10^6}, uses multiple cores of you system for faster calculations
}
\value{
The returned value is always a boolean/logical vector indicating which nos are primes
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
\code{\link{is.prime}}, \code{\link{prime.firstN}}
}
\examples{
#Call the library
library(StatsChitran)

vec <- prime.till(10^7)
tail(vec)
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
