\name{is.prime}
\alias{is.prime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a vector indicating which elements of the input vector are primes
}
\description{
Returns a vector indicating which elements of the input vector are primes.\cr
For very large integers or very large vectors, use \code{\link{is.prime.large}}
}
\usage{
is.prime(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
Numeric vector of positive integers
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
is.prime(c(37, 53, 71, 43, 63, 9, 117, 127))
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
