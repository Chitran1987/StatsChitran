\name{seq.sc}
\alias{seq.sc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a numeric vector whose elements follow an arithmetic-progression defined by length
}
\description{
The function generates a sequence of numbers, \eqn{S_k}, which follow an arithmetic-progression as shown below.\cr
\deqn{S_k = S_1 + (k - 1)d}\cr
Here \eqn{d} is automatically already calculated from \eqn{n}, \eqn{st} and \eqn{end}\cr
}
\usage{
seq.sc(st, end, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{st}{
The starting point of the sequence defined as \eqn{S_1}
}
  \item{end}{
The ending point of the sequence defined as \eqn{S_n = S1 + (n -1)d}
}
  \item{n}{
The no. of elements in the sequence
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a vector
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
\code{\link{ArithProg}}
}
\examples{
#Call the important functions
library(StatsChitran)
#Call the function
X <- seq.sc(st = -10, end = 10, n = 10^6)
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
