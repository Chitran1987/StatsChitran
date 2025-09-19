\name{ArithProg}
\alias{ArithProg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a numeric vector whose elements follow an arithmetic progression
}
\description{
The function generates a sequence of numbers, \eqn{S_k}, which follow an arithmetic progression as shown below.\cr
\deqn{S_k = S_1 + (k - 1)d}
}
\usage{
ArithProg(st=0, n, d)
}
\arguments{
  \item{st}{
The term, \eqn{S_1}, starting point of your arithmetic progression. Defaults to zero.
}
  \item{n}{
Defines the no. of terms in the progression.\cr
Expects an integer \eqn{\geq 1}
}
  \item{d}{
Denotes the difference term, \eqn{d = S_{k+1} - S_k}, between consecutive terms of the progression.
}
}
\details{
}
\value{
The return type of the function is always a vector containing \eqn{S_k,\,\,\, \forall k \in [1, n]}
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
rm(list=ls())
v1 <- ArithProg(n=3,d=3)
v1

v2 <- ArithProg(st=11, n=10,d=3)
v2

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ progressions }% use one of  RShowDoc("KEYWORDS")
\keyword{ arithmetic progression }% __ONLY ONE__ keyword per line
\keyword{ sequences }
