\name{ArithProg}
\alias{ArithProg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a numeric vector whose elements follow an arithmetic progression
}
\description{
The function generates a sequence of numbers which follow an arithmetic progression and which can be tuned by the function arguments
}
\usage{
ArithProg(st=0, n, d)
}
\arguments{
  \item{st}{
The starting point of your arithmetic progression set to a default of zero
}
  \item{n}{
Defines the length of the vector returned by the function (or defines how many terms are present in the proghression)
}
  \item{d}{
Defines the distance between the numbers of the arithmetic progression.
}
}
\details{
The function always returns a vector. There are no other return types or no way to tune the return type in this function.
}
\value{
The return type of the function is always a vector
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
ArithProg(n=3,d=3)
[1] 0 3 6

ArithProg(st=11, n=10,d=3)
[1] 11 14 17 20 23 26 29 32 35 38


ArithProg<-function(st=0, n, d){
  v<-vector(mode = 'numeric', length = n)
  for (i in 1:n) {
    v[i]<-st+(i-1)*d
  }
  return(v)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ progressions }% use one of  RShowDoc("KEYWORDS")
\keyword{ arithmetic progression }% __ONLY ONE__ keyword per line
\keyword{ sequences }
