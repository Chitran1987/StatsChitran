\name{GeomProg}
\alias{GeomProg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a numeric vector whose elements follow an geometric progression
}
\description{
The function generates a sequence of numbers which follow an geometric progression and which can be tuned by the function arguments
}
\usage{
GeomProg(r=3, n=10)
GeomProg(st=5, r=3, n=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{st}{
The starting point of your geometric progression set to a default of one
}
  \item{n}{
Defines the length of the vector returned by the function (or defines how many terms are present in the proghression)
}
  \item{r}{
Defines the ratio with which the geometric series progresses
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
GeomProg(r=3, n=10)
 [1]     1     3     9    27    81   243   729  2187  6561 19683

 GeomProg(st<-7, r=3, n=10)
 [1]      7     21     63    189    567   1701   5103  15309  45927 137781

## The function is currently defined as
GeomProg<-function(st=1,r,n){
  v<-vector(mode = 'numeric', length = n)
  for (i in 1:n) {
    v[i]<-st*(r^(i-1))
  }
  return(v)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ progressions }% use one of  RShowDoc("KEYWORDS")
\keyword{ geometric progression }% __ONLY ONE__ keyword per line
\keyword{ sequences }
