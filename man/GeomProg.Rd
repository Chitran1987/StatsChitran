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
v1 <- GeomProg(r=3, n=10)
v1

v2 <- GeomProg(st<-7, r=3, n=10)
v2


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ progressions }% use one of  RShowDoc("KEYWORDS")
\keyword{ geometric progression }% __ONLY ONE__ keyword per line
\keyword{ sequences }
