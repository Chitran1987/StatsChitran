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
The starting point of your geometric progression set to a default of one. Denoted as \eqn{a_0}
}
  \item{n}{
Defines the length of the vector returned by the function (or defines how many terms are present in the proghression). Denoted as \eqn{n}
}
  \item{r}{
Defines the ratio with which the geometric series progresses. Denoted as \eqn{r}
}
}
\details{
The function returns a sequence \eqn{S_k = a_0 r^{k-1} \quad k \in \{1, 2, 3\, \dots \, n\} }
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
