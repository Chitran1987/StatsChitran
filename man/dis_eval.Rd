\name{dis_eval}
\alias{dis_eval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the euclidean distance between two given points in any n-dimensional space
}
\description{
Function returns the euclidean distance metric between any two points
}
\usage{
dis_eval(v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1}{
The first n-dimensional vector
}
  \item{v2}{
The second n-dimensional vector
}
}
\details{
The length of the vectors need to be the same else an error would be returned.\cr
}
\value{
The returned value is always a scalar
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
p1 <- c(1, 4, 6)
p2 <- c(2, 5, 7)
p3 <- c(1, 2)
p4 <- c(4, 6)
dis_eval(p1, p2)
dis_eval(p1, p3)
dis_eval(p1, p4)
dis_eval(p2, p3)
dis_eval(p2, p4)
dis_eval(p3, p4)
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
