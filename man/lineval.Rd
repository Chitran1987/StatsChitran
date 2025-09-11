\name{lineval}
\alias{lineval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a line through two said points
}
\description{
Given two points, (x1,y1) and (x2,y2) and a dataset X, the function spills out a vector Y\cr
Y is the sequence of values of the line for the corresponding values of X
}
\usage{
lineval(v1, v2, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1, v2}{
Two element numeric (atomic) vectors
}
  \item{X}{
Numeric sequence for which Y values are required
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The return type is a numeric vector(Y) with the same length as X
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
X <- seq(0, 20, by=0.01)
v1 <- (0,0)
v2 <- (3,4)
Y <- lineval(v1, v2, X)
plot(X, Y)
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
