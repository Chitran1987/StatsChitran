\name{rad2deg}
\alias{rad2deg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts radians to degrees
}
\description{
Returns a vector after converting radians to degrees.\cr
When overlap is set to TRUE, output is contained within the [-180, 180] set.
}
\usage{
rad2deg(rad, overlap = T)
rad2deg(rad)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rad}{
Numeric vector containing the angle values in radians.
}
  \item{overlap}{
A binary bit, set to FALSE by default.\cr
When set to TRUE, forces the outputs within the [-180, 180] set.
}
}
\details{
Input argument "rad" must be a numeric vector. \cr
Input argument "overlap" must be a vector of dimension equal to unity. \cr
Input argument "overlap" must be of logical type. \cr
}
\value{
The returned value is always a vector of the same length as the argument "rad"
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
#test code for positive
test_pos <- NULL
for (i in 1:12) {
  test_pos <- c(test_pos, i*pi/3)
}

v_pos <- rad2deg(test_pos, overlap = T)
test_pos
v_pos


#test code for negative
test_neg <- NULL
for (i in 1:12) {
  test_neg <- c(test_neg, -i*pi/3)
}

v_neg <- rad2deg(test_neg, overlap = T)
test_neg
v_neg
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
