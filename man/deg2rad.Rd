\name{deg2rad}
\alias{deg2rad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts degrees to radians
}
\description{
Returns a vector after converting degrees to radians.\cr
When overlap is set to TRUE, output is contained within the [-pi, pi] set.
}

\usage{
deg2rad(deg, overlap = T)
deg2rad(deg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deg}{
Numeric vector containing the angle values in degrees.
}
  \item{overlap}{
A binary bit, set to FALSE by default.\cr
When set to TRUE, forces the outputs within the [-pi, pi] set.
}
}
\details{
Input argument "deg" must be a numeric vector. \cr
Input argument "overlap" must be a vector of dimension equal to unity. \cr
Input argument "overlap" must be of logical type. \cr
}
\value{
The returned value is always a vector of the same length as the argument "deg"
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
  test_pos <- c(test_pos, i*60)
}

v_pos <- deg2rad(test_pos, overlap = T)
test_pos
v_pos


#test code for negative
test_neg <- NULL
for (i in 1:12) {
  test_neg <- c(test_neg, -i*60)
}

v_neg <- deg2rad(test_neg, overlap = T)
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
