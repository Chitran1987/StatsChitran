\name{is.num_complex}
\alias{is.num_complex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks whether a given vector is either a complex no. or a numeric array.
}
\description{
returns a boolean TRUE if the vector is a numeric OR complex vector.
}
\usage{
is.num_complex(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The item x must be a vector or a matrix.
}
}
\details{
Function was introduced to take care of complex vectors and numeric vectors since mathematically a complex and numeric matrix would occupy the same space *C^{m,n}* space.\cr
In comp. sci. parlancethey are diffrent.\cr
Hence was introduced to ease the build up of mathematical functions.
}
\value{
The value returned is a SINGLE boolean value and NOT a boolean vector or matrix.\cr
This is the case since you can't have a complex no., a numeric and a character in the same vector or matrix in R.\cr
Hence only a single bit is returned indicating the type of vector or matrix being used.
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
 library(StatsChitran) ##Call the library

 M <- matrix(data = c(1, 2, 3+2i, 'a'), nrow=2, ncol=2, byrow=T)
 is.num_complex(M)

 M <- matrix(data = c(1, 2, 3+2i, 4), nrow=2, ncol=2, byrow=T)
 is.num_complex(M)

 M <- matrix(data = c(1, 2, 3, 4), nrow=2, ncol=2, byrow=T)
 is.num_complex(M)

 ###The function is currently defined as
is.num_complex <- function(X){
  return(is.numeric(X) | is.complex(X))
}
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
