\name{err.vec.dim.bit}
\alias{err.vec.dim.bit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a TRUE boolean bit if the argument is not a vector of a specified dimension
}
\description{
Returns a TRUE boolean bit if the argument is not an atomic vector of a specified dimension.
}
\usage{
err.vec.dim.bit(v, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
  The vector/item under scrutiny
  }
  \item{n}{
  The size of the vector/object v
  }
}
\details{
Returns TRUE if the value v is NULL\cr
The vector must be of atomic class(excludes lists/dataframes, i.e. returns TRUE if they are)\cr
No constraints imposed on the contents of the vector, i.e. returns FALSE if the other criteria are satisfied\cr
}
\value{
Return type is a boolean bit. \cr
Returns TRUE if an error-worthy exception is detected. \cr
}
\references{
boolean return version of err.vec.dim(v, n) .\cr
See function err.vec.dim(). \cr
See also err.WN.vec.dim() and err.WN.vec.dim.bit(). \cr
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
v <- c(1.3, 2.0)
w <- c(1.0, 2.0)
L <- list(v, w)
err.vec.dim.bit(v, 2)
err.vec.dim.bit(w, 2)
err.vec.dim.bit(w, 3)
err.vec.dim.bit(L, 2)
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
