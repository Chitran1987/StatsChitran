\name{err.WN.vec.dim.bit}
\alias{err.WN.vec.dim.bit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a TRUE boolean bit if the argument vector is not a whole-number vector of a specified dimension
}
\description{
Returns a TRUE boolean bit if the argument is not an atomic vector withh whole-number elements(could be of type 'double', not necessarily of type 'int') and must have a specified dimension.
}
\usage{
err.WN.vec.dim.bit(v, n)
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
The vector must be of atomic class(excludes lists or Dataframes. Returns TRUE if they are)\cr
The elements of the object/vector v could be whole no.s of type 'double'. Not necessarily 'int'. Returns FALSE in this case.\cr
}
\value{
Return type is a boolean bit. \cr
Returns TRUE if an error-worthy exception is detected. \cr
}
\references{
boolean return version of err.WN.vec.dim(v, n) .\cr
See function err.WN.vec.dim(). \cr
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
err.WN.vec.dim.bit(v, 2)
err.WN.vec.dim.bit(w, 2)
err.WN.vec.dim.bit(w, 3)
err.WN.vec.dim.bit(L, 2)
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
