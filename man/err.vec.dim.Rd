\name{err.vec.dim}
\alias{err.vec.dim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Throws an error if the argument is not a vector of a specified dimension
}
\description{
Throws an error if the argument is not an atomic vector of a specified dimension.
}
\usage{
err.vec.dim(v, n)
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
Throws an error if the value v is NULL\cr
The vector must be of atomic class(excludes lists)\cr
No constraints imposed on the contents of the vector\cr
}
\value{
Return type is NULL
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
v <- c(1.3, 2.0)
w <- c(1.0, 2.0)
L <- list(v, w)
err.vec.dim(v, 2)
err.vec.dim(w, 2)
err.vec.dim(w, 3)
err.vec.dim(L, 2)
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
