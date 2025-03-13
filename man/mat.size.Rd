\name{mat.size}
\alias{mat.size}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sizes a matrix down to its mask but doesn't etch the values within
}
\description{
Transforms a matrix called \code{org}, represented by \eqn{\mathbf{Z} = (z_{ij})_{p\times q}}, using a matrix called \code{mask}, represented by \eqn{\mathbf{M} = (m_{ij})_{p \times q}}.\cr
Both matrices, \code{org} and \code{mask} have the same dimensionality, i.e. \eqn{p \times q}.\cr
\code{org} is real or complex, but \code{mask} is necessarily boolean, i.e. \eqn{\mathbf{Z} \in \mathbb{C}^{p\times q}\,\wedge \, \mathbf{M}\in\mathbb{B}^{p\times q}}.\cr
\code{mat.size} weans \code{org} down to the bare minimum size equivalent to \code{mask}, but does \bold{NOT} etch the \code{org} at all.\cr
This is the difference between \code{mat.size} and \code{mat.mask(org, mask, drop.dat = T)}.\cr
See also \link{mat.mask} and \bold{description}
}
\usage{
mat.size(org, mask)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{org}{
The original data \eqn{\mathbf{Z}=(z_{ij})_{p\times q}} that you want to mask.\cr
Can be numeric, complex or boolean.\cr
}
  \item{mask}{
The masking matrix \eqn{\mathbf{M} = (m_{ij})_{p \times q}} that you want to use as a mask.\cr
Necessarily boolean.\cr
Should have the same dimensions as the \code{org} matrix.
}
}
\details{
\code{mat.size} doesn't etch the matrix \code{org}, only sizes it.\cr
Please see figure below.\cr
\if{html}{\figure{mat.size.png}{options: width=600px}}
}
\value{
The returned value is always a matrix
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
##3.build the function
para <- function(X, Y){
  return((X^2 + Y^2) - (X^2 + Y^2) + 1)
}
##convert to an array, plot and calculate
arr <- func_2_arr(func = para, X = seq(-1.5, 1.5, by = 0.01), Y = seq(-1.5, 1.5, by = 0.01))
plot2D.arr(arr = arr)
##define the mask and then plot it as a matrix
M <- arr[,,2]^2 + arr[,,3]^2 <= 1
image(M, asp=1.0)
##mask matrix M1 and plot
M1 <- mat.mask(org = arr[,,1], mask = M, drop.dat = T)
image(M1, asp=1.0)
##size matrix M2 and plot
M2 <- mat.size(org = arr[,,1], mask = M)
image(M2, asp=1.0)
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
