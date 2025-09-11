\name{mat_2_arr}
\alias{mat_2_arr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a rank-3 array from a matrix
}
\description{
Takes a matrix, \eqn{(z_{ij})_{m \times n}}, pertaining to an image along with limits of the \eqn{x} and the \eqn{y} axis values.\cr
Returns a rank-3 array, \eqn{T_{m,n,p}}, which is 3-slice, i.e. \eqn{p \in \{1, 2, 3\}}.\cr
\eqn{T_{m,n,1}} is the matrix, \eqn{(z_{ij})_{m \times n}}, while \eqn{T_{m,n,2}} and \eqn{T_{m,n,3}} are the X-natrix and the Y-matrix.\cr
Please see \bold{details}
}
\usage{
mat_2_arr(mat, x.lim, y.lim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
The input matrix, \eqn{(z_{ij})_{m \times n}}
}
  \item{x.lim}{
The limits of the \eqn{x}-values of the \eqn{X \times Y} grid of the matrix.\cr
\code{x.lim = c(xmin, xmax)}
}
  \item{y.lim}{
The limits of the \eqn{y}-values of the \eqn{X \times Y} grid of the matrix.\cr
\code{y.lim = c(ymin, ymax)}
}
}
\details{
For clearer picture of the structure of the rank-3 array \eqn{T_{m,n,p}}, see image below.\cr
\if{html}{\figure{array.png}{options: width=600px}}
}
\value{
Returns a rank-3 array, \eqn{T_{m,n,p}}, which is 3-slice, i.e. \eqn{p \in \{1, 2, 3\}}
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
###Build the matrix
sines <- function(X, Y){
  l1 <- 2*X +3*Y
  l2 <- 4*X + 2*Y
  return(sin(l1) + sin(l2))
}
M <- function_2D(X = seq(-10, 10, by = 0.01), Y = seq(-5, 5, by = 0.01), func = sines)


#Call the mat_2_arr function
arr <- mat_2_arr(mat = M, x.lim = c(-10, 10), y.lim = c(-5, 5))

#Plot the built array to verify
plot2D.arr(arr = arr)

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
