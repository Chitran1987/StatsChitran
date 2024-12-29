\name{function_2D}
\alias{function_2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a 2D function and returns the matrix
}
\description{
Create a matrix using vaector x, y and a function f(x,y) as inputs.\cr
The returned matrix has elements positioned as would be expected an a euclidean co-ordinate grid.\cr
This does NOT adhere to the positions built out of the \code{outer(X, Y, FUN)} function.
}
\usage{
function_2D(X, Y, func)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{X}{
Describes the (numeric) vector in the X direction
}
  \item{Y}{
Describes the (numeric) vector in the Y direction
}
  \item{func}{
Describes the function used to calculate the Z return matrix
}

}


\details{
X and Y need to be numeric vectors.\cr
func needs to be a function.\cr
for plotting, please see *plot2D.mat()* and *plot2D()*
}

\value{
The returned value is a matrix. \cr
The no. of columns of the matrix is equal to the no. of X-values.\cr
The no. of rows of the matrix are equal to the no. of Y-values.\cr
The first element f(X[1], Y[1]) is located at the matrix position Z[length(Y), 1].\cr
The element f(X[length(X)], Y[1]) is located at the matrix position Z[length(Y), length(X)].\cr
The element f(X[1], Y[length(Y)]) is located at the matrix position Z[1, 1].\cr
The element f(X[length(X)], Y[length(Y)]) is located at the matrix position Z[1, length(X)].
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
#define the x-axis
X <- seq(0, 10, by = 0.01)
Y <- seq(0, 16, by = 0.01)

#define the function
patt <- function(x, y){
  return(exp(-0.2*x - 0.1*y)*sin(3*x)*cos(4*y))
}

#build the Z matrix
Z <- function_2D(X, Y, patt)

#plot the matrix
plot2D.mat(X, Y, Z)
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
