\name{func_2_arr}
\alias{func_2_arr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a 2D function into an array
}
\description{
Converts a 2D function, \eqn{f:\mathbb{R}^2 \to \mathbb{R}^1}, of the form \eqn{f(x,y)} to a rank-3, 3-slice array \eqn{T_{m,n,p}}.\cr
Please see \bold{details}.
}
\usage{
func_2_arr(func, X, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{func}{
Describes the function \eqn{f(x,y)}.\cr
Has to be an \code{R function}
}
  \item{X}{
The X-vector over which the function should be evaluated.\cr
The X and Y vectors form the X-Y grid over which the function is evaluated.
}
  \item{Y}{
The Y-vector over which the function should be evaluated.\cr
The X and Y vectors form the X-Y grid over which the function is evaluated.
}
}
\details{
The returned array, \eqn{T_{m,n,p}}, will be a 3-slice array, i.e. \eqn{p \in \{1, 2, 3\}}.\cr
\eqn{T_{m,n,1}} is the data matrix for \eqn{f(x,y)}.\cr
\eqn{T_{m,n,2}} and \eqn{T_{m,n,3}} are the X-matrix and the Y-matrix respectively.\cr
For clearer understanding please see image.\cr
\if{html}{\figure{array.png}{options: width=600px}}
}
\value{
The returned value is an \code{R array}, \eqn{(T_{ijk})_{m \times n \times p}},.\cr
Here \eqn{m} and \eqn{n} are the length of the \code{Y vector} and \code{X vector} respectively while \eqn{p = 3}. Please see image above.\cr
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
\link{img_2_arr}, \link{plot2D.arr}
}
\examples{
###build the function
sines <- function(X, Y){
  w1 <- 2*X+3*Y
  w2 <- -3*X + Y
  return(sin(w1) + sin(w2))
}

###call the function
arr <- func_2_arr(func = sines, X = seq(-10,10, by=0.05), Y = seq(-7, 7, by = 0.05))
###plot the array
plot2D.arr(arr)
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
