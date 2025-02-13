\name{img_2_arr}
\alias{img_2_arr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a complete array object for a 2D plot
}
\description{
Takes an image along with x and y values and creates an R-array which is effectively a rank-3 Tensor or a 3 slice tensor \eqn{T_{m,n,p}}.
}
\usage{
img_2_arr(source.png, x.lim, y.lim, pl = T, bgrnd = 1)
img_2_arr(source.png, x.lim, y.lim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source.png}{
Path to the PNG file
}
  \item{x.lim}{
The x-limits of the image.\cr
Must be a two element numeric vector.
}
  \item{y.lim}{
The y-limits of the image.\cr
Must be a two element numeric vector.
}
  \item{pl}{
Boolean bit, set to \code{TRUE} by default.\cr
Plots the grayscale matrix if set to \code{TRUE}
}
  \item{bgrnd}{
Numeric scalar, set to 1 by default.\cr
Value should lie between 0 and 1.\cr
Determines the grayscale value of the background.\cr
Values of 1 and 0 determine white and black backgrounds respectively.
}
}
\details{
For returning only the grayscale matrix without any array format explaining the independent co-ordinates, please see \link{img_2_data}.\cr
Since \eqn{T_{m,n,2}} is the X-matrix, hence every row of the matrix will be the same vector, i.e. \eqn{T_{m,n,2} = T_{k,n,2}\,\,\forall m,n,k}.\cr
Since \eqn{T_{m,n,3}} is the Y-matrix, hence every column of the matrix will be the same vector, i.e. \eqn{T_{m,n,3} = T_{m,k,3}\,\,\forall m,n,k}.\cr
}
\value{
The returned value is a rank-3 tensor, \eqn{T_{m, n, p}}, in the form of an R-array.\cr
In \eqn{T_{m,n,p}}, it should be noted that \eqn{p \in \{1, 2, 3\}}.\cr
For \eqn{p = 1}, \eqn{T_{m,n,1}} is the grayscale matrix.\cr
For \eqn{p = 2}, \eqn{T_{m,n,2}} is the X-matrix which returns the x-value at any point on the image.\cr
For \eqn{p = 3}, \eqn{T_{m,n,3}} is the Y-matrix which returns the y-value at any point on the image.\cr
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
tens <- img_2_tens(source.png = 'STM.png', x.lim = c(0, 6.115*10^-9), y.lim = c(0, 6.115*10^-9))
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
