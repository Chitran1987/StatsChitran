\name{plot2D.zoom}
\alias{plot2D.zoom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Zooms into a section of an array object and plots it
}
\description{
Takes in an array object, \eqn{T_{m,n,p}}.\cr
Also requires centre of the zoom area, given by \code{center} and the expected zoom dimensions \code{Del_X} and \code{Del_Y}.\cr
Then, it returns the subsetted array in accordance to the zoom requirements set by \code{center}, \code{Del_X} and \code{Del_Y}
}
\usage{
plot2D.zoom(arr, center, Del_X, Del_Y, pl = T)
plot2D.zoom(arr, center, Del_X, Del_Y)
plot2D.zoom(arr, center, Del_X, Del_Y, pl = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arr}{
The array object, \eqn{T_{m,n,p}}.\cr
Has to be a rank-3 array.\cr
\eqn{p \in \{1, 2, 3\}}, since \eqn{T_{m,n,p}} has to be 3-slice.\cr
For better understanding of structure of array, check \code{\link{img_2_arr}}.
}
  \item{center}{
Two element vector denoting the \eqn{(X, Y)} values of the center of the zoom-area
}
  \item{Del_X}{
The length of the zoomed image along the X-axis
}
  \item{Del_Y}{
The length of the zoomed image along the Y-axis
}
  \item{pl}{
Plots the new array if \code{pl == T}.\cr
Set to \code{TRUE} by default
}
}
\details{
See the image below.\cr
\if{html}{\figure{zoom_array.png}{options: width=600px}}
}
\value{
The returned value is necessarily an array of the same structure as \code{arr}
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
\code{\link{img_2_arr}}
}
\examples{
library(StatsChitran)
dat <- system.file("extdata", "STM.png", package = "StatsChitran")
arr1 <- img_2_arr(source.png = dat, x.lim = c(0, 19), y.lim = c(0,19)) #The image array
arr.zoom <- plot2D.zoom(arr = arr1, center = c(16, 12), Del_X = 6, Del_Y = 6) #The zoomed in array
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
