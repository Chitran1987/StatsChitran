\name{plot2D.arr}
\alias{plot2D.arr}
\title{
Plots a 3-slice rank-3 array
}
\description{
Plots a rank-3 array \eqn{T_{m, n, p}} which has to be 3-slice, hence \eqn{p \in \{1, 2, 3\}}.\cr
The actual image matrix/data is \eqn{T_{m, n, 1}}.\cr
The X-matrix is \eqn{T_{m, n, 2}}.\cr
The Y-matrix is \eqn{T_{m.n.3}}.\cr
Please see \bold{details} for clearer understanding of array structure.\cr
}
\usage{
plot2D.arr(arr, gray.dens = 10^5, x.lab = NULL, y.lab = NULL)
plot2D.arr(arr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arr}{
The array,\eqn{T_{m,n,p}}
}
  \item{gray.dens}{
The no. of colours in the gray colour scale between 0 and 1 that the matrix plot of \eqn{T_{m,n,1}} would be sensitive to.\cr
Please see \link{gray} for more details.\cr
Has to be a numeric scalar.
}
  \item{x.lab}{
The x-axis label. \cr
Must be a character scalar.
}
  \item{y.lab}{
The y-axis label.\cr
Must be a character scalar.
}
}
\details{
The image below describes the 3-slice rank-3 array \eqn{T_{m,n,p}} that is needed.\cr
\if{html}{\figure{array.png}{options: width=600px}}\cr
}
\value{
The function returns nothing
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
\link{img_2_arr}
}
\examples{
dat <- system.file("extdata", "STM.png", package = "StatsChitran")
tens <- img_2_arr(source.png = dat, x.lim = c(0, 6.115*10^-9), y.lim = c(0, 6.115*10^-9))
plot2D.arr(arr = tens)
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
