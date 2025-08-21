\name{arr.mask}
\alias{arr.mask}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform array data upon masking
}
\description{
Transform a rank-3 array called \code{arr}, represented by, \eqn{\mathbb{T} = T_{m,n,p}}, using a matrix called \code{mask}, represented by \eqn{\mathbb{M}=(m_{ij})_{m \times n}}.\cr
Masks each matrix \eqn{T_{m,n,p}\,\forall\, p \in \{1, 2, 3, \dots , N\}} using the same \code{mask} \eqn{\mathbb{M}=(m_{ij})_{m \times n}}.\cr
Any matrix slice \eqn{T_{m,n,k}} and the \code{mask} have the same dimensionality, i.e. \eqn{m \times n}. See \link{mat.mask} for details.\cr
}
\usage{
arr.mask(arr, mask, drop.dat = F)
arr.mask(arr, mask, drop.dat = T)
arr.mask(arr, mask)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arr}{
The rank-3 array, \eqn{T_{m,n,p}} that you want to mask.\cr
}
  \item{mask}{
The masking matrix, \eqn{\mathbb{M}=(m_{ij})_{m \times n}}, that you need to use as a mask.\cr
Should be a boolean matrix.\cr
Should have the same dimesions, \eqn{m \times n}, as any slice \eqn{T_{m,n,k}} of the array \code{arr}.
}
  \item{drop.dat}{
Boolean bit, defaults to \code{FALSE}.\cr
When set to \code{FALSE}, the output array will have the same dimesnionality as \code{arr}, i.e. \code{m \eqn{\times} n \eqn{\times} p}\cr
When set to \code{TRUE}, the output array dimesnionality might be different. Please see \link{mat.mask} for more details.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a rank-3 array.\cr
Whether the dimensions of the output and \code{arr} are same or not, depends on \code{drop.dat}.\cr
Please see \link{mat.mask} for more details.
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
###plot the original image and convert it to an array
dat <- system.file("extdata", "STM.png", package = "StatsChitran")
tens <- img_2_arr(source.png = dat, x.lim = c(0, 6.115*10^-9), y.lim = c(0, 6.115*10^-9))
plot2D.arr(arr = tens)

###create the mask
mask_x <- tens[,,2] >= 2*10^-9 & tens[,,2] <=4*10^-9
mask_y <- tens[,,3] >= 2.5*10^-9 & tens[,,3] <= 5.5*10^-9
mask <- mask_x & mask_y

####points not dropped
arr.res <- arr.mask(arr = tens, mask = mask, drop.dat = F)
plot2D.arr(arr.res)

####points dropped
arr.res <- arr.mask(arr = tens, mask = mask, drop.dat = T)
plot2D.arr(arr.res)
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
