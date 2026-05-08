\name{avg.curves}
\alias{avg.curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average a List of Curves After Interpolation
}
\description{
Interpolates a list of two-column data frames onto a common \code{X} grid,
optionally normalizes the curves, and returns their average curve.
}
\usage{
avg.curves(L, X, pl = TRUE, norm.gr = TRUE, interp = "spline", lin.wd = 3, pl.col = "gray")
avg.curves(L, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{
A list of two-column data frames. Each data frame should contain the
\code{X} and \code{Y} values of one curve. The column names are not important,
as they are internally renamed to \code{X} and \code{Y}.
}

  \item{X}{
A numeric vector specifying the common \code{X}-axis grid onto which all curves
are interpolated before averaging.
}

  \item{pl}{
Logical scalar. If \code{TRUE}, the original group of curves is plotted and the
averaged curve is overlaid in black. Default is \code{TRUE}.
}

  \item{norm.gr}{
Logical scalar. If \code{TRUE}, each interpolated curve is normalized using
\code{nrm()} before averaging. Default is \code{TRUE}.
}

  \item{interp}{
Character scalar specifying the interpolation method. Allowed values are
\code{"spline"} and \code{"linear"}. If \code{"spline"}, interpolation is
performed using \code{\link[stats]{spline}}. If \code{"linear"}, interpolation
is performed using \code{\link[stats]{approx}}. Default is \code{"spline"}.
}

  \item{lin.wd}{
Numeric value controlling the line width used when plotting the original group
of curves through \code{plot.grp()}. Default is \code{3}.
}

  \item{pl.col}{
Color used for plotting the original curves. Default is \code{"gray"}.
}
}
\details{
The function is useful when several curves are sampled on different
\code{x}-axis grids and need to be compared or averaged on a common grid.

Each element of \code{L} must be a two-column data frame. The first column is
treated as the \code{X}-axis and the second column as the corresponding
\code{Y}-axis values. The function first interpolates every curve onto the
user-supplied grid \code{X}. If \code{norm.gr = TRUE}, the interpolated
\code{Y} values are normalized using \code{nrm()}. The final output is obtained
by averaging the interpolated \code{Y} values point by point.

If \code{pl = TRUE}, the original curves are plotted using \code{plot.grp()},
and the averaged curve is added to the same plot as a black line.
}
\value{
A two-column data frame with the following columns:

\item{X}{
The common \code{X}-axis grid supplied by the user.
}

\item{Y}{
The averaged \code{Y}-axis values calculated after interpolation and optional
normalization.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
This function depends on the auxiliary functions \code{nrm()} and
\code{plot.grp()}. These functions must be available in the working environment
or inside the same package.

The input curves do not need to be manually sorted by their \code{X}-values,
however, duplicated \code{x}-values within an input data frame may affect the
interpolation result and should be avoided or preprocessed if necessary.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Call the library
library(StatsChitran)

#build the data
L <- vector(mode = 'list', length = 6)
v.abs <- runif(n=6, min = 1, max = 2)
X <- seq(-10, 10, by=0.1)
for(i in 1:length(L)){
  df <- data.frame(X, Y=gauss(X, sig = v.abs[i], probability = T))
  L[[i]] <- df
}

#plot the data
plot.grp(L, norm.gr = F)

#call the group_avg result
df <- avg.curves(L, X = seq(-10, 10, by=0.01), norm.gr = T, pl.col = rgb(0.1,0.5,0.5,0.1))
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
