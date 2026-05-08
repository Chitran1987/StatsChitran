\name{interp.curves}
\alias{interp.curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate a List of Two-Column Curves
}
\description{
Interpolates a list of two-column data frames onto a common numeric
\code{X} grid using either spline interpolation or linear interpolation.
}
\usage{
interp.curves(L, X, interp = "spline")
interp.curves(L, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{L}{
A list of two-column data frames. Each data frame should contain the
original x- and y-values of one curve. The first column is treated as
the x-coordinate and the second column as the y-coordinate.
}

  \item{X}{
A numeric vector specifying the new x-values onto which each curve
should be interpolated.
}

  \item{interp}{
A character string specifying the interpolation method. Allowed values
are \code{"spline"} and \code{"linear"}. The default is \code{"spline"}.
}
}
\details{
The function takes a list of two-column data frames and interpolates
each curve onto the same x-axis defined by \code{X}. This is useful when
multiple curves have different original x-values but need to be compared,
averaged, plotted, or processed on a common grid.

If \code{interp = "spline"}, interpolation is performed using
\code{\link[stats]{spline}}. If \code{interp = "linear"}, interpolation
is performed using \code{\link[stats]{approx}}.

Each output data frame has two columns named \code{X} and \code{Y}.
}
\value{
A list of data frames with the same length as \code{L}. Each data frame
contains the interpolated curve evaluated at the supplied \code{X} values.

Each returned data frame contains:
\item{X}{The common x-values supplied by the user.}
\item{Y}{The interpolated y-values.}
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
# Create two example curves
df1 <- data.frame(x = 1:5, y = c(1, 2, 1.5, 3, 2.5))
df2 <- data.frame(x = c(1, 2, 4, 5), y = c(2, 1, 3, 2))

# Store them in a list
L <- list(df1, df2)

# Define a common X grid
X <- seq(1, 5, length.out = 50)

# Spline interpolation
L.spline <- interp.curves(L, X, interp = "spline")

# Linear interpolation
L.linear <- interp.curves(L, X, interp = "linear")

# Plot original and interpolated first curve
plot(df1[, 1], df1[, 2], pch = 16)
lines(L.spline[[1]]$X, L.spline[[1]]$Y)
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
