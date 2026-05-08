\name{plot.grp}
\alias{plot.grp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a Group of Two-Column Data Frames
}
\description{
Plots multiple two-column data frames stored in a list. Each data frame is
interpreted as an \eqn{X}-\eqn{Y} dataset and plotted on the same graph.
Optionally, the \eqn{Y}-values of each dataset can be normalized before plotting.
}
\usage{
plot.grp(L, pl.typ = "l", lin.wid = 1, norm.gr = TRUE, pl.col, x.lim, y.lim)
plot.grp(L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{
A list of data frames. Each element of the list must be a data frame with
exactly two columns. The first column is treated as the \eqn{X}-axis and the
second column as the \eqn{Y}-axis.
}
  \item{pl.typ}{
A single character specifying the plot type passed to \code{\link{plot}} and
\code{\link{points}}. The default is \code{"l"}, which produces line plots.
Other common options include \code{"p"} for points and \code{"b"} for both
points and lines.
}
  \item{lin.wid}{
Numeric value specifying the line width passed to the \code{lwd} argument of
\code{\link{plot}} and \code{\link{points}}. The default is \code{1}.
}
  \item{norm.gr}{
Logical value indicating whether the \eqn{Y}-values of each data frame should
be normalized before plotting. The default is \code{TRUE}. Normalization is
performed using the function \code{nrm}.
}
  \item{pl.col}{
Color used for plotting the datasets. This argument is passed to the
\code{col} argument of \code{\link{plot}} and \code{\link{points}}.\cr
Defaults to \code{col = 'black'}
}
  \item{x.lim}{
Optional numeric vector of length two specifying the limits of the \eqn{X}-axis.
If supplied, the data are also filtered to this \eqn{X}-range before plotting.
If missing, the limits are determined automatically from all datasets.
}
  \item{y.lim}{
Optional numeric vector of length two specifying the limits of the \eqn{Y}-axis.
If supplied, the data are also filtered to this \eqn{Y}-range before plotting.
If missing, the limits are determined automatically from all datasets.
}
}
\details{
The function is designed for quickly plotting several related \eqn{x}-\eqn{y}
datasets stored as a list of two-column data frames.

Each data frame in \code{L} is internally renamed to have columns \code{X} and
\code{Y}. If \code{norm.gr = TRUE}, the \code{Y} column of each data frame is
normalized using \code{nrm}. Therefore, the function \code{nrm} must be
available in the package or in the user's R environment.

The first data frame is plotted using \code{\link{plot}}, and all subsequent
data frames are added using \code{\link{points}} with the same plot type,
line width, and color.

}
\value{
This function is used for its side effect of producing a plot. It does not
return a meaningful object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
All elements of \code{L} must be two-column data frames. The function stops with
an error if \code{L} is not a list, if any element is not a two-column data
frame, or if the plotting and limit arguments are not supplied in the expected
format.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot}},
\code{\link{points}}
}
\examples{
## Create example data
df1 <- data.frame(x = seq(0, 10, length.out = 100),
                  y = sin(seq(0, 10, length.out = 100)))

df2 <- data.frame(x = seq(0, 10, length.out = 100),
                  y = cos(seq(0, 10, length.out = 100)))

L <- list(df1, df2)

## Plot the group without normalization
plot.grp(L, norm.gr = FALSE, pl.col = "blue")

## Plot only a selected x-range
plot.grp(L, norm.gr = FALSE, pl.col = "red", x.lim = c(2, 8))
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
