\name{order.curves}
\alias{order.curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sort a List of Curve Data Frames by X Values
}
\description{
Sorts each two-column numeric data frame in a list according to the values in its first column.
The columns of each data frame are renamed to \code{X} and \code{Y}.
}
\usage{
order.curves(L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{
A list of two-column data frames. Both columns of each data frame must be numeric.
The first column is treated as the \code{X} coordinate and the second column as the
\code{Y} coordinate.
}
}
\details{
This function is useful for preparing curve-like data before interpolation, averaging,
plotting, or further numerical processing. Each element of \code{L} must be a data frame
with exactly two numeric columns.

For each data frame, the function renames the columns to \code{X} and \code{Y}, then
sorts the rows in ascending order of \code{X}.
}
\value{
A list of data frames with the same length as \code{L}. Each returned data frame has
two numeric columns named \code{X} and \code{Y}, ordered by increasing \code{X}.
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
df1 <- data.frame(a = c(3, 1, 2), b = c(30, 10, 20))
df2 <- data.frame(a = c(5, 4, 6), b = c(50, 40, 60))

L <- list(curve1 = df1, curve2 = df2)

order.curves(L)
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
