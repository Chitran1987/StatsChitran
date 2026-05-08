\name{clip.curves}
\alias{clip.curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clip a List of Two-Column Data Frames by X Range
}
\description{
Clips each two-column data frame in a list according to user-defined bounds on the first column.
The function is intended for curve-like data where the first column represents the x-axis and the
second column represents the y-axis.
}
\usage{
clip.curves(L, bounds, two.sided = c(TRUE, TRUE))
clip.curves(L, bounds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L}{
A list of two-column data frames. Each data frame should contain x-values in the first column
and y-values in the second column. The x-values must be sorted in ascending order.
}

  \item{bounds}{
A numeric vector of length 2 specifying the lower and upper x-limits used for clipping.
For example, \code{bounds = c(-5, 5)}.
}

  \item{two.sided}{
A logical vector of length 2 specifying which side of the data should be clipped.
The first element controls clipping by the lower bound and the second element controls clipping
by the upper bound. The default is \code{c(TRUE, TRUE)}, meaning both sides are clipped.
}
}
\details{
The function first checks that \code{L} is a list and that each element of \code{L} is a
two-column data frame. The columns of each data frame are internally renamed to \code{X}
and \code{Y}.

The argument \code{two.sided} controls whether clipping is applied to both sides or only one side.
For example, \code{two.sided = c(TRUE, FALSE)} clips only the lower side, keeping rows for which
\code{X >= bounds[1]}. Similarly, \code{two.sided = c(FALSE, TRUE)} clips only the upper side,
keeping rows for which \code{X <= bounds[2]}.

If \code{two.sided = c(FALSE, FALSE)}, the original list is returned unchanged.
}
\value{
A list of two-column data frames with the same length as \code{L}. Each data frame is clipped
according to the specified x-range.
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
df1 <- data.frame(x = seq(-10, 10, by = 1),
                  y = seq(-10, 10, by = 1)^2)

df2 <- data.frame(x = seq(-8, 8, by = 1),
                  y = sin(seq(-8, 8, by = 1)))

L <- list(df1, df2)

## Clip both sides
L_clip <- clip.curves(L, bounds = c(-5, 5))

## Clip only the lower side
L_lower <- clip.curves(L, bounds = c(-5, 5), two.sided = c(TRUE, FALSE))

## Clip only the upper side
L_upper <- clip.curves(L, bounds = c(-5, 5), two.sided = c(FALSE, TRUE))

## Return unchanged
L_same <- clip.curves(L, bounds = c(-5, 5), two.sided = c(FALSE, FALSE))
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
