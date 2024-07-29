\name{padding}
\alias{padding}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aperiodically increases the span of a given X-Y dataset.\cr
See also \link[StatsChitran]{periodify}
}
\description{
Adds an integer no. of periods of the X-Y dataset to itself. \cr
The no. of integer periods by which the X-Y dataset is increased in the forward and backward direction is specified by "fwd" and "bwd" arguments. \cr
Hence, if L is the length of the X-Y dataset, the new dataset has a length of "L*(fwd + bwd + 1)". \cr
The returned dataset is always a dataframe
}
\usage{
df <- padding(X, Y, fwd, bwd)
df <- padding(X, Y, fwd, bwd, pad.type = 'default')
df <- padding(X, Y, fwd, bwd, pad.type = 'zero')
df <- padding(X, Y, fwd, bwd, fval, bval, pad.type = 'custom')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
An atomic numeric vector X signifying the X-axis of the dataset
}
  \item{Y}{
An atomic numeric vector Y signifying the X-axis of the dataset
}
  \item{fwd}{
A single whole number or integer specifying the no. of periods by which to increase the span of the dataset in the forward direction
}
  \item{bwd}{
A single whole number or integer specifying the no. of periods by which to increase the span of the dataset in the backward direction
}
  \item{pad.type}{
1. A single character which can take values of either, 'default', 'zero' or 'custom'.\cr
2. The argument is set to 'default' by default. \cr
3. When set to 'custom', the arguments *fval* and *bval* specifying forward and backward values are necessary.\cr
}
  \item{fval}{
Defines the constant Y value of the function for the range of X greater than max(X)
}
  \item{bval}{
Defines the constant Y value of the function for the range of X less than min(X)
}
}
\details{
1. When 'pad.type' is set to 'default', 'fval' is set to 'Y[X = max(X)]' and 'bval' is set to 'Y[X = min(X)]'.\cr
2. When 'pad.type' is set to 'zero', then 'fval = bval = 0' is set for zero padding. \cr
3. When 'pad.type' is set to 'custom', then fval and bval are needed as user inputs.
}
\value{
1. The returned value is always a 2 column data frame. \cr
2. The columns are named as 'X' and 'Y'.
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
X <- seq(0, 3, by = 0.01)
Y <- X^2
subplot(c(2,2))
df1 <- data.frame(X, Y)
plot(df1$X, df1$Y, type = 'l', main = 'without padding')
df2 <- padding(X, Y, fwd = 2, bwd = 3)
plot(df2$X, df2$Y, type = 'l', main = 'default')
df3 <- padding(X, Y, fwd = 1, bwd = 4, pad.type = 'zero')
plot(df3$X, df3$Y, type = 'l', main = 'zero')
df4 <- padding(X, Y, fwd = 0, bwd = 3, pad.type = 'custom', fval = NA, bval = 5 )
plot(df4$X, df4$Y, type = 'l', main = 'custom')
subplot(c(1,1))
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
