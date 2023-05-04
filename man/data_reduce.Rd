\name{data_reduce}
\alias{data_reduce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for reducing the no. of data points in an X-Y dataset
}
\description{
The function\cr
1. Uniformly selects points between the dataset and for the select indexes returns the X and Y values in a dataframe
2. The no. of selected points is reduced by an integer factor "fract"
}
\usage{
data_reduce(X, Y, fract)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
X vector of the original dataset
}
 \item{Y}{
Y vector of the original dataset
}
 \item{fract}{
The factor by which the function needs to reduce the size of the data set.\cr
This necessarily needs to be an integer.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a 2-column dataframe with column names 'X' and 'Y'
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
If the no. of points in the new dataset are one or less, then the function throws an error.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
t <- seq(0, 2*pi, by=0.0001)
Y <- Sin(t)
df <- data_reduce(t,Y, fract = 1000)
par(mfrow = c(2,1))
plot(t,Y, main = 'original data')
plot(df$X, df$Y, main = 'reduced data')

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
