\name{peri}
\alias{peri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates the length of an arc defined by a function
}
\description{
1. This function returns the length of an arc which is defined by the function whose data points are present in the df dataframe.\cr
2. The dataframe should be a 2 column dataframe with column names should be defined as 'X' and 'Y', where 'X' denotes the x-axis of the function.\cr
3. The function also allows to calculate segments of the function by placing it within the xmin and xmax boundaries.
}
\usage{
peri(df, xmin, xmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Two column dataframe with columns defined a 'X' and 'Y' defining the arc whose length needs to be calculated
}
  \item{xmin}{
A single element numeric vector which defines the starting point of the section of the arc
}
  \item{xmax}{
A single element numeric vector which defines the ending point of the section of the arc
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a single element numeric vector
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
#define a semi-circle in the range (-1, 1) centered at the origin with a radius of 1
X <- seq(-2, 2, by = 0.01)
Y <- rep(NA, times = length(X))
for(i in 1:length(X)){
  if( (X[i] < -1) | (X[i] > 1)){
  Y[i] <- 0
  }else{
  Y[i] <- sqrt(1 - X[i]^2)
  }
}
dat <- data.frame(X, Y)
names(dat) <- c('X', 'Y')
plot(dat$X, dat$Y, type = 'l', col = 'red')
peri(df = dat, xmin = 0, xmax = 1)
peri(df = dat, xmin = -1, xmax = 1 )
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
