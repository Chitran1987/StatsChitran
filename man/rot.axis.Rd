\name{rot.axis}
\alias{rot.axis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rotates a 2D dataset about an axis, perpendicular to the screen
}
\description{
Returns a dataframe after rotating an X-Y dataset abot an axis(perpendicular to the screen) passing through a point, mentioned in the function argument.\cr
}
\usage{
rot.axis(df, pt, deg)
rot.axis(df, deg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
The X-Y dataframe that needs to be rotated.\cr
Should be a 2 column dataframe.\cr
The first columns hsould be the X-axis.
}
  \item{pt}{
A two element numeric vector which denotes the point around which the dataset needs to be rotated.\cr
Default set to the ORIGIN.
}
  \item{deg}{
Angle in degrees denoting the amnount by which the dataset needs to be rotated.
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a 2 column dataframe
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
library(StatsChitran)

#build a linear dataset

X <- seq(-1, 1, by = 0.01)
Y <- X
dat <- data.frame(X, Y)


#plot the linear dataset

plot(dat$X, dat$Y, type = 'l')


#rotate the linear dataset

df <- rot.axis(df = dat, pt = c(0.5, 0.5), deg = 30)


#plot the rotated dataset

lines(df$X, df$Y, col = 'red')

#legend

legend("topleft", legend = c("original", "rotated"),col = c("black", "red"), lty = 1, lwd = 2)

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
