\name{movavgf}
\alias{movavgf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a self-defined-functionalized (not just a table function ) moving average of the 2D dataset in question
}
\description{
The function returns a 2 column data frame with the first column named X.
The second column named Y contains the smoothed dataset
}
\usage{
movavgf(X, Y, bn, fn, f, val)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The X vector of the 2D dataset
}
  \item{Y}{
The Y vector of the 2D dataset
}
  \item{bn}{
The no of elements before the current element that is needed to be included in the averaging window
}
  \item{fn}{
The no of elements after the current element that is needed to be included in the averaging window
}
  \item{f}{
This argument is a function using which the moving window used for averaging of the X, Y dataset is created.\cr
The window is centered at wc = "val", (see argument "val").\cr
If "s" is the sampling interval of variable X, then the window abscissa vector "w"" is calculated as..\cr
w = seq(val - bn*s, val + fn*s) \cr
The window ordinate is calculated as f_seq = f(w).
}
  \item{val}{
The value around which the averaging window should be centered.\cr
For example, for a standard normal distribution val = 0, but for a gaussian with a mean of 4, val = 4.
}
}
\details{
Ensure before using that... \cr
1) f_seq does not contain Inf elements. \cr
2) f_seq does not contain NAs. \cr
3) f_seq does not contain NANs. \cr
All of the above will result in errors being thrown by the function.

}
\value{
The return type is always a 2 column data frame with the first column named X is same as the input argument X, while the second column Y is the smoothed dataset.
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
#build the dataset
set.seed(11)
X <- seq(-4, 4, by = 0.001)
Y <- X^2/16 + gauss(X, mu =  2, sig = 0.3, amp = 3, probability = F)
Y <- Y + rnorm(length(X), mean = 1, sd = 5)


##define the averaging function
lorentz <- function(v){
  y <- 1/((v - 3)^2 + 5)
  return(y)
}

##divide the plot into 4 co-ordinates
subplot(c(2,2))

##plot to check the averaging function
v <- seq(-10, 10, by=0.001)
plot(v, lorentz(v), type='l', col = 'red', main = 'averaging window', lwd=3)
#mark the position of the center value "val"
abline(v = 3)


##plot the dataset
plot(X, Y, col = rgb(0, 0.5, 0, 0.1), pch = 19, main = 'dataset vs averages')
#calculate the lorentz moving average
dat <- movavgf(X, Y, bn=300, fn=300, f=lorentz, val = 3)
#calculate the simple moving average (available in StatsChitran)
dat1 <- movavg(X, Y, bn=300, fn=300)
#plot the averages
lines(dat$X, dat$Y, col=rgb(1,0,0, 0.5), type='l', lwd = 3)
lines(dat1$X, dat1$Y, col=rgb(0,0,1, 0.25), type='l', lwd = 3)
#add legend
legend("topleft", legend = c("dataset", "lorentz MA", "MA"),
       col = c(rgb(0, 0.5, 0, 0.5), rgb(1,0,0, 0.5), rgb(0,0,1, 0.25)), lwd = c(0, 3, 3), pch = c(19, NA, NA),
        bty = "n")



##plot the averages only
plot(dat$X, dat$Y, col=rgb(1,0,0, 0.5), type='l', lwd = 3, main = 'moving averages')
lines(dat1$X, dat1$Y, col=rgb(0,0,1, 0.25), type='l', lwd = 3)
#add legend
legend("topleft", legend = c("lorentz MA", "MA"),
       col = c(rgb(1,0,0, 0.5), rgb(0,0,1, 0.25)), lwd = c( 3, 3), pch = c( NA, NA),
       bty = "n")


##plot the errors
vec <- dat1$Y -dat$Y
plot(X, vec, type = 'l', col=rgb(0,1,1), main = 'absolute diff bw averages')
#add legend
legend("bottomleft", legend = "lorentz MA - MA",
       col = rgb(0,1,1), lwd = 1, pch = NA,
       bty = "n")


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
