\name{exp.movavg.Rd}
\alias{exp.movavg.Rd}
\title{
Return a moving average with an exponentially decaying kernel of the 2D dataset in question
}
\description{
The function returns a 2 column data frame with the first column named X. The second column named Y contains the smoothed dataset.\cr
The kernel used for the averaging is an exponentially decaying kernel of the form \eqn{\Large e^{-\lambda x}}.
}
\usage{
exp.movavg.Rd(X, Y, bn, fn, dec, ord = 1)
exp.movavg.Rd(X, Y, bn, fn, dec)
exp.movavg.Rd(X, Y, bn, fn, dec, ord = 100)
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
  The no. of elements before the current element thatyou want included in the average
  }
  \item{fn}{
  The no. of elements after the current element(future element) thatyou want included in the average
  }
  \item{dec}{
  The decay constant, \eqn{-\lambda}, of the exponential kernel, \eqn{\Large e^{-\lambda x}}, used for the averaging.\cr
  \eqn{\code{dec} = -\lambda \leq 0} is expected, \code{else :} an error is thrown
  }
  \item{ord}{
  The no. of iterations of the smoothing function on the dataset\cr
  Defaults to one
  }
}

\details{
The function basically wraps around \code{kernel_movavgf.cpp} and forces a decaying exponential kernel smoother \eqn{f(x, \lambda) = e^{-\lambda x}} to smooth the \eqn{(X, Y)} as shown below.\cr
\deqn{\large\displaystyle Y_{res}[i] =  \bigg\{\displaystyle\sum_{j = -bn}^{fn}Y[i + j] e^{-\lambda [bn + j]} \bigg\}/\bigg\{ \sum_{j = -bn}^{fn} e^{-\lambda [bn+j]} \bigg\}     }

}
\value{
The return type is always a 2 column data frame with the first column named X is same as the input argument X, while the second column Y is the smoothed dataset.
}
\references{
\url{https://en.wikipedia.org/wiki/Moving_average}
}
\author{
Chitran Ghosal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{movavg}}, \code{\link{movavgf}}, \code{\link{gauss.avg}}
}
\examples{
##Call the relevant libraries
library(StatsChitran)

##Build a dataset and plot
X <- seq(-10, 10, by = 0.01)
Y <- gauss(X, mu = -2, sig = 2, probability = T) + gauss(X, mu = 3, sig = 2.7, probability = T) + rnorm(length(X), mean = 0, sd = 0.02)
plot(X, Y, col =rgb(0,0,0, 0.25))

##Call the function
df1 <- exp.movavg(X, Y, bn = 5, fn = 5, dec = -3)
df10 <- exp.movavg(X, Y, bn = 5, fn = 5, dec = -3, ord = 10)

##Plot the smoothed datasets
plot(X, Y, col =rgb(0,0,0, 0.25))
lines(df1$X, df1$Y, col='blue', lwd = 1)
lines(df10$X, df10$Y, col='red', lwd = 3)
legend('topright',
       legend = c('data-noisy std.gaussian', 'exp.smooth, ord = 1', 'exp.smooth, ord = 10'),
       col = c('black', 'blue', 'red'),
       pch =c(1, NA, NA),
       lty = c(NA, 1, 1)
         )
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
