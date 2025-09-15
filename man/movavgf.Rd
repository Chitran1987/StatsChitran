\name{movavgf}
\alias{movavgf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a self-defined-kernel moving average of the 2D dataset in question
}
\description{
The function returns a 2 column data frame with the first column named X.
The second column named Y contains the smoothed dataset
}
\usage{
movavgf(X, Y, bn, fn, f, val, ord)
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
The no of elements before the element under investigation that shoul included in the averaging window
}
  \item{fn}{
The no of elements after the element under investigation that should be included in the averaging window
}
  \item{f}{
The kernel function, \eqn{f(x)}, used for the averaging.\cr
The window is centered at \code{wc = "val"}, (see argument \code{"val"}).\cr
If \eqn{\code{s} = (\sum_i\code{X[i+1] - X[i]})/N} is the sampling interval of variable \code{X}, then the averaging window \code{"w"} is shown below\cr
\code{w = seq(val - bn*s, val + fn*s)} \cr
The window ordinate is calculated as \code{f_seq = f(w)}.
}
  \item{val}{
The value around which the averaging window should be centered.\cr
For example, for a standard normal distribution \code{val = 0}, but for a gaussian with a mean of 4, \code{val = 4}.
}
    \item{ord}{
The no. of times you want to run the movavgf fucntion for smoothing.\cr
Defaults to \code{ord = 1}
}
}
\details{
The function basically lets the user choose a kernel smoother \eqn{f(x)} to smooth the \eqn{(X, Y)} as shown below.\cr
\deqn{\large\displaystyle Y_{res}[i] =  \bigg\{\displaystyle\sum_{j = -bn}^{fn}Y[i + j] f[\code{val} + j] \bigg\}/{ \sum_{j = -bn}^{fn} f[\code{val}+j]}     }


}
\value{
The return type is always a 2 column data frame with the first column named X is same as the input argument X, while the second column Y is the smoothed dataset.
}
\references{
\url{https://en.wikipedia.org/wiki/Kernel_density_estimation}
}
\author{
Chitran Ghosal
}
\note{
Ensure before using that... \cr
All elements of \eqn{f(\code{w})} are finite and defined. \cr
All inconsistencies like \eqn{f(\code{w}) = \pm\infty,\,\code{NA},\,\code{NAN},\, \ldots} will throw errors
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Call the relevant libraries
library(StatsChitran)

#Build the noisy data
X <- seq(-10, 10, by = 0.001)
Y <- gauss(X, sig = 1, probability = T)+rnorm(length(X), mean = 0, sd=0.1)

#Plot the noisy data
plot(X,Y)


#Build the kernel function (a lorentzian)
f <- function(vec){
  Y <- lorentz(X=vec, x_0 = 0, gamm = 1, probability = T)
  return(Y)
}

#Call two smoothers for ord=1 and ord=10
df1 <- movavgf(X, Y, bn=3, fn=3, f, val = 0, ord = 1)
df10 <- movavgf(X, Y, bn=3, fn=3, f, val = 0, ord = 100)

#Plot the results
plot(X,Y)
lines(df1$X, df1$Y, col='blue')
lines(df10$X, df10$Y, col='red')
legend('topright',
       legend = c('data-noisy std.gaussian', 'lor.smooth, ord = 1', 'lor.smooth, ord = 10'),
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
