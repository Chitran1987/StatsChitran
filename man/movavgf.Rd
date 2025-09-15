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
%% ~put references to the literature/web site here ~
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
#call the relevant libraries
library(StatsChitran)

#Call the dataset
data("noisy.gaussians")
df <- noisy.gaussians


##define the kernel function (lorentzian)
lor.avg <- function(v){
  return(lorentz(X = v, x_0 = 0, gamm = 1, probability = T))
}

##divide the plot into two
subplot(c(1,2))

##plot the kernel function
Y_kernel <- lor.avg(df$X)
plot(df$X, Y_kernel, type = 'l', col = 'red', main = 'The lorentzian kernel')
abline(v = 0)


##Perform kernel smoothening of ord = 1, 10, 100
df1 <- movavgf(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 1)
df2 <- movavgf(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 10)
df3 <- movavgf(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 100)

##Plot the smoothed dataframes against the main data
plot(df$X, df$Y, main = 'Data Processing')
lines(df1$X, df1$Y, col = 'green')
lines(df2$X, df2$Y, col = 'red')
lines(df3$X, df3$Y, col = 'blue')
legend(x = 'topright', legend = c('data', 'smooth. ord = 1', 'smooth. ord = 10', 'smooth. ord = 100'), fill = c('black', 'green', 'red', 'blue'))
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
