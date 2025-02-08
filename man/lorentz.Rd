\name{lorentz}
\alias{lorentz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
returns a vector with the lorentz equation/Cauchy distribution aginst the input vector X
}
\description{
The function takes in a vector "X" and returns a vector with the gaussian with the other parameters "amp" denoting \eqn{A}, "x_0" denoting the mean \eqn{x_0} , "gamm" denoting the HWHM or \eqn{\gamma} and "probability" to toggle between statistical and probability distributions.\cr
See \bold{Details}
}
\usage{
lorentz(X, amp, x_0, gamm, probability = F)
lorentz(X, x_0, gamm, probability = T)
lorentz(X, amp, x_0, gamm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The (numeric) vector which contain the \eqn{x_i} values against which you need the lorentzian vector
}
  \item{amp}{
Amplitude, \eqn{A}, of the lorentzian. \cr
Can only be specified when \code{probability = F}.\cr
If specified when \code{probability = T}, then function throws an error
}
  \item{gamm}{
The HWHM, \eqn{\gamma}, of the lorentzian.\cr
Should be noted that the second moment (w.r.t mean), i.e. \eqn{M_2 = \mathbb{E}[(X - x_0)^2] \to \infty}\cr
Thus \eqn{\sigma \to \infty} and has no meaning, unlike a gaussian. Please see \link{gauss}
}
  \item{x_0}{
Describes the mean value, \eqn{x_0}, or the center of the lorentzian.
}
  \item{probability}{
A single boolean value.\cr
Defaults to \code{FALSE}. \cr
Used to select between a normalized ( \code{probability = T}) or a non-normalized distribution( \code{probability = F})
}
}
\details{
The function returns the vector \eqn{y_i} against the inputs \eqn{x_i} as shown below.\cr
\deqn{\displaystyle y_i = \frac{1}{\pi\gamma}\large{\frac{A}{1 + (\frac{x_i - x_0}{\gamma})^2} \large}}
For normalized probability curves, \eqn{A = 1}, while \eqn{x_0} and \eqn{\gamma} are the center/mean of the distribution and the HWHM respectively
}
\value{
The returned value is always a numeric vector.
}
\references{
\url{https://en.wikipedia.org/wiki/Cauchy_distribution}
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
#####Build the variable X
X <- seq(-10, 10, by = 0.01)

####Build four lorentzians with increasing gamm and decreasing x_0
Y1 <- lorentz(X, x_0 = 1.5, gamm = 1.0, probability = T)
Y2 <- lorentz(X, x_0 = 1.0, gamm = 2.0, probability = T)
Y3 <- lorentz(X, x_0 = 0.5, gamm = 3.0, probability = T)
Y4 <- lorentz(X, x_0 = 0.0, gamm = 4.0, probability = T)

####Plot the lorentzians
plot(X, Y1, type = 'l')
lines(X, Y2, type = 'l', col='red')
lines(X, Y3, type = 'l', col='green')
lines(X, Y4, type = 'l', col='blue')

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
