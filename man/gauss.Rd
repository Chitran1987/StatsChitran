\name{gauss}
\alias{gauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
returns a vector with the gaussian equation aginst the input vector X
}
\description{
The function takes in a vector "X"" against which it returns a vector with the gaussian with the other parameters "amp", "mu", "sig" and "probability"
}
\usage{
gauss(X, amp, sig, mu)
gauss(X, amp, sig, mu, probability=F)
gauss(X, sig, mu, probability=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The (numeric) vector which contain the \eqn{x_i} values against which you need the gaussian vector
}
  \item{probability}{
A single boolean value.\cr
Defaults to \code{FALSE}. \cr
Used to select between a normalized gaussian( \code{probability = T}) or a non-normalized statistical distribution( \code{probability = F})
}
  \item{amp}{
Amplitude, \eqn{A}, of the gaussian or the value of the gaussian at the mean. \cr
Can only be specified when \code{probability = F}.\cr
If specified when \code{probability = T}, then function throws an error
}
  \item{sig}{
Describes the standard deviation, \eqn{\sigma}, of the gaussian
}
  \item{mu}{
Describes the mean value, \eqn{\mu}, or the center of the gaussian.
}
}
\details{
The function returns the vector \eqn{y_i} against the inputs \eqn{x_i} as shown below.\cr
\deqn{\displaystyle y_i = Ae^{-(x_i - \mu)^2/(2\sigma^2)} \quad \text{with} \quad A\sqrt{2\pi} = 1/\sigma \,\,\, \text{normalization for density functions}}
}
\value{
The returned value is always a numeric vector.
}
\references{
\url{https://en.wikipedia.org/wiki/Gaussian_function}
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
X <- seq(-10,10,by=0.5)
y <- gauss(X, sig = 1.5, mu = 3, probability = T)
y1 <- gauss(X, amp=1, sig = 1.5, mu = 3, probability = F)
plot(X,y1, type = 'b')
lines(X,y, type='b', col='red')
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
