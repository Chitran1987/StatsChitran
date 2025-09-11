\name{noisy.gaussians}
\alias{noisy.gaussians}
\docType{data}
\title{
Two gaussians added to gaussian noise to create the noisy.gaussians dataset
}
\description{
Dataset created using two noisy gaussians
}
\usage{data("noisy.gaussians")}
\format{
  A data frame with 401 observations on the following 3 variables.
  \describe{
    \item{\code{X}}{The X-axis of the observations (1st column)}
    \item{\code{Y1}}{The Y-axis of the base observations (3rd column)}
    \item{\code{Y}}{The Y-axis of the complete noisy observations (2nd column)}
  }
}
\details{
Was built using the code shown in the \emph{Source} section
}
\source{
This dataset was generated using the following R code:

\preformatted{
X <- seq(-10, 10, by=0.05)
noise.data <- function(X){
  G1 <- gauss(X, amp = 3, mu = -3, sig = 2, probability = F)
  L1 <- gauss(X, amp = sqrt(3), mu=5, sig = 3, probability = F)
  Y1 <- G1 - L1
  set.seed(13)
  noise <- rnorm(n = length(X), mean = 0, sd = 0.5)
  Y <- Y1 + noise
  plot(X, Y, col = rgb(0.25,0,0,0.25))
  return(data.frame(X, Y, Y1))
}

noisy.gaussians <- noise.data(X)
}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(noisy.gaussians)
plot(noisy.gaussians)
}
\keyword{datasets}
