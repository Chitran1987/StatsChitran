\name{sample.distr}
\alias{sample.distr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Randomly sample from a user defined probability distribution
}
\description{
Returns a set of values of the random variable, \eqn{\bar{X}}, sampled from a user defined probability distribution \eqn{f(\bar{X}, \hat{\theta})}.\cr
Identical functionality to the \link{runif} and \link{rnorm} functions present in the \link{base} R package.
}
\usage{
sample.distr(n, func, f.par, xmin, xmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
The no. of values of \eqn{\bar{X}} that are expected to be returned.\cr
Will be the length of the return vector.
}
  \item{func}{
The function \eqn{f(\bar{X}, \hat{\theta})}.\cr
The argument passed to \code{func} must be a function.\cr
The first argument to the function that is passed to \code{func} must be the unknown \eqn{\bar{X}} and NOT the distribution parameters, \eqn{\hat{\theta}}.\cr
The subsequent arguments must encode the distribution parameters \eqn{\hat{\theta}} as the user desires.\cr
The ordering of the arguments for the function passed to \code{func} is \bold{important}.\cr
Please see \bold{examples}.
}
  \item{f.par}{
The specific values of the parameters, \eqn{\hat{\theta}}, that define the distribution, \eqn{f(\bar{X}, \hat{\theta})}, in this specific case.\cr
Must be a numeric atomic vector.\cr
It is \bold{important} that the length of \code{f.par} is one less than the no. of arguments of the function passed to \code{func}.\cr
This is because the first argument to \code{func} would be \eqn{\bar{X}}.\cr
Ordering of the values of the elements of \code{f.par} should adhere to the ordering of the parameters, \eqn{\hat{\theta}}, in \eqn{f(\bar{X}, \hat{\theta})}.
Please see \bold{examples}.
}
  \item{xmin, xmax}{
Numeric values within which the returned values of \eqn{\bar{X}} will lie
}
}
\details{
Uses rejection sampling to arrive at the results of \eqn{\bar{X}}.\cr
Thus results for \code{n > 10^7} could strain system resources, since rejection sampling is a monte-carlo method and needs overgeneration beyond \code{n} in the rejection space.\cr
The range of the function \eqn{f(\bar{X},\hat{\theta})} input to \code{func} will be normalized to \eqn{f(\bar{X}, \hat{\theta}) \in [0, 1]}.\cr
Hence it should be ensured that \eqn{f(\bar{X},\hat{\theta})} is devoid of badly behaved singularities within    \eqn{\code{xmin} \leq \bar{X} \leq \code{xmax}}.
}
\value{
The reurned value is a 2 element list \cr
  \itemize{
    \item \code{random_var}: The random variable \eqn{\bar{X}} which follows the distribution \eqn{f(\bar{X}, \hat{\theta})} returned as a R vector
    \item \code{likelihood}: The density value \eqn{f(\bar{X}, \hat{\theta})} for each corresponding value in \eqn{\bar{X}} as a R vector
  }
}
\references{
\url{https://en.wikipedia.org/wiki/Rejection_sampling}
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
####Define the probability distribution
sin.exp <- function(X, al, omeg){
  y <- exp(-al*abs(X))*(sin(omeg*X))^2
  return(y)
}

####define the distribution parameters
par_f <- c(2, 2)

####plot the bare distribution function
RV <- seq(-10, 10, by=0.01) #Random variable needed for the plotting
Y <- nrm(sin.exp(X=RV, al =par_f[1], omeg = par_f[2])) #The density/likelihood values against the given random variables
plot(RV, Y, type = 'l')

####retrieve random values of X out of the distribution and plot its histogram for different values of n

## for n = 10
L <- sample.distr(n = 10^1, func = sin.exp, f.par = par_f, xmin = -10, xmax = 10)
h <- hist(L$random_var, probability = T, breaks = seq(-10, 10, by = 0.25), border = NA, main = "### n = 10 ")
lines(RV, nrm(Y, min = 0, max = max(h$density)), col = 'black', lwd=2.5)

## for n = 100
L <- sample.distr(n = 10^2, func = sin.exp, f.par = par_f, xmin = -10, xmax = 10)
h <- hist(L$random_var, probability = T, breaks = seq(-10, 10, by = 0.25), border = NA, main = "### n = 100 ")
lines(RV, nrm(Y, min = 0, max = max(h$density)), col = 'black', lwd=2.5)

## for n = 1000
L <- sample.distr(n = 10^3, func = sin.exp, f.par = par_f, xmin = -10, xmax = 10)
h <- hist(L$random_var, probability = T, breaks = seq(-10, 10, by = 0.25), border = NA, main = "### n = 1000 ")
lines(RV, nrm(Y, min = 0, max = max(h$density)), col = 'black', lwd=2.5)

## for n = 10^4
L <- sample.distr(n = 10^4, func = sin.exp, f.par = par_f, xmin = -10, xmax = 10)
h <- hist(L$random_var, probability = T, breaks = seq(-10, 10, by = 0.25), border = NA, main = "### n = 10000 ")
lines(RV, nrm(Y, min = 0, max = max(h$density)), col = 'black', lwd=2.5)
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
