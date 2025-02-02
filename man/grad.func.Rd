\name{grad.func}
\alias{grad.func}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the gradient of a function at a specific point in its domain.
}
\description{
Returns the vector gradient, \eqn{\nabla f}, of a function, \eqn{f(\vec{r})}, at aspecific point, \eqn{\vec{r} = \vec{r}_o}, in its domain, once the tolerance, \eqn{\epsilon}, for the calculation of the gradient has been provided.\cr
Mathematically speaking, the function returns the expression shown below.\cr
\deqn{\nabla f(\vec{r})\bigg|_{\vec{r} = \vec{r}_o}}
}
\usage{
grad.func(f, pt, epsilon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
The function, \eqn{f(\vec{r}) = f(x_1, x_2, \dots , x_n)}, for which you need to calculate the gradient.\cr
The function f has to return a numeric scalar.\cr
The function should only have a single argument, which is the vector of the independent variables of the function. i.e. please use \eqn{f(\vec{r})} and not \eqn{f(x_1, x_2, \dots , x_n)} for your function definition.\cr
Please see \bold{Examples}.\cr
}
  \item{pt}{
The point, \eqn{\vec{r}_o}, in the domain of the function where the gradient needs to be calculated.\cr
This has to be a numeric vector whose length is equal to the vector which is the argument of the function. i.e. the size of the vectors \eqn{\vec{r}} and \eqn{\vec{r}_o} should match.\cr
}
  \item{epsilon}{
This epsilon or \eqn{\epsilon} is the perturbation which will be used to numerically estimate the gradient.\cr
The derivative is calculated using partials for example as shown below.\cr
\deqn{\partial f/\partial x_1 = \left\{\,\, f(x_1 + \epsilon, x_2, \dots) - f(x_1 - \epsilon, x_2, \dots)\,\,\right\}/2\epsilon}\cr
The value of \eqn{\epsilon} should be chosen wisely. Please see \bold{Details}.\cr
}
}
\details{
The value of \code{epsilon}, \eqn{\epsilon}, should be chosen wisely, depending on context.\cr
The rate of rise or fall of the function between \eqn{f(x_1 + \epsilon, \dots)} and \eqn{f(x_1 - \epsilon, \dots)} should be considered.\cr
See example below.\cr
\preformatted{
#build the function
expfun <- function(X){
  return(2*X^12)
}

#call grad.func
grad.func(f = expfun, pt = 2*10^-9, epsilon = 10^-6)
grad.func(f = expfun, pt = 2*10^-9, epsilon = 10^-15)

#output
> grad.func(f = expfun, pt = 2*10^-9, epsilon = 10^-6)
[1] 4.800352e-68
> grad.func(f = expfun, pt = 2*10^-9, epsilon = 10^-15)
[1] 4.9152e-95
}
The exact answer is \eqn{4.9152 \times 10^{-95}}, but \eqn{\epsilon = 10^{-6}} is too large for a highly increasing function like \eqn{f(x) = 2x^{12}}.\cr

}
\value{
The returned value, \eqn{\nabla f}, is always a vector whose length is equal to the length of the argument \code{pt} or \eqn{\vec{r}_o}
}
\references{
\link{https://en.wikipedia.org/wiki/Gradient}\cr
\link{https://math.stackexchange.com/questions/2744497/what-does-gradient-actually-mean}
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
##define the paraboloid function centered at (3,5)
para <- function(p){
  return( (p[1] - 3)^2 + (p[2] - 5)^2)
}

#call the grad.func functions
grad.func(f=para, pt = c(3,5), epsilon = 10^-1)
grad.func(f=para, pt = c(0,0), epsilon = 10^-1)
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
