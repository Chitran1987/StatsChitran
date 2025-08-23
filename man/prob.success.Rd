\name{prob.success}
\alias{prob.success}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability of at least \eqn{r} succesful events
}
\description{
Returns the probability of AT LEAST \eqn{r} succesful events in a series of \eqn{N} independent events\cr
The probability of occurence/success of a single event is \eqn{p}\cr
Should not be confused with the binomial mass function \eqn{P[k] = {}^nC_kp^k(1-p)^{n-k}}. \cr
See \emph{details}.
}
\usage{
prob.success(N.trials, p, r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N.trials}{
The total no of trials, \eqn{n}, in the experiment. See \emph{details}
}
  \item{p}{
The probability of occurrence/success of a single independent event\cr
Denoted as \eqn{p}
}
  \item{r}{
The minimum no. of successes, \eqn{r}, expected out of the \eqn{n} trials \cr
Obviously \eqn{0 \leq r \leq n}\cr
}
}
\details{
Should not be confused with the binomial mass function \eqn{P[k] = {}^nC_kp^k(1-p)^{n-k}}. \cr
\eqn{P[k]} returns the probabilty of EXACTLY \eqn{r} successes and \bold{NOT} AT LEAST \eqn{r} successes. \cr
The probability of at least \eqn{r} successes is returned by \eqn{\mathcal{P}[r]}
\deqn{\displaystyle \mathcal{P}[r] = \sum_{k = r}^n{}^nC_k \,p^k (1-p)^{n-k}}
}
\value{
Returns a single integer denoting the probability of at least \eqn{r} successes
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
Please note that the probability of success of at least \eqn{r = 0} succesful events is 1.\cr
Since it encompasses the entire sample space. See \emph{details}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Load the library
library(StatsChitran)

#Call the function
prob.success(N.trials = 3, p = 0.5, r = 2)
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
