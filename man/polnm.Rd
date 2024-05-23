\name{polnm}
\alias{polnm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Defines a univariate polynomial with coefficients set by the user
}
\description{
Returns a 2 column dataframe.\cr
First column containts the independent variable.\cr
Second column contains the dependent variable (polynomial outputs against the independent variable).
}
\usage{
polnm(X, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The independent variable against which the polynomial needs to be defined.\cr
Has to be numeric.
}
  \item{v}{
The vector comprising of the sequence of coefficients of the polynomials.\cr
Has to be numeric.
}
}
\details{
Suppose that a polynomial is defined as y = ao + a1*x + a2*x^2 + a3*x^3 + ... \cr
Then for the vector v,
v[1] = a0
v[2] = a1
v[3] = a2
v[4] = a3
.
.
.
so on and so forth
}
\value{
The returned value is always a dataframe with column names 'X' and 'Y'
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
S <- seq(-2, 2, by = 0.001) #define the independent variable
coeff <- c(0, 1, 1) #define the coefficient vector ( a0 = 0, a1 = 1, a2 = 1)
df <- polnm(X = S, v = coeff) #call the polnm function
plot(df$X, df$Y) #plot the output
##check the roots
abline(h = 0)
abline(v = c(-1, 0))
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
