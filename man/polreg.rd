\name{polreg}
\alias{polreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Polynomial Regression for Two-Column Data Frames
}
\description{
Performs polynomial regression on a two-column numeric data frame. The first
column is treated as the independent variable and the second column as the
dependent variable. A polynomial of user-specified degree is fitted using
linear regression on powers of the independent variable.
}
\usage{
polreg(df, degree)
polreg(df, degree, pl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A two-column numeric data frame. \cr
The first column contains the independent variable and the second column contains the dependent variable.
}
  \item{degree}{
A positive integer specifying the degree of the polynomial to be fitted.\cr
For example, \code{degree = 2} fits a quadratic polynomial.
}
\item{pl}{
Logical. If \code{TRUE}, the original data points are plotted together withthe fitted polynomial curve. \cr
Default is \code{TRUE}.
}
}
\details{
The function first checks whether \code{df} is a two-column data frame with
numeric columns. It then constructs a new data frame containing powers of the
independent variable from \code{degree} down to 1, together with the dependent
variable. A linear regression is then performed on this transformed data frame
using \code{\link{linreg}}.

The fitted polynomial has the form

\deqn{
Y = a_0 + a_1 X + a_2 X^2 + \cdots + a_n X^n
}

where \eqn{n} is the polynomial degree specified by \code{degree}.

The coefficient vector returned in the \code{coeff} element is ordered from
the highest polynomial degree to the constant term. Therefore, the first
element of \code{coeff} corresponds to \eqn{a_n}, the second element
corresponds to \eqn{a_{n-1}}, the third element corresponds to
\eqn{a_{n-2}}, and so on. The final element of \code{coeff} corresponds to
the constant term \eqn{a_0}.

The function returns both the regression coefficients and a fitted data frame
containing the original \eqn{X} values and the corresponding fitted
\eqn{Y} values.
}
\value{
A list with two elements:

\item{coeff}{
The regression coefficients returned by \code{\link{linreg}}.
}

\item{fit dataframe}{
A data frame containing the original independent variable values and the
corresponding fitted polynomial values. The column names are restored to the
original names of \code{df}.
}
}
\references{
\href{https://en.wikipedia.org/wiki/Polynomial_regression}{polynomial regression}
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
####Call the relevant libraries
library(StatsChitran)

###Create a dataset
#The core dataset
X <- seq(-3.5, 3.5, by=0.01)
Y <- (X-1)*(X-3)*(X+0.5)*(X+3)
plot(X, Y, type='l')
abline(h=0, col='red')
#The noisy dataset
Y <- Y + rnorm(n=length(X), sd=7)
plot(X, Y, type='l')
abline(h=0, col='red')

###Call the function and test its coefficients
fit.model <- polreg(df=data.frame(X=X, Y=Y), degree = 4)
abline(h=0, col='blue')

###Call the coefficients
fit.model$coeff
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
