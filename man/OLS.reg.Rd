\name{OLS.reg}
\alias{OLS.reg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
General purpose modelling using OLS(Ordinary Least Squares) regression
}
\description{
Fits a function \eqn{f(x_1, x_2, \dots, x_{n-1}, \vec{v} )} to a multivariate dataset \eqn{\left(x_1, x_2, \dots , x_{n-1}, y \right) }.\cr
The vector \eqn{\vec{v}} contains the parameters that are to be extracted by the fitting procedure.\cr
Uses OLS regression methodology.\cr
}
\usage{
OLS.reg(model, dat, guess, algorithm = 'Nelder-Mead', Delta = 10^-6)
OLS.reg(model, dat, guess, algorithm)
OLS.reg(model, dat, guess, Delta)
OLS.reg(model, dat, guess)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
Has to be a function, \eqn{f(x_1, x_2, \dots, x_{n-1}, \vec{v} )}, with a scalar numeric return type.\cr
Needs to have a vector, \eqn{\vec{v}}, as its LAST argument. All elements of this vector should be the individual scalar parameters that need to be extracted.\cr
The first \eqn{n - 1} arguments should be the independent varaibles or \eqn{x_k} that describe the model.\cr
See the formulation below
\deqn{f(x_1, x_2, \dots, x_{n-1}, \vec{v} ) }
\deqn{\vec{v} = \left( v[1], v[2], \dots, v[m]\right)}
}
  \item{dat}{
Has to be the dataframe with \eqn{n} columns.\cr
The first \eqn{n - 1} columns should be the data for the first \eqn{x_k} variables IN THE ORDER in which the arguments are described in the model.\cr
The last column will be the actual \eqn{y} values of the data.\cr
Please see \bold{examples}\cr
}
  \item{guess}{
Has to be a numeric vector.\cr
The initial guess for the value of \eqn{\vec{v}}.\cr
Obviously its length should match the length of the vector \eqn{\vec{v}} passed in the model function.\cr
}
  \item{algorithm}{
Denotes the optimization algorithm.\cr
A character-scalar which only values within the R-vector \code{c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")}\cr
Defaults to \code{"Nelder-Mead"}.\cr
Uses the \link{optim} function for the optimization process.\cr
}
  \item{Delta}{
Has to be a numeric scalar, \eqn{\delta},.\cr
The peturbation used to calculate the gradient \eqn{\nabla f_{\vec{v}}} of the function, \eqn{f(x_1, x_2, \dots, x_{n-1}, \vec{v} )}.\cr
Passes \eqn{\delta} to \code{epsilon} or \eqn{\epsilon} while using \link{grad.func} to calculate \eqn{\nabla f_{\vec{v}}}.\cr
Defaults to \eqn{\delta = 10^{-6}}, but should be chosen with caution, see \link{grad.func}
}
}
\details{
With \eqn{f(x_1, x_2, \dots, x_{n-1}, \vec{v} )}, make sure that the \code{dat} dataframe has columns \eqn{x_1, x_2, \dots , x_{n-1}, y} in the SAME ORDER.\cr
Uses the simple squared error and mean squared error loss and cost functions.\cr
\deqn{l_i(\vec{v}) = \{ y_i - f(x_{i,1}, x_{i,2}, \dots x_{i, n-1}, \vec{v}) \}^2}
\deqn{C(\vec{v})= \frac{1}{N}{{\sum_i l_i(\vec{v})}} }

}
\value{
The reurned value is a 6 element list \cr
  \itemize{
    \item \code{par}: The optimized/extracted parameters for the  vector \eqn{\vec{v}}
    \item \code{value}: The value of the cost function \eqn{C(\vec{v})} at the optimized value of \eqn{\vec{v}}.
    \item \code{counts}: This a 2-element vector. 1st elem: tells us the no. of times the function was evaluated, 2nd elem: gradient at that point. See \link{optim}
    \item \code{convergence}: Is an integer value, 0 implies succesful convergence, 1 implies maximum no. of iterations reached prior to convergence, 10 implies degenerate problem detected. See \link{optim}.
    \item \code{message}: helps diagnose convergence issues. See \link{optim}.
    \item \code{err}: The residuals, \eqn{e_i}, after the fitting process, calculated  at the optimized value of \eqn{\vec{v}}
    \deqn{e_i = y_i - f(x_{i,1}, x_{i,2}, \dots, x_{i,n-1}, \vec{v} )}
  }

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
#Build the dataframe for dat
X1 <- sort(rnorm(500))
X2 <- sort(rnorm(500))
Y <- 110*X1 + 120*X2
Y <- Y + rnorm(n = length(X1), mean = 0, sd = 10)
plot(X1, Y)
plot(X2, Y)
df <- data.frame(X1, X2, Y)


#Build the function for the model
lin.fun <- function(X1, X2, v){
  Y <- v[1]*X1 + v[2]*X2
  return(Y)
}


lst <- OLS.reg(model = lin.fun, dat = df, guess = c(50, 50), algorithm = 'BFGS')


library(StatsChitran)
plot(X1, Y)
lines(X1, lin.fun(X1, X2, v=lst$par), col='red', lwd=3)
plot(X2, Y)
lines(X2, lin.fun(X1, X2, v=lst$par), col='red', lwd=3)
lst$par
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
