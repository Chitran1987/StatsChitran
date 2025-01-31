\name{OLS.reg}
\alias{OLS.reg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
General purpose modelling using OLS(Ordinary Least Squares) regression
}
\description{
General-purpose function for model fitting/parameter extraction using the MSE(Mean Squared Error) loss functions from the OLS(Ordinary Least Squares) regression methodology.\cr
No constraints can be applied to the parameters during the regression process.\cr
Based on only the gradient free Nelder-Mead or SANN methods depending on choice.\cr
}
\usage{
OLS.reg(model, dat, guess)
OLS.reg(model, dat, guess, method = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
Has to be a function with a scalar numeric return type.\cr
Needs to have a vector as its LAST argument. All elements of this vector should be the individual scalar parameters that need to be extracted.\cr
The first n-1 arguments should be the n-1 X's(independent variables) that describe the model. Please see examples\cr
}
  \item{dat}{
Has to be the dataframe with n columns.\cr
The first n-1 columns should be the data for the first n-1 X's IN THE ORDER in which the arguments are described in the model.\cr
The last column will be the actual Y values of the data against which the regression will be performed. Please see examples\cr
}
  \item{guess}{
Denotes the initial guess from where to start the minima search in parameter space.\cr
Obviously it should be numeric and its length should match the length of the vector(LAST argument) in the function.\cr
}
  \item{method}{
Denotes the optimization algorithm. Can take only values of 1(Nelder-Mead) or 2(SANN).\cr
Obviously it should be a numeric.\cr
Nelder-Mead is a faster, but less precise. SANN is slower, resource heavy, but considerably more precise.\cr
}
}
\details{
When the *model* function is f(x1, x2, x3, ... , xn-1, V), make sure that the *dat* dataframe has columns x1, x2, x3, ... , xn-1, Y in the SAME ORDER
}
\value{
The reurned value is a 6 element list \cr
par: defines the optimized parameters for the fit/values for vector V in the model function f(x1, x2, x3, ... , xn-1, V) \cr
value: The value of the MSE(Mean Squared Error) with the current fit \cr
counts: This a 2-element vector. 1st elem: tells us the no. of times the function was evaluated, 2nd elem: gradient at that point \cr
convergence: Is an integer value, 0 implies succesful convergence, 1 implies maximum no. of iterations reached prior to convergence, 10 implies degenerate problem detected. See optim() for further clarification. \cr
message: helps diagnose convergence issues. See optim() for further clarification.\cr
err: The residuals after the fitting process, basically err = Y - f(x1, x2, ... , xn-1, V = par).\cr
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
Y <- Y + rnorm(n = length(X1), mean = 0, sd = 100)
df <- data.frame(X1, X2, Y)


#Build the function for the model
lin.fun <- function(X1, X2, v){
  Y <- v[1]*X1 + v[2]*X2
  return(Y)
}

#Call the OLS regression function
lst <- OLS.reg(model = lin.fun, dat = df, guess = c(105, 115), method = 2)


#Plot the built regression model
ClearPlot()
subplot(c(1,2))
plot(X1, Y)
lines(X1, lin.fun(X1, X2, v=lst$par), col='red', lwd=3)
plot(X2, Y)
lines(X2, lin.fun(X1, X2, v=lst$par), col='red', lwd=3)
subplot(c(1,1))
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
