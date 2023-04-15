\name{num_diff}
\alias{num_diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a dataframe consisting of the numerically differentiated second variable w.r.t the first variable (dY/dX) as the second column and (X) as the first(independent variable) column. It also plots the curve depending on the input arguments
}
\description{
The function allows for the numeric differentiation of the second column of your data frame with respect to the first
}
\usage{
num_diff(X,Y)
num_diff(X,Y, pl=F)
num_diff(X,Y, type='left', pl=T)
num_diff(X,Y, type='right', pl=T)
num_diff(X,Y, type='center', pl=T)
}
\arguments{
  \item{X}{
The input X-column.
}
\item{Y}{
The input Y-column.
}
\item{type}{
The type variable can be set to 'left', 'right' or 'center'. Depending on each it calculates the left handed or right handed differentiation dataframe. Set to 'center' by default.\cr
Note, when type='center', the X-axis values of the returned dataframe differs slightly from the x-axis values of the original dataset
}
  \item{pl}{
Set to true by default, the argument can be toggled.When set to TRUE, Outputs a plot of the differentiated function with respect to the original.
  }
}
\details{
The function always returns a dataframe. There are no other return types or no way to tune the return type in this function.
}
\value{
The return type of the function is always a 2-column dataframe with the 1st column being the independent variable, while the next column is the dependent variable(dY/dX)
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
X <- seq(0,2*pi, by=0.01)
y <- sin(X)
a <- num_diff(X,y)
abline(v=pi, col='red')
abline(h=0, col='black')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ numerical differentiation}% use one of  RShowDoc("KEYWORDS")
\keyword{ numerical calculus}% __ONLY ONE__ keyword per line
