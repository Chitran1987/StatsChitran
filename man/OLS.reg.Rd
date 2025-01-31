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
Needs to have
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (x)
{
  }
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
