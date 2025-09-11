\name{nrm}
\alias{nrm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
(normalizes) recalibrates the vector between two said values
}
\description{
This function takes in a vector and outputs a recalibrated vector according to the min and max values set in the function argument. The min and the max values are set to min=0 and max=1 by default. Hence the name nrm(X)
}
\usage{
nrm(x, min, max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
denotes the vector that needs recalibration
}
  \item{min}{
denotes the minimum value of the vector that you want to recalibrate your input vector(X) to. Default set to min=0.
}
  \item{max}{
denotes the maximum value of the vector that you want to recalibrate your input vector(X) to. Default set to max=1.
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
##testing the function1
vec_in <- c(1,1.5,3)
vec_out <- nrm(vec_in, min = 2, max = 100)

##testing the function2
t <- seq(-11,11, by=0.05)
y <- 35*sin(0.05*t)*cos(t+0.75)
par(mfrow=c(2,1))
plot(t,y,type = 'b' )
y <- nrm(y)
plot(t,y, col='red', typ='l')
########################################
## The function is currently defined as
nrm <- function(X, min=0, max=1){
  Y <- ((max - min)/(max(X) - min(X)))*(X - min(X)) + min
  return(Y)
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
