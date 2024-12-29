\name{plot2D.mat}
\alias{plot2D.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a matrix from x-values, y-values and z-vales
}
\description{
The function plots a grayscale image matrix from vectors x and y and matrix Z
}
\usage{
plot2D.mat <- function(X, Y, Z, gray.dens = 10^5, x.lab = NULL, y.lab = NULL)
plot2D.mat <- function(X, Y, Z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Vector describing the x-axis
}
  \item{Y}{
Vector describing the y-axis
}
  \item{Z}{
Matrix describing the Z values.\cr
Should have dimensions length(Y)*length(X)
}
  \item{gray.dens}{
Numeric value used to describe the grayscale density.\cr
Default value set to 10^5.
}
  \item{x.lab}{
Sets the label of the x-axis.\cr
Character vector of length 1 needed.\cr
Default value set to NULL.
}
  \item{y.lab}{
Sets the label of the y-axis.\cr
Character vector of length 1 needed.\cr
Default value set to NULL.
}
}
\details{
X and Y need to be numeric vectors.\cr
Z needs to be a matrix.\cr
for directly plotting the matrix from a function of X and Y, please see *plot2D()*.\cr
gray.dens has to be a whole number.\cr
x.lab and y.lab have to be a single valued character vectors, if not NULL.~
}
\value{
This function returns a NULL value
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
#define the x-axis
X <- seq(0, 10, by = 0.01)
Y <- seq(0, 16, by = 0.01)

#define the function
patt <- function(x, y){
  return(exp(-0.2*x - 0.1*y)*sin(3*x)*cos(4*y))
}

#build the Z matrix
Z <- function_2D(X, Y, patt)

#plot the matrix
plot2D.mat(X, Y, Z)
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
