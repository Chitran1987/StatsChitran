\name{plot2D}
\alias{plot2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Accepts a 2D function f(x,y) along with x and y vectors and plots it
}
\description{
Accepts a function f(x,y) along with vectors, x and y and plots it
}
\usage{
function(X, Y, func, pl = T, gray.dens = 10^5, x.lab = NULL, y.lab = NULL)
function(X, Y, func, pl = T)
function(X, Y, func, pl = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Describes the (numeric) vector in the X direction
}
  \item{Y}{
Describes the (numeric) vector in the Y direction
}
  \item{func}{
Describes the function used to calculate the Z return matrix
}
  \item{pl}{
Boolean bit used to specify whether plotting is needed or not.\cr
Default set to TRUE.
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
func needs to be a function.\cr
for directly plotting a matrix, please see *plot2D.mat()*.\cr
gray.dens has to be a whole number.\cr
x.lab and y.lab have to be a single valued character vectors, if not NULL.
}
\value{
The returned value is always a numeric matrix.\cr
If pl is set to TRUE, a plot is also produced.\cr
The plot is always returned with an asp = 1.0. \cr
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
#define the function
f_test <- function(x, y){
  return(sin(5 * x) * cos(5 * y) + cos(x^2 + y^2))
}

#define the x-axis
x_seq <- seq(-10, 10, by = 0.01)

#define the y-axis
y_seq <- seq(-7, 7, by = 0.01)

#plot the function and get the matrix Z
Z = plot2D(X = x_seq, Y = y_seq, func = f_test, x.lab = 'X', y.lab = 'Y')

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
