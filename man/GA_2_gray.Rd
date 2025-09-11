\name{GA_2_gray}
\alias{GA_2_gray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts GA values to grayscales
}
\description{
Converts GA values in R numeric formats to grayscale values of the same format
}
\usage{
GA_2_gray(gray.ch, alp.ch)
GA_2_gray(gray.ch, alp.ch, bg = 1)
GA_2_gray(gray.ch, alp.ch, bg = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gray.ch}{
The gray channel of the data that you want to convert.\cr
This can be either a vector, a matrix or an array.
}
  \item{alp.ch}{
This argument denotes the transparency of the GA data.\cr
The alpha channel of the data that you want to convert.\cr
This can be either a vector, a matrix or an array.
}
  \item{bg}{
This argument has to be a single number between 0 and 1.\cr
Denotes the background for implementing the transparency.\cr
bg = 0 denotes black, while bg = 1 denotes white.\cr
bg = 1 by default, hence default background is white.
}
}
\details{
gray.ch and alp.ch, can be have structures of matrix, vectors(NOT lists) and arrays.\cr
gray.ch and alp.ch all need to have the same structures and dimensions.\cr
Dataframes will not be handled.
}
\value{
The return value will have the same structure and dimension of gray.ch and alp.ch.\cr
It will defintely be a numeric as non-numeric values are not entertained as inputs.\cr
The gray values are defined as shown below......\cr
gray_val <- alp.ch*gray_val + (1 - alp.ch)*bg
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().
#write a program to test RGBA_2_Gray
library(StatsChitran)
#define the GA array
arr <- array(data = runif(n = 2*10^4, min = 0, max = 1), dim = c(100, 100, 2))

#define the X and the Y values
X <- seq(1, 100)
Y <- seq(1, 100)

#call the GA_2_gray values with different backgrounds
M1 <- GA_2_gray(gray.ch = arr[,,1], alp.ch = arr[,,2]) #white background
M2 <- GA_2_gray(gray.ch = arr[,,1], alp.ch = arr[,,2], bg = 0) #black background

#plot the results
subplot(c(2, 1))
plot2D.mat(X, Y, Z = M1)
plot2D.mat(X, Y, Z = M2)
subplot(c(1, 1))

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
