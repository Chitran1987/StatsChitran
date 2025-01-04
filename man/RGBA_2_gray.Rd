\name{RGBA_2_gray}
\alias{RGBA_2_gray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts RGBA/RGB values to grayscales
}
\description{
Converts RGB or RGBA values in R numeric formats to grayscale values of the same format
}
\usage{
RGBA_2_gray(red.ch, green.ch, blue.ch, alp.ch, bg = 1, alp = T)
RGBA_2_gray(red.ch, green.ch, blue.ch, alp = F)
RGBA_2_gray(red.ch, green.ch, blue.ch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{red.ch}{
The red channel of the data that you want to convert.\cr
This can be either a vector, a matrix or an array.
}
  \item{green.ch}{
The green channel of the data that you want to convert.\cr
This can be either a vector, a matrix or an array.
}
  \item{blue.ch}{
The blue channel of the data that you want to convert.\cr
This can be either a vector, a matrix or an array.
}
  \item{alp.ch}{
This argument is only needed when alp = T.\cr
This argument denotes the transparency of the RGB/RGBA data.\cr
The alpha channel of the data that you want to convert.\cr
This can be either a vector, a matrix or an array.
}
  \item{bg}{
This argument is only needed when alp = T.\cr
This argument has to be a single number between 0 and 1.\cr
Denotes the background for implementing the transparency.\cr
bg = 0 denotes black, while bg = 1 denotes white.\cr
bg = 1 by default, hence default background is white.
}
  \item{alp}{
This argument denotes whether an alpha value is needed for the conversion.\cr
alp = T, by default.
}
}
\details{
red.ch, green.ch, blue.ch and alp.ch (if needed), can be have structures of matrix, vectors(NOT lists) and arrays.\cr
red.ch, green.ch, blue.ch and alp.ch (if needed) all need to have the same structures and dimensions.\cr
Dataframes will not be handled.
}
\value{
The return value will have the same structure and dimension of red.ch, green.ch and blue.ch.\cr
It will defintely be a numeric as non-numeric values are not entertained as inputs.\cr
The gray values are defined as shown below......\cr
gray_val <- 0.2989*red.ch + 0.5870*green.ch + 0.1140*blue.ch  \cr
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
r.ch <- matrix(data = runif(n = 10^4, min = 0, max = 2), nrow = 100, byrow = T)
g.ch <- matrix(data = runif(n = 10^4, min = 0, max = 2), nrow = 100, byrow = T)
b.ch <- matrix(data = runif(n = 10^4, min = 0, max = 2), nrow = 100, byrow = T)
a.ch <- matrix(data = runif(n = 10^4, min = 0, max = 2), nrow = 100, byrow = T)

gray_mat1 <- RGBA_2_gray(red.ch = r.ch, green.ch = g.ch, blue.ch = b.ch, alp.ch = a.ch, bg=0, alp = T)
gray_mat2 <- RGBA_2_gray(red.ch = r.ch, green.ch = g.ch, blue.ch = b.ch, alp = F )
subplot(c(2, 1))
plot2D.mat(X = seq(1, 100), Y = seq(1, 100), Z = gray_mat1)
plot2D.mat(X = seq(1, 100), Y = seq(1, 100), Z = gray_mat2)
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
