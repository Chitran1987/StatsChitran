\name{img_2_data}
\alias{img_2_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Takes a PNG image and converts it into grayscale and returns the individual channels along with the grayscale matrix.
}
\description{
Inputs a PNG image and return the grayscale matrix along with the individual channels separated.\cr
The input PNG images are normally classified into RGB, RGBA, G/grayscale or GA types.\cr
The grayscale matrix could also be plotted.
}
\usage{
img_2_data(source.png, x.lim, y.lim, pl = T, bgrnd = 1)
img_2_data(source.png, x.lim, y.lim)
img_2_data(source.png, x.lim, y.lim, bgrnd = 0.5)
img_2_data(source.png, x.lim, y.lim, pl = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source.png}{
Path leading to the PNG file
}
  \item{x.lim}{
The x-limits of the image.\cr
Must be a two element numeric vector.
}
  \item{y.lim}{
The y-limits of the image.\cr
Must be a two element numeric vector.
}
  \item{pl}{
Boolean bit, set to TRUE by default.\cr
Plots the grayscale matrix if set to TRUE.
}
  \item{bgrnd}{
Numeric scalar, set to 1 by default.\cr
Value should lie between 0 and 1.\cr
Determines the grayscale value of the background.\cr
Values of 1 and 0 determine white and black backgrounds respectively.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a two-element list.\cr
The first element in the list is "gray.val" which is the grayscale matrix.\cr
The second element of the list is "data".\cr
"data" itself is a list which contains the matrixes pertaining to the different channels of the original PNG image, like red, green, blue and alpha in case of an RGBA image.
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

library(StatsChitran)
dat <- 'asymmetry.png'
L <- img_2_data(source.png = dat, x.lim = c(0, 7), y.lim = c(0, 3), bgrnd = 0.1)
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
