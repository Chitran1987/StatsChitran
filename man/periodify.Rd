\name{periodify}
\alias{periodify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Periodically increases the span of a given X-Y dataset
}
\description{
Adds an integer no. of periods of the X-Y dataset to itself. \cr
The no. of integer periods by which the X-Y dataset is increased in the forward and backward direction is specified by "fwd" and "bwd" arguments. \cr
Hence, if L is the length of the X-Y dataset, the new dataset has a length of "L*(fwd + bwd + 1)". \cr
The returned dataset is always a dataframe
}
\usage{
periodify(X, Y, fwd, bwd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
An atomic numeric vector X signifying the X-axis of the dataset
}
  \item{Y}{
An atomic numeric vector Y signifying the Y-axis of the dataset
}
  \item{fwd}{
A single whole number or integer specifying the no. of periods by which to increase the span of the dataset in the forward direction
}
  \item{bwd}{
A single whole number or integer specifying the no. of periods by which to increase the span of the dataset in the backward direction
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned datatype is always a 2 column dataframe with the new X and Y vectors contained in it and named as "X" and "Y respectively"
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
Can be used to increase the resolution of a DFT(Discrete Fourier Transform). See examples.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

library(StatsChitran)
##define the datasets
X <- seq(0, 2*pi, by = 0.1)
Y <- sin(X)
subplot(c(2, 2)) #divide the plotting screen
plot(X, Y, type = 'l', col = 'gray', lwd = 2, main = 'original') #plot the basic dataset
ft_XY <- ft(X, Y, w = T, set.nyquist = T) #fourier transform of the original dataset
plot(ft_XY$wf, abs(ft_XY$fy), type = 'b', col = 'red', xlim = c(-10, 10), main = 'original fft') #plot the amplitude function of the original dataset
df <- periodify(X, Y, fwd = 1, bwd = 2) #use the periodify function to build a dataset df
plot(df$X, df$Y, type = 'l', col = 'gray', lwd = 2, main = 'new dataset') #plot the new dataset
ft_df <- ft(df$X, df$Y, w = T, set.nyquist = T) #fourier transform of the new dataset
plot(ft_df$wf, abs(ft_df$fy), type = 'b', col = 'red', xlim = c(-10, 10), main = 'new fft') #plot the amplitude function of the new dataset
subplot(c(1, 1)) #return to the original screen

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
