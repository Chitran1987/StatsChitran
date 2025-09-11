\name{ft_inv}
\alias{ft_inv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return an inverse FFT along with the time components
}
\description{
The function returns a 2 column data frame with the first column named 't' containing the time components,\cr
and the second column named 'Y' containing the complex valued signal components of the inverted FFT.
}
\usage{
ft_inv(wf, fy)
ft_inv(wf, fy, w = F)
ft_inv(wf, fy, w = F, t_start = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wf}{
The discretized w-space vector of the fourier transsform is intended for invertion. \cr
Note: in case the frequency space of the fourier transform is in w, then set "w = T"(set by default) and sampling time Ts = 2*pi/(N*ws).\cr
In case the frequency space of the fourier transform is in f, then set "w = F" and sampling time Ts = 1/(N*ws).\cr
N  : no. of samples in your Fourier Transform.\cr
ws : smallest frequency staep size of your fourier transfrom. \cr
ws = 2*pi/T0, if "w = T". \cr
ws = 1/T0, if "w = F". \cr
T0 = N*Ts : T0 is the largest/global periodicity of the original signal that was fourier transformed from time domain.
}
  \item{fy}{
The complex fourier transformed samples intended for inverse fourier transformation.
}
  \item{w}{
A binary bit, when set to TRUE, Ts = 2*pi/(N*ws).\cr
when set to FALSE, Ts = 1/(N*ws). \cr
ws : Minimum frequency step in the wf vector. \cr
Set to TRUE by default.
}
  \item{t_start}{
The first element of the vector t, t[1]\cr
Note: incorrect assignment in t_start will lead to phase shifts in the inverted signal.\cr
Set to zero by default.
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
library(StatsChitran) #call library
X <- seq(-10*pi, 10*pi, by=0.001) #build the X-vector
Y <- sin(3*X)                     #build the sinusoid Y-vector which oscillates 3 times within 2*pi
df <- ft(X, Y, w = T, set.nyquist = T) #build the fourier transform
df_back <- ft_inv(df$wf, df$fy, w = T, t_start=-10*pi) #calculate the inverse fourier transform
subplot(c(2,1)) #divide the graphics screen
plot(df$wf, abs(df$fy), type='b', col = 'red', xlim = c(-5,5), main = 'Fourier Transform') #plot the fourier transform
plot(df_back$t, df_back$Y, xlim=c(0, 2*pi), type='l', main ='Inverse Fourier Transform', col = 'red') #plot the inverse function
abline(v=c(0, 2*pi), h=0) #build global geometric markers
abline(v = c(1, 2)*2*pi/3, col='blue') #build local geometric markers
subplot(c(1,1)) #default to original graphics screen
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
