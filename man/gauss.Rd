\name{gauss}
\alias{gauss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
returns a vector with the gaussian equation aginst the input vector X
}
\description{
The function takes in a vector "X"" against which it returns a vector with the gaussian with the other parameters "amp", "mu", "sig" and "probability"
}
\usage{
gauss(X, amp, sig, mu)
gauss(X, amp, sig, mu, probability=F)
gauss(X, sig, mu, probability=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Describes the (numeric) vector against which you need the gaussian vector
}
  \item{probability}{
Describes a single boolean value. Ordinarily set to FALSE by default. Used to select between a normalized gaussian(probability = TRUE) or a non-normalized statistical distribution(probability = FALSE)
}
  \item{amp}{
Amplitude of the gaussian or the value of the gaussian at the mean. Can only be specified when probability = FALSE, if specified when probability = TRUE, then function will throw an error
}
  \item{sig}{
Describes the standard deviation of the gaussian
}
  \item{mu}{
Describes the mean value of the gaussian
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a numeric vector.
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
X <- seq(-10,10,by=0.5)
y <- gauss(X, sig = 1.5, mu = 3, probability = T)
y1 <- gauss(X, amp=1, sig = 1.5, mu = 3, probability = F)
plot(X,y1, type = 'b')
lines(X,y, type='b', col='red')
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
