\name{ft}
\alias{ft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return an FFT along with the freq components
}
\description{
The function returns a 2 column data frame with the first column (named fr or wf, depending whether w=F or w=T in the arguments respectively) containing the frequencies of the frequency space.
The second column named fy contains the complex values of the fourier transform
}
\usage{
ft(X,Y,w=F, set.nyquist = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Contains the X component of the input function
}
  \item{Y}{
  Contains the Y component of the input function
  }
  \item{w}{
  Set to w=F by default, it toggles between fr or wf=2*pi*fr on the frequency axis
  }
  \item{set.nyquist}{
  Ordinarily set to true, this argument(when true) promotes the dataset which is greater than half of the sampling frequency to the negative frequencies.\cr
  This is mathematically allowed since the DFT is periodic in frequency domain
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
