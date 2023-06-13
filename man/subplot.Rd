\name{subplot}
\alias{subplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Divides the display into a grid for multiple plots
}
\description{
Allows plotting multiple plots in the divided Graphics window
}
\usage{
subplot(v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
A numeric atomic vector of dimension 2
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Return type is NULL
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
 X <- rnorm(10^4, mean=3, sd=3)
 Y <- X^2
 subplot(c(2,1))
 hist(X)
 hist(Y)
 subplot(c(1,1))

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
