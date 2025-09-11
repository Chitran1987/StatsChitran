\name{ClearPlot}
\alias{ClearPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clears the output of the graphic device and doesn't throw an error if the graphic device is unused by R
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ClearPlot()
}
%- maybe also 'usage' for other objects documented here.
\details{
The ClearPlot() function has no return type and does not pass any arguments
}
\value{
The ClearPlot() function returns nothing
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
ClearPlot()

ClearPlot<-function(){
  if (names(dev.cur()) !='null device'){
    invisible(dev.off())
  }
  #return NULL
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
