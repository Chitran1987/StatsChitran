\name{movavg}
\alias{movavg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a moving average of the 2D dataset in question
}
\description{
The function returns a 2 column data frame with the first column named X.
The second column named Y contains the averaged dataset
}
\usage{
movavg(X,Y,bn,fn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The X vector of the 2D dataset
}
 \item{Y}{
The Y vector of the 2D dataset
}
 \item{bn}{
The no. of elements before the current element thatyou want included in the average
}
 \item{fn}{
The no. of elements after the current element(future element) thatyou want included in the average
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
###plotting a noisy parabola
x <- seq(-1, 1, by = 0.01)
y <- x^2 + rnorm(length(x), mean=0, sd=0.1)
plot(x,y)

###plotting the moving average
df <- movavg(x,y, 10, 10)
lines(df$X, df$Y, col='red')
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
