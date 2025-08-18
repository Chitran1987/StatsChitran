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
movavg(X,Y,bn,fn, ord = 2)
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
 \item{ord}{
The no. of times you want to run the movavg function for smoothing.\cr
Ordinarily defaults to \code{ord = 1}.\cr
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a 2 column dataframe.\cr
The columns are named as X and Y.
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
df <- movavg(x,y, 10, 10) #moving average of bn = fn = 10
lines(df$X, df$Y, col='red')
df <- movavg(x, y, 1, 10) ##moving average of bn = 1 and fn = 10
lines(df$X, df$Y, col = 'blue')
df <- movavg(x,y, 10, 10, ord = 3)
lines(df$X, df$Y, col = 'green')
legend('top', legend = c('Noisy Data', 'movavg(bn = fn = 10)', 'movavg(bn = 1, fn = 10)', 'movavg(bn = 10, fn = 10, ord = 3)'), col = c('black', 'red', 'blue', 'green'), lty = c(1,1,1,1))
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
