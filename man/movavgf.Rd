\name{movavgf}
\alias{movavgf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a self-defined-functionalized (not just a table function ) moving average of the 2D dataset in question
}
\description{
The function returns a 2 column data frame with the first column named X.
The second column named Y contains the smoothed dataset
}
\usage{
movavgf(X, Y, bn, fn, f, val)
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
The no of elements before the current element that is needed to be included in the averaging window
}
  \item{fn}{
The no of elements after the current element that is needed to be included in the averaging window
}
  \item{f}{
This argument is a function using which the moving window used for averaging of the X, Y dataset is created.\cr
The window is centered at wc = "val", (see argument "val").\cr
If "s" is the sampling interval of variable X, then the window abscissa vector "w"" is calculated as..\cr
w = seq(val - bn*s, val + fn*s) \cr
The window ordinate is calculated as f_seq = f(w).
}
  \item{val}{
The value around which the averaging window should be centered.\cr
For example, for a standard normal distribution val = 0, but for a gaussian with a mean of 4, val = 4.
}
    \item{ord}{
The no. of times you want to run the movavgf fucntion for smoothing.\cr
Defaults to \code{ord = 1}
}
}
\details{
Ensure before using that... \cr
1) f_seq does not contain Inf elements. \cr
2) f_seq does not contain NAs. \cr
3) f_seq does not contain NANs. \cr
All of the above will result in errors being thrown by the function.

}
\value{
The return type is always a 2 column data frame with the first column named X is same as the input argument X, while the second column Y is the smoothed dataset.
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
#call the relevant libraries
library(StatsChitran)

#Call the dataset
df <- noisy.gaussians


##define the kernel function (lorentzian)
lor.avg <- function(v){
  return(lorentz(X = v, x_0 = 0, gamm = 1, probability = T))
}

##divide the plot into two
subplot(c(1,2))

##plot the kernel function
Y_kernel <- lor.avg(df$X)
plot(df$X, Y_kernel, type = 'l', col = 'red', main = 'The lorentzian kernel')
abline(v = 0)


##Perform kernel smoothening of ord = 1, 10, 100
df1 <- movavgf(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 1)
df2 <- movavgf(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 10)
df3 <- movavgf(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 100)

##Plot the smoothed dataframes against the main data
plot(df$X, df$Y, main = 'Data Processing')
lines(df1$X, df1$Y, col = 'green')
lines(df2$X, df2$Y, col = 'red')
lines(df3$X, df3$Y, col = 'blue')
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
