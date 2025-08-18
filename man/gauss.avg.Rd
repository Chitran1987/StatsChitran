\name{gauss.avg}
\alias{gauss.avg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return a moving average with a gaussian kernel of the 2D dataset in question
}
\description{
The function returns a 2 column data frame with the first column named X. The second column named Y contains the averaged dataset.\cr
The kernel used for the averaging is a gaussian kernel. See \code{\link{gauss}}
}
\usage{
gauss.avg(X, Y, bn, fn, sig = 1, ord = 1, pl = T)
gauss.avg(X, Y, bn, fn)
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
  \item{sig}{
  The standard deviation of the gaussian used as the kernel
  }
  \item{ord}{
  The no. of iterations of the smoothing function on the dataset\cr
  Defaults to one
  }
  \item{pl}{
  Defaults to \code{TRUE} \cr
  Permission to plot the function
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a 2 column dataframe\cr
Columns named as X and Y
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
\code{\link{gauss}}, \code{\link{movavgf}}
}
\examples{
df1 <- gauss.avg(X = df$X, Y = df$Y, fn = 3, bn = 3, sig = 3, pl = F)
df2 <- gauss.avg(X = df$X, Y = df$Y, fn = 3, bn = 3, sig = 3, pl = F, ord = 300)
plot(df$X, df$Y)
lines(df1$X, df1$Y, col = 'green')
lines(df2$X, df2$Y, col = 'red')
legend(x = 'topright', legend = c('data', 'ord = 1', 'ord = 300'), col = c('black', 'green', 'red'), lty = 1, lwd = 2)
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
