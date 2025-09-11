\name{lin.bg.sub}
\alias{lin.bg.sub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear background subtraction out of an X-Y dataset
}
\description{
Subtracts a linear background out of a X-Y dataset and returns the background coefficients as well as the corrected dataset
}
\usage{
lin.bg.sub(df, win_df, pl = F)
lin.bg.sub(df, win_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Has to be a 2 column dataframe containing the X-Y data.\cr
First column should be the X-data.\cr
}
  \item{win_df}{
1. This is referred to as the *windows* dataframe.\cr
2. It MUST be a 2 column dataframe.\cr
3. The column names MUST be *min* and *max*.\cr
4. Each entry of this dataframe defines a window within which the background must be evaluated.\cr
}
  \item{pl}{
1. Boolean bit set to TRUE by default.\cr
2. When true plots the dataset along with a red line depicting the evaluated background.\cr
3. The column names MUST be *min* and *max*.\cr
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
1. The returned value is alway a LIST. \cr
2. The list contains two elements, 'bg_coeff' and 'sub_data'. \cr
3. 'sub_data' is a dataframe containing the background subtracted data. \cr
4. 'bg_coeff' is a vector containing the two coefficients of the linear background fit. \cr
5. The first coefficient is the slope and the second is the intercept.\cr
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
##testing
#create a gaussian on a linear background
X <- seq(0, 10, by = 0.1)
Y <- gauss(X, amp = 6, mu = 5, sig = 0.75)
Y <- Y + 0.1*X +1
#add noise
Y <- Y + rnorm(n = length(X), mean = 0, sd = 0.5)
#build dataframe
dat <- data.frame(X, Y)
subplot(c(2,1))
plot(dat$X, dat$Y, type = 'b')
abline(v = c(0, 3, 7, 10), col = 'red')
win <- data.frame(min = c(0, 7), max = c(3, 10))
names(win) <- c('min', 'max')

L <- lin.bg.sub(df = dat, win_df = win)
lines(L$sub_data$X, L$sub_data$Y, type = 'l', col = 'blue')
subplot(c(1, 1))
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
