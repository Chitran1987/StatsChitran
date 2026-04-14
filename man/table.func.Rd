\name{table.func}
\alias{table.func}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct a table function over a numeric vector
}
\description{
Constructs a piecewise constant ("table") function over a numeric vector
\code{X}, using user-supplied breakpoints \code{BP} and interval heights
\code{H}.


The function assigns a constant value from \code{H} to each interval of
\code{X} determined by the breakpoints in \code{BP}.
}
\usage{
table.func(X, BP, H)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
 A numeric vector over which the table function is to be evaluated.
}
  \item{BP}{
  A numeric vector of breakpoints that divide \code{X} into intervals.
    These should usually be supplied in increasing order.
  }
  \item{H}{
  A numeric vector of heights defining the constant function value on each
    interval. Its length must be equal to \code{length(BP) + 1}.
  }
}
\details{
Given breakpoints \code{BP = c(b_1, b_2, ..., b_n)} and heights
\code{H = c(h_1, h_2, ..., h_{n+1})}, the table function is defined as:

\deqn{
f(x) =
\left\{
\begin{array}{ll}
h_1, & x \le b_1 \\
h_2, & b_1 < x \le b_2 \\
\vdots & \\
h_n, & b_{n-1} < x \le b_n \\
h_{n+1}, & x > b_n
\end{array}
\right.
}

Thus, the intervals are left-open and right-closed after the first interval:
\itemize{
  \item \code{x <= BP[1]} gets \code{H[1]}
  \item \code{BP[i-1] < x <= BP[i]} gets \code{H[i]} for intermediate intervals
  \item \code{x > BP[length(BP)]} gets \code{H[length(H)]}
}
}
\value{
A data frame with the following columns:
\itemize{
  \item \code{X}: the input numeric vector
  \item \code{Y}: the corresponding function values assigned according to
  \code{BP} and \code{H}
}
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
X <- seq(-10, 10, by = 0.01)
BP <- c(-3, 4, 7)
H  <- c(1, 2, 3, 4)

df <- table.func(X, BP, H)
plot(df$X, df$Y, type='l', col='red')
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
