\name{ArithGeomProg}
\alias{ArithGeomProg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a numeric vector whose elements follow an arithmetic-geometric progression
}
\description{
The function generates a sequence of numbers, \eqn{S_k}, which follow an arithmetic-geometric progression as shown below.\cr
\deqn{S_k = (S_1 + (k - 1)d)r^{k-1}}
}
\usage{
ArithGeomProg(st=1, d, r, n)
ArithProg(d, r, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{st}{
The term, \eqn{S_1}, starting point of your arithmetic-geometric progression. Defaults to one.
}
\item{d}{
Denotes the difference term, \eqn{d}, for the arithmetic progression part of the series.
}
\item{n}{
Denotes the number of terms in the arithmetic-geometric sequence
}
\item{r}{
Denotes the geometric progression ratio \eqn{r} between the subsequent terms of the geometric part of the arithmetic-geometric progression
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The return type of the function is always a vector containing \eqn{S_k,\,\,\, \forall k \in [1, n]}
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
v1 <- ArithGeomProg(n=5, d=3, r=1)
v1

v2 <- ArithGeomProg(st=2, n=5, d=3, r=2)
v2
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
