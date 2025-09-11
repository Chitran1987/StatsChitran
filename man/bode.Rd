\name{bode}
\alias{bode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for creating a bode plot from a continuous time transfer function
}
\description{
The function\cr
1. returns a list with two dataframes, one for the mag plot and the other for the phase plot \cr
2. plots the mag and phase if the argument "pl" is set to TRUE (default) \cr
3. Allows to reduce execution time by setting desired resolution using n (default = 5*10^5) \cr
}
\usage{
bode(wL, wU, fun = tf1)
bode(wL, wU, fun = tf1, pl = T)
bode(wL, wU, fun = tf1, pl = F)
bode(wL, wU, fun = tf1, pl = F, n = 3000)
bode(wL, wU, fun = tf1, n = 3000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wL}{
Lower limit of frequency window in bode plot. Single numeric value
}
  \item{wU}{
Upper limit of frequency window in bode plot. Single numeric value
}
  \item{fun}{
A single argument function describing the transfer function
}
  \item{pl}{
A single boolean value,(default = TRUE), toggles plot on or off
}
  \item{n}{
Single numeric value, (default = 5*10^5), decides the resolution of the bode plot
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a 2 element list containing dataframes related to magnitude and phase plots
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

##define transfer function
tf <- function(s){
  K <- 0.0016
  w1 <- 2*pi*1000
  w2 <- 2*pi*50000
  return(K*s/((s/w1 + 1)*(s/w2 + 1)))
}

##call bode function
L <- bode(0.1, 10^8, fun = tf)
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
