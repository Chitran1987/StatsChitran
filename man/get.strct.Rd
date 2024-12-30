\name{get.strct}
\alias{get.strct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the structure of an object
}
\description{
Tells you the structure of an object among dataframe, matrix, array, vector, list, function and unknown
}
\usage{
get.strct(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
The object whose structure you want to get
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is always a string
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
#build function
dumm <- function(X){
  return(X)
}

#build other data-types and put them in a list
L <- list(vec = c(1, 2, 3, 4))
L$matrix <- matrix(data = L$vec, nrow = 2, byrow = T)
L$dataframe <- data.frame(L$matrix)
L$array <- as.array(L$matrix)
L$list <- L
L$func <- dumm

#loop through the list to see the outputs
for(i in 1:length(L)){
get.strct(L[[i]])
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
