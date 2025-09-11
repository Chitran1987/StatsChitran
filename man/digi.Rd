\name{digi}
\alias{digi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a digital ON-OFF function which is toggled for specific windows
}
\description{
1. This function returns a 2 column dataframe with columns X and Y.\cr
2. The Y column is mostly 1( inv = F (default)) or 0( inv = T) with specific *windows* wherein the function is toggled in the opposite direction  .\cr
3. These *windows* are defined by the *win_df* dataframe called a window dataframe
}
\usage{
digi(X, win_df = win_d, inv = T)
digi(X, win_df = win_d, inv = F)
digi(X, win_df = win_d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The X axis values for which the Y values need to be defined
}
  \item{win_df}{
1. This is referred to as the *windows* dataframe.\cr
2. It MUST be a 2 column dataframe.\cr
3. The column names MUST be *min* and *max*.\cr
4. Each entry of this dataframe defines a window within which the function must be toggled.\cr
5. The max and min values of this window MUST be less than and greater than the max and min values of the X sequence. \cr
}
  \item{inv}{
1. A boolean bit, set to FALSE by deafult.\cr
2. When inv = FALSE, Y = 0 within the windows and Y = 1 otherwise. \cr
3. When inv = TRUE, Y = 1 within the windows and Y = 0, otherwise
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The returned value is a 2 column dataframe with names 'X' and 'Y'
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
#Let's create a sequence within X = 0 and X = 20 with windows at (12, 14) and (18, 19)
X <- seq(10, 20, by = 0.01)
win_d <- data.frame(c(12, 18), c(14, 19))
names(win_d) <- c('min', 'max')

#Call the digi function
df <- digi(X, win_df = win_d)
df1 <- digi(X, win_df = win_d, inv = T)

#plot the data
subplot(c(2,1))
plot(df$X, df$Y, type = 'l', col = 'red')
plot(df1$X, df1$Y, type = 'l', col = 'blue')
subplot(c(1,1))
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
