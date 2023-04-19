\name{freq_map}
\alias{freq_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for frequency contribution mapping in the real functional domain
}
\description{
The function\cr
1. divides the data(X,Y) into segments decided by either nbin or xbox. For ex. no_of_segments = (max(X) - min(X))/xbox \cr
2. Performs a fourier transform on each of these segments and then integrates between the frequency limits/windows (mentioned in each w_int column)\cr
3. Saves these integration values against each segment number and builds dataframes like data(X, frequency_window(w))\cr
4. Obviously, the no. of such dataframes depends on the no. of such frequency windows asked to integrate between. (This is equal to the no. of columns of the w_int dataframe)\cr
5. Return each such dataframe as a separate object in a list\cr
6. Oviously the length of the returned list is equal to the no. of columns in w_int\cr
7. For plotting, using 'plt=T' and 'plt.leg=T/F' overlays each frequency map available in the returned list with the plotted data(X,Y). 'plt.leg' simply toggles the legend on or off.\cr
8. If 'plt=T', the 'color' vector must be provided with the right required sequence, else function throws an error.
}
\usage{
freq_map(X, Y, w_int, xbox)
freq_map(X,Y, w_int, nbin)
freq_map(X,Y, w_int, xbox, color, plt=T, plt.leg=T)
freq_map(X,Y, w_int, xbox, color, plt=T, plt.leg=F)
freq_map(X,Y, w_int, nbin, color, plt=T, plt.leg=T)
freq_map(X,Y, w_int, nbin, color, plt=T, plt.leg=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A vector with the x-values of the dataset. This argument must be provided.
}
  \item{Y}{
A vector with the y-values of the dataset. This argument must be provided.
}
  \item{w_int}{
1. A dataframe with n-columns and 2 rows\cr
2. Each value in w_int is of of 'w = 2*pi*f' or circular frequency format. DONOT use the 'w = 1*f' format\cr
3. Here the no. of required frequency windows is n\cr
4. Each column denotes a given frequency window\cr
5. Each column MUST have EXACTLY 2 values (denoting the upper/lower frequency limit of that window), else error is thrown\cr
6. This argument must be provided
}
  \item{xbox}{
1. The width of the segmentation box in real space\cr 
2. Must be a single number (numeric and 'dim(xbox)=NULL')\cr
3. Must only be provided when 'nbin' is NOT provided\cr
4. If provided along with 'nbin', function throws an error\cr
}
  \item{nbin}{
1. The no. of periodic units wanted in each freq. window (mapped w.r.t lower limit of freq window)\cr
2. Must be a numeric vector(NOT list) of length = n, otherwise error thrown\cr
3. Must only be provided when 'xbox' is NOT provided\cr
4. If provided along with 'xbox', function throws an error\cr
}
  \item{plt}{
1. Toggles plot on or off with TRUE/FALSE respectively\cr
2. Default set to FALSE
}
  \item{plt.leg}{
1. Toggles plot legend on or off with TRUE/FALSE respectively\cr
2. Ordinarily set to FALSE\cr
3. Can only be provided if 'plt=TRUE'
}
  \item{color}{
1. Vector of length n\cr
2. Provides colours of the different frequency windows to be plotted\cr
3. Must only be provided, if 'plt=TRUE'
}
}
\details{
1. X, Y, w_int and (either xbox or nbin) are mandatory\cr
2. If 'plt = TRUE', 'color' is mandatory\cr
3. w_int uses 'w = 2*pi*f' format, not 'w = 1*f' format
}
\value{
Returns a list containing dataframes related to each individual frequency window presented in the w_int dataframe
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
w1 <- c(550, 850)
w2 <- c(3000, 3300)
w3 <- c(4700, 5000)
w_df <- data.frame(w1, w2, w3)
col_vec <- c('red', 'blue', 'green')
bin <- 0.085
List_frmap <- freq_map(my_voice_01$time, my_voice_01$left, w_int = w_df, xbox = bin, plt = T, plt.leg = T, color = col_vec)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
