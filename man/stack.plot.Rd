\name{stack.plot}
\alias{stack.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for plotting X-Y graphs stacked one on top of the other
}
\description{
The function\cr
1.  Plots stacks of X-Y graphs \cr
2.  Graphs can exist within the same stack or can belong to different stacks \cr
3.  Graphs that exist within the same stack have properties like colour, type and width specified as elements within the same row of the concerned matrix
4.  Accepts only a list as the main data argument (gr.data). This argument is obviously mandatory \cr
5.  The length of the list is the no. of stacks/rows. This argument is also mandatory (stack.len)\cr
6.  Each element of the list should be an even columned dataframe\cr
7.  Preferably the no. of columns of each dataframe should be the same. If not, col.eq should be set to false\cr
8.  Dataframes need to be even columned because each dataframe consists of X-Y pairs which exist in the stack/row\cr
9.  A dataframe should be defined as  data.fame(X1, Y1, X2, Y2, X3, Y3,......Xn,Yn)\cr
10. The dataframe shown above will have n graphs within the same stack
11. The no. of stacks to be plotted is equal to the length of the gr.data list as specified in point 5.
}
\usage{
stack.plot(gr.data, stack.len, stack.rat = 1, col.eq = T, color.mat, type.mat, pch.mat, lwd.mat, main.txt = NULL, axlab = c('', ''))
stack.plot(gr.data, stack.len)
stack.plot(gr.data, stack.len, col.eq = F)
stack.plot(gr.data, stack.len, stack.rat = 1.2, color.mat = M, axlab = c('X', 'Y'))
stack.plot(gr.data, stack.len, color.mat = M, axlab = c('X', 'Y'))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr.data}{
1.  gr.data has to be a list. Each element of the list should be an even columned dataframe. \cr
2.  If the no. of columns of the dataframes existing as different elements of the list is not the same, this should be specified in the 'col.eq' argument. \cr
3.  This argument is mandatory and has to be provided.
}
  \item{col.eq}{
1.  boolean bit, defaults to TRUE. \cr
2.  If the no. of columns of the dataframes existing as different elements of the 'gr.data' list is not the same, then 'col.eq = F' should be set.
}
  \item{stack.len}{
1.  single numeric positive integer. \cr
2.  'stack.len' should be equal to the length of the 'gr.data' list.
3.  This argument is mandatory and has to be provided.
}
  \item{stack.rat}{
1.  single numeric defaults to 1. \cr
2.  defines the scaling of the height of each individual stack before the next stack starts. \cr
3.  ht_stack is defined as ht_stack = stack.rat*(max(Y_stack) - min(Y_stack)). \cr
}
  \item{color.mat}{
1.  A matrix of dimension (stack.len X n) which defaults to 'black' for each element. \cr
2.  n here is the maximum no. of datasets in a stack compared to all stacks. \cr
3.  This matrix contains the colors of each of the 'stack.len X n' plots.
}
  \item{type.mat}{
1.  A matrix of dimension (stack.len X n) which defaults to 'l'(line) for each element. \cr
2.  The matrix contains the 'type' information of each of the 'stack.len X n' plots. \cr
3.  Each element of 'type.mat' can be specified as any of the allowed inputs of the 'type' argument in the 'plot' function in the base R package.
}
  \item{pch.mat}{
1.  A matrix of dimension (stack.len X n) which defaults to '1'(default 'points' in base R) for each element. \cr
2.  The matrix contains the 'pch' information of each of the 'stack.len X n' plots. \cr
3.  Each element of 'pch.mat' can be specified as any of the allowed inputs of the 'pch' argument in the 'plot' function in the base R package.
}
  \item{lwd.mat}{
1.  A matrix of dimension (stack.len X n) which defaults to '1'(default 'points' in base R) for each element. \cr
2.  The matrix contains the 'lwd' information of each of the 'stack.len X n' plots. \cr
3.  Each element of 'lwd.mat' can be specified as any of the allowed inputs of the 'lwd' argument in the 'plot' function in the base R package.
}
  \item{main.txt}{
1.  A single string to the used as a header of the plot, defaults to NULL.
}
  \item{ax.lab}{
1.  Needs to be a two element character vector, defaults to 'c('', '')'. \cr
2.  Defines the labels for the x-axis and the y-axis
}
}
\details{
1.  'gr.data' and 'stack.len' are the only absolutely necessary arguments. \cr
}
\value{
The function always returns a NULL value if execution is not interrupted by an error.
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
rm(list=ls())
library(StatsChitran)


###### A 6-stack, 4-graph dataset is built out of (6 X 4) X-Y pairs.
###### Each of the 6 dataframes hence contain 8 columns
###### The first 6 columns of each dataframe contains the X values and Y values of 3 gaussians(df1, df2, df3) whose amplitudes we vary as we change stack
###### The last two columns of each dataframe is the sum of the first three gaussians

#build the X values
X <- seq(-10, 10, by= 0.1)

#build the amplitude values of the gaussians
#since we have only three gaussians per stack and 6 independent stacks, hence we have 18 amplitude values in a (6 X 3) matrix
vec <- c(1, 0.5, 0.125, 0.125, 0.5, 1, 0.25, 0.5, 1.0, 1.0, 0.5, 0.25, 0.25, 0.5, 1.0, 1.0, 0.5, 0.25)
I <- matrix(data = vec, byrow = F, nrow = 6)

#build the mu and sig vectors of the three gaussians
#we don't change the center and width of the gaussians from stack to stack for this example
mu_vec <- c(0, 3, -3)
sig_vec <- c(1, 2, 2)

#build the dataset for gr.data
#since we need 6 stacks, hence we need a 6 element list
L <- vector(mode = 'list', length = 6) #list to store the dataframes in
for (i in 1:6) {
  df1 <-  data.frame(X, gauss(X, amp = I[i, 1], mu = mu_vec[1], sig = sig_vec[1]))
  df2 <-  data.frame(X, gauss(X, amp = I[i, 2], mu = mu_vec[2], sig = sig_vec[2]))
  df3 <-  data.frame(X, gauss(X, amp = I[i, 3], mu = mu_vec[3], sig = sig_vec[3]))
  df4 <-  data.frame(X, df1[,2] + df2[,2] + df3[,2])
  df  <-  as.data.frame(cbind(df1, df2, df3, df4))
 L[[i]] <- df
}

##build the parameters for testing
#build the colour matrix
#we don't change the color of the relevant component from stack to stack to study its behaviour
v1 <- v2 <- v3 <- v4 <- NULL
for (i in 1:6) {
  v1 <- c( v1, rgb(1, 0, 0, 1) )
  v2 <- c( v2, rgb(0, 1, 0, 1) )
  v3 <- c( v3, rgb(0, 0, 1, 1) )
  v4 <- c( v4, rgb(0, 0, 0, 0.2) )
}
Mcol <- matrix(data = c(v1, v2, v3, v4), nrow = 6, byrow = F)

#build the type matrix
Mtyp <- matrix(data = rep(c('l', 'l', 'l', 'p'), times = 6), nrow = 6, byrow = T)

#build the pch matrix
Mpch <- matrix(data = rep(21, times = 24), nrow = 6, byrow = F)

#build the lwd matrix
Mlwd <- matrix(data = rep(2, times = 24), nrow = 6, byrow = F)

stack.plot(gr.data = L, stack.len = 6, stack.rat = 1.2, color.mat = Mcol, type.mat = Mtyp, pch.mat = Mpch, lwd.mat = Mlwd, axlab = c('X', 'Y'), main.txt = 'Example Graph')

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
