\name{mat.mask}
\alias{mat.mask}

\title{
Transform matrix data upon masking
}
\description{
Transforms a matrix called \code{org}, represented by \eqn{ \mathbf{Z} = (z_{ij})_{p \times q}}, using a matrix called \code{mask}, represented by \eqn{\mathbf{M} = (m_{ij})_{p \times q}}.\cr
Both matrices, \code{org} and \code{mask} have the same dimensionality, i.e. \eqn{p \times q}.\cr
However both matrices belong to different vector spaces,\cr
Since \code{org} is real or complex, but \code{mask} is boolean, i.e. \eqn{\mathbf{Z} \in \mathbb{C}^{p \times q}\, \wedge \, \mathbf{M} \in \mathbb{B}^{p \times q}}
}
\usage{
mat.mask(org, mask, drop.dat=F)
mat.mask(org, mask)
mat.mask(org, mask, drop.dat=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{org}{
The original data matrix \eqn{ \mathbf{Z} = (z_{ij})_{p \times q}} that you want to mask.\cr
Can be numeric, complex or boolean.
}
  \item{mask}{
The masking matrix \eqn{\mathbf{M} = (m_{ij})_{p \times q}} that you want to use as a mask.\cr
Should be a boolean matrix.\cr
Should have the same dimensions as the \code{org} matrix.
}
  \item{drop.dat}{
Boolean bit defaulting to \code{FALSE}.\cr
For all indices \eqn{i,j}, when \eqn{m_{ij} == \code{T}}, then the corresponding \eqn{z_{ij}} remains unchanged.\cr
When \code{drop.dat == F}, then \eqn{\forall m_{ij} = \code{F}, \,\, z_{ij}=0}.\cr
When \code{drop.dat == T}, then \eqn{\forall m_{ij} = \code{F}}, either \eqn{ z_{ij}=0} or \eqn{z_{ij}} will be completely dropped from the matrix.\cr
Please see details.
}
}
\details{
\code{drop.dat == F} preserves the size of the \code{org} and \code{mask} data and transfers it to the output.\cr
The unmasked parts, where \eqn{m_{ij} == \code{F}} are at least etched down to \eqn{0}.\cr
Hence a presumptive bias might be needed in the \code{org} matrix to separate actual \eqn{0}s and etched \eqn{0}s.\cr
When \code{drop.dat == T}, after the etching process, the matrix might be reduced in size.\cr
The algo chooses the largest bounding rectangle of choice which contains the masked results.\cr
The rest of the complementary data outside the rectangle contains only etched \eqn{0}s and are removed from the result.\cr
Hence \code{drop.dat == T} might fail to transfer the size of \code{org} or \code{mask} to the result.\cr
Please see figure below.\cr
\if{html}{\figure{mat.mask.png}{options: width=600px}}
}
\value{
The returned value is a matrix which is exactly the type(numeric, complex, boolean) that \code{org} was.\cr
The size of the matrix is \eqn{p \times q} (same as \code{mask} and \code{org}) when \code{drop.dat == F}.\cr
The size of the matrix might be different when \code{drop.dat == T}.
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
####create a 3 gaussian function
gauss3 <- function(X, Y){
  set.seed(2)
  mu <- runif(n = 6, min = 2, max = 7)
  sd <- runif(n = 6, min = 1, max = 3)
  gauss1 <- gauss(X, mu=mu[1], sig = sd[1], probability = T)*gauss(Y, mu=mu[2], sig = sd[2], probability = T)
  gauss2 <- gauss(X, mu=mu[3], sig = sd[3], probability = T)*gauss(Y, mu=mu[4], sig = sd[4], probability = T)
  gauss3 <- gauss(X, mu=mu[5], sig = sd[5], probability = T)*gauss(Y, mu=mu[6], sig = sd[6], probability = T)
  return(gauss1 + gauss2 + gauss3)
}
####define the X and Y values
X <- seq(0, 10, by=0.05)
Y <- seq(0, 10, by=0.05)
Z <- 0.9*function_2D(X, Y, func = gauss3)

####plot the matrix
image(Z, asp=1.0, main = 'org', col = terrain.colors(1000))

####create the masks
mask1 <- Z > 0.5*max(Z)
mask2 <- Z > 0.75*max(Z)
image(mask1, asp = 1.0, main = 'mask1',col = terrain.colors(1000) )
image(mask2, asp = 1.0, main = 'mask2', col = terrain.colors(1000))

##The mask after dropping points
Z1 <- mat.mask(org = Z, mask = mask1, drop.dat = T)
Z2 <- mat.mask(org = Z, mask = mask2, drop.dat = T)
image(Z1, asp=1.0, main = 'after masking by mask1: points dropped', col = terrain.colors(1000))
image(Z2, asp=1.0, main = 'after masking by mask2: points dropped', col = terrain.colors(1000))


##The mask set without dropping points
Z1 <- mat.mask(org = Z, mask = mask1, drop.dat = F)
Z2 <- mat.mask(org = Z, mask = mask2, drop.dat = F)
image(log(Z1+1), asp=1.0, main = 'after masking by mask1: points NOT dropped', col = terrain.colors(1000))
image(log(Z2+1), asp=1.0, main = 'after masking by mask2: points NOT dropped', col = terrain.colors(1000))
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
