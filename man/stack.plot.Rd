\name{stack.plot}
\alias{stack.plot}

\title{
Stacked plotting of multiple X-Y graphs
}

\description{
Plots several X-Y graphs stacked vertically, one stack level above another.

The main data argument, \code{gr.data}, must be a list. Each element of the list
represents one stack level and must be an even-columned data frame containing
X-Y column pairs. For example, a data frame of the form
\code{data.frame(X1, Y1, X2, Y2, X3, Y3)} contains three graphs in the same
stack level.

Different stack levels are supplied as different elements of \code{gr.data}.
Graphical properties such as colour, plot type, point character, line width,
and character expansion are supplied through matrices whose rows correspond to
stack levels and whose columns correspond to graphs within each stack level.
}

\usage{
stack.plot(gr.data, stack.len, stack.rat = 1, col.eq = T,
           color.mat, type.mat, pch.mat, lwd.mat, cex.mat,
           main.txt = NULL, axlab = c("", ""), x.lim,
           show.y.axis = F)
}

\arguments{
  \item{gr.data}{
A mandatory list containing the data to be plotted. Each element of the list
must be an even-columned data frame. Consecutive column pairs are interpreted
as X-Y pairs. For example, columns \code{1, 2} define the first graph,
columns \code{3, 4} define the second graph, and so on.
}

  \item{stack.len}{
A mandatory single positive integer giving the number of stack levels. This
must be equal to \code{length(gr.data)}. Values of \code{stack.len < 2} are
not allowed; for a single graph or a single stack level, use the base
\code{\link{plot}} function instead.
}

  \item{stack.rat}{
A single numeric value controlling the vertical separation between successive
stack levels. The default is \code{1}. Larger values increase the separation
between stack levels, while smaller non-negative values reduce the separation
and may cause overlap.
}

  \item{col.eq}{
A logical scalar, defaulting to \code{TRUE}. If \code{TRUE}, all data frames
inside \code{gr.data} must have the same number of columns. If different stack
levels contain different numbers of X-Y graph pairs, set \code{col.eq = FALSE}.
}

  \item{color.mat}{
An optional matrix of dimension \code{stack.len} by \code{n}, where \code{n}
is the maximum number of graphs in any stack level. The entries define the
colours of the corresponding graphs. If not supplied, all graphs are plotted
in black.
}

  \item{type.mat}{
An optional matrix of dimension \code{stack.len} by \code{n}. The entries
define the plot type for each graph and are passed to the \code{type}
argument of \code{\link{lines}}. If not supplied, all graphs are plotted as
lines using \code{"l"}.
}

  \item{pch.mat}{
An optional matrix of dimension \code{stack.len} by \code{n}. The entries
define the plotting symbols and are passed to the \code{pch} argument of
\code{\link{lines}}. If not supplied, all entries default to \code{1}.
}

  \item{lwd.mat}{
An optional matrix of dimension \code{stack.len} by \code{n}. The entries
define the line widths and are passed to the \code{lwd} argument of
\code{\link{lines}}. If not supplied, all entries default to \code{1}.
}

  \item{cex.mat}{
An optional matrix of dimension \code{stack.len} by \code{n}. The entries
define the character expansion factors and are passed to the \code{cex}
argument of \code{\link{lines}}. If not supplied, all entries default to
\code{1}.
}

  \item{main.txt}{
An optional character string used as the main title of the plot. The default
is \code{NULL}.
}

  \item{axlab}{
A character vector of length two giving the x-axis and y-axis labels,
respectively. The default is \code{c("", "")}.
}

  \item{x.lim}{
An optional numeric vector of length two giving the x-axis plotting range.
The lower limit is \code{x.lim[1]} and the upper limit is \code{x.lim[2]}.
If \code{x.lim} is not supplied, the x-axis range is calculated automatically
from the data.
}

  \item{show.y.axis}{
A logical scalar, defaulting to \code{FALSE}. If \code{FALSE}, the default
y-axis tick marks and tick labels are suppressed using \code{yaxt = "n"}.
If \code{TRUE}, the standard y-axis is drawn using \code{yaxt = "s"}.
For stacked plots, the y-axis usually represents shifted plotting coordinates
rather than the original y-values.
}
}

\details{
Each element of \code{gr.data} corresponds to one stack level. Within each
data frame, every pair of columns defines one graph:

\preformatted{
Column 1: X values for graph 1
Column 2: Y values for graph 1
Column 3: X values for graph 2
Column 4: Y values for graph 2
...
}

The function shifts the y-values of each stack level vertically so that the
different stack levels are displayed one above another. For each stack level,
the minimum y-value is subtracted before the vertical stack offset is added.

The graphical parameter matrices \code{color.mat}, \code{type.mat},
\code{pch.mat}, \code{lwd.mat}, and \code{cex.mat} use the same indexing
convention. Row \code{i} corresponds to stack level \code{i}; column \code{j}
corresponds to graph \code{j} within that stack level.

If \code{col.eq = FALSE}, different stack levels may contain different numbers
of graphs. In that case, the graphical parameter matrices must still have
\code{n} columns, where \code{n} is the maximum number of graphs in any stack
level. Entries corresponding to non-existent graphs in shorter stack levels
are ignored.
}

\value{
The function is called for its plotting side effect and returns \code{NULL}
invisibly if execution is not interrupted by an error.
}

\author{
Chitran Ghosal
}

\seealso{
\code{\link{plot}}, \code{\link{lines}}, \code{\link{axis}}
}

\examples{
## Build example X values
X <- seq(-10, 10, by = 0.1)

## Amplitudes for three Gaussian components across six stack levels
vec <- c(1, 0.5, 0.125,
         0.125, 0.5, 1,
         0.25, 0.5, 1.0,
         1.0, 0.5, 0.25,
         0.25, 0.5, 1.0,
         1.0, 0.5, 0.25)

I <- matrix(data = vec, byrow = TRUE, nrow = 6)

## Gaussian centres and widths
mu_vec <- c(0, 3, -3)
sig_vec <- c(1, 2, 2)

## Build gr.data as a six-element list
L <- vector(mode = "list", length = 6)

for (i in 1:6) {
  df1 <- data.frame(X, gauss(X, amp = I[i, 1], mu = mu_vec[1], sig = sig_vec[1]))
  df2 <- data.frame(X, gauss(X, amp = I[i, 2], mu = mu_vec[2], sig = sig_vec[2]))
  df3 <- data.frame(X, gauss(X, amp = I[i, 3], mu = mu_vec[3], sig = sig_vec[3]))
  df4 <- data.frame(X, df1[, 2] + df2[, 2] + df3[, 2])

  L[[i]] <- as.data.frame(cbind(df1, df2, df3, df4))
}

## Build graphical parameter matrices
v1 <- v2 <- v3 <- v4 <- NULL

for (i in 1:6) {
  v1 <- c(v1, rgb(1, 0, 0, 1))
  v2 <- c(v2, rgb(0, 1, 0, 1))
  v3 <- c(v3, rgb(0, 0, 1, 1))
  v4 <- c(v4, rgb(0, 0, 0, 0.2))
}

Mcol <- matrix(data = c(v1, v2, v3, v4), nrow = 6, byrow = FALSE)
Mtyp <- matrix(data = rep(c("l", "l", "l", "p"), times = 6), nrow = 6, byrow = TRUE)
Mpch <- matrix(data = rep(21, times = 24), nrow = 6, byrow = FALSE)
Mlwd <- matrix(data = rep(2, times = 24), nrow = 6, byrow = FALSE)

## Plot stacked graph
stack.plot(gr.data = L,
           stack.len = 6,
           stack.rat = 1.2,
           color.mat = Mcol,
           type.mat = Mtyp,
           pch.mat = Mpch,
           lwd.mat = Mlwd,
           axlab = c("X", "Y"),
           main.txt = "Example Graph")

## Same plot with standard y-axis shown
stack.plot(gr.data = L,
           stack.len = 6,
           stack.rat = 1.2,
           color.mat = Mcol,
           type.mat = Mtyp,
           pch.mat = Mpch,
           lwd.mat = Mlwd,
           axlab = c("X", "Y"),
           main.txt = "Example Graph with y-axis",
           show.y.axis = TRUE)
}

\keyword{hplot}
