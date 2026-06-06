\name{exp.prob}
\alias{exp.prob}
\title{Exponential Decay Function and Probability Distribution}

\description{
Computes an exponential decay function of the form
\eqn{A \exp(-\lambda |x|)}
or, when \code{probability = TRUE}, returns the normalized symmetric exponential probability distribution
\eqn{\frac{\lambda}{2} \exp(-\lambda |x|)}.
}

\usage{
exp.prob(X, amp, lambda, probability = FALSE)
}

\arguments{
  \item{X}{
    A numeric vector containing the input values at which the function is evaluated.
  }

  \item{amp}{
    Numeric scaling factor for the non-normalized exponential decay function.
    This argument is required when \code{probability = FALSE}.
    It should be omitted when \code{probability = TRUE}.
  }

  \item{lambda}{
    Positive numeric value controlling the decay rate.
  }

  \item{probability}{
    Logical value. If \code{FALSE}, the function returns a scaled exponential decay.
    If \code{TRUE}, the function returns the normalized probability distribution.
    Default is \code{FALSE}.
  }
}

\details{
When \code{probability = FALSE}, the function evaluates

\deqn{
Y = A \exp(-\lambda |X|)
}

where \eqn{A} is supplied through the \code{amp} argument.

When \code{probability = TRUE}, the function evaluates

\deqn{
Y = \frac{\lambda}{2} \exp(-\lambda |X|)
}

This corresponds to a symmetric exponential distribution centered at zero,
also known as the Laplace distribution with location parameter zero and
scale parameter \eqn{1/\lambda}.

If \code{probability = TRUE}, the argument \code{amp} should not be supplied,
because the normalization factor is already determined by \code{lambda}.
}

\value{
A numeric vector of the same length as \code{X}, containing the evaluated
exponential decay or probability density values.
}

\references{
Laplace, P. S. (1774). Memoir on the probability of the causes of events.
}

\author{
Chitran Ghosal
}

\examples{
# Input vector
x <- seq(-5, 5, length.out = 100)

# Scaled exponential decay
y1 <- exp.prob(X = x, amp = 2, lambda = 1)

# Normalized probability distribution
y2 <- exp.prob(X = x, lambda = 1, probability = TRUE)

# Plot scaled exponential decay
plot(x, y1, type = "l",
     xlab = "X",
     ylab = "Y",
     main = "Scaled Exponential Decay")

# Plot probability distribution
plot(x, y2, type = "l",
     xlab = "X",
     ylab = "Probability Density",
     main = "Symmetric Exponential Probability Distribution")
}

\seealso{
\code{\link{exp}}
}

\keyword{distribution}
\keyword{probability}
\keyword{exponential}
\keyword{univar}
