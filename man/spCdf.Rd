\name{spCdf}
\Rdversion{1.1}
\alias{spCdf}
\title{
  (Weighted empirical) cumulative distribution function
}
\description{
  Compute a (weighted empirical) cumulative distribution function for survey 
  or population data.  For survey data, sample weights are taken into account.
}
\usage{
spCdf(x, weights = NULL, approx = FALSE, n = 10000)
}
\arguments{
  \item{x}{a numeric vector.}
  \item{weights}{an optional numeric vector containing sample weights.}
  \item{approx}{a logical indicating whether an approximation of the cumulative 
    distribution function should be computed.}
  \item{n}{a single integer value; if \code{approx} is \code{TRUE}, this 
    specifies the number of points at which the approximation takes place 
    (see \code{\link[stats:approxfun]{approx}}).}
}
\details{
  Sample weights are taken into account by adjusting the step height.  To be 
  precise, the weighted step height for an observation is defined as its weight 
  divided by the sum of all weights\eqn{\ ( w_{i} / \sum_{j = 1}^{n} w_{j} ).}{.}
  
  If requested, the approximation is performed using the function 
  \code{\link[stats:approxfun]{approx}}.
}
\value{
  A list of class \code{"spCdf"} with the following components:
  \item{x}{a numeric vector containing the \eqn{x}-coordinates.}
  \item{y}{a numeric vector containing the \eqn{y}-coordinates.}
  \item{approx}{a logical indicating whether the coordinates represent an 
    approximation.}
}
\author{
  Andreas Alfons and Stefan Kraft
}
\seealso{
  \code{\link{spCdfplot}}, \code{\link[stats]{ecdf}}, 
  \code{\link[stats:approxfun]{approx}}
}
\examples{
data(eusilcS)
cdfS <- spCdf(eusilcS$netIncome, weights = eusilcS$rb050)
plot(cdfS, type="s")
}
\keyword{dplot}
