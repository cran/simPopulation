\name{tableWt}
\Rdversion{1.1}
\alias{tableWt}
\title{
  Weighted cross tabulation
}
\description{
  Compute contingency tables taking into account sample weights.
}
\usage{
tableWt(x, weights = NULL)
}
\arguments{
  \item{x}{a vector that can be interpreted as a factor, or a matrix or 
    \code{data.frame} whose columns can be interpreted as factors.}
  \item{weights}{an optional numeric vector containing sample weights.}
}
\details{
  For each combination of the variables in \code{x}, the weighted number of 
  occurence is computed as the sum of the corresponding sample weights.  If 
  weights are not specified, the function \code{\link{table}} is applied.
}
\value{
The (weighted) contingency table as an object of class \code{table}, an 
array of integer values.
}
\author{Stefan Kraft}
\seealso{\code{\link{table}}, \code{\link{contingencyWt}}}
\examples{
data(eusilcS)
tableWt(eusilcS[, c("hsize", "db040")], weights = eusilcS$rb050)
}
\keyword{category}
