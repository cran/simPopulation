\name{getBreaks}
\alias{getBreaks}
\title{
  Compute breakpoints for categorizing (semi-)continuous variables
}
\description{
  Compute breakpoints for categorizing continuous or semi-continuous variables 
  using (weighted) quantiles.  This is a utility function that is useful for 
  writing custom wrapper functions such as \code{\link{simEUSILC}}.
}
\usage{
getBreaks(x, weights = NULL, zeros = TRUE, 
          lower = NULL, upper = NULL)
}
\arguments{
  \item{x}{a numeric vector to be categorized.}
  \item{weights}{an optional numeric vector containing sample weights.}
  \item{zeros}{a logical indicating whether \code{x} is semi-continuous, i.e., 
    contains a considerable amount of zeros.  See \dQuote{Details} on how this 
    affects the behavior of the function.}
  \item{lower, upper}{optional numeric values specifying lower and upper bounds 
    other than minimum and maximum of \code{x}, respectively.}
}
\details{
  If \code{zeros} is \code{TRUE}, the 0\%, 10\%, \dots, 90\% quantiles of the 
  negative values and the 10\%, 20\%, \dots, 100\% of the positive values are 
  computed.  These quantiles are then used as breakpoints together with 0.  If
  \code{zeros} is not \code{TRUE}, on the other hand, the 0\%, 10\%, \dots, 
  100\% quantiles of all values are used.
  
  Note that duplicated values among the quantiles are discarded and that the 
  minimum and maximum are replaced with \code{lower} and \code{upper}, 
  respectively, if these are specified.
  
  The (weighted) quantiles are computed with the function 
  \code{\link{quantileWt}}. 
}
\value{
  A numeric vector of breakpoints.
}
\author{
  Andreas Alfons
}
\seealso{
  \code{\link{getCat}}, \code{\link{quantileWt}}
}
\examples{
data(eusilcS)

## semi-continuous variable
getBreaks(eusilcS$netIncome, weights=eusilcS$rb050)
}
\keyword{manip}
