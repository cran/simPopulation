\name{contingencyWt}
\Rdversion{1.1}
\alias{contingencyWt}
\title{
  Weighted contingency coefficients
}
\description{
  Compute (weighted) contingency coefficients.
}
\usage{
contingencyWt(x, y, weights = NULL)
}
\arguments{
  \item{x, y}{two vectors that can be interpreted as factors.}
  \item{weights}{an optional numeric vector containing sample weights.}
}
\details{
  The function \code{\link{tableWt}} is used for the computation of the 
  corresponding contingency table.
}
\value{
  The (weighted) contingency coefficient of \code{x} and \code{y}.
}
\references{
Kendall, M.G. and Stuart, A. (1967) \emph{The Advanced Theory of Statistics, 
Volume 2: Inference and Relationship}. Charles Griffin & Co Ltd, London, 2nd 
edition.}
\author{Stefan Kraft}
\seealso{\code{\link{tableWt}}}
\examples{
data(eusilcS)
contingencyWt(eusilcS$pl030, eusilcS$pb220a, weights = eusilcS$rb050)
}
\keyword{category}
