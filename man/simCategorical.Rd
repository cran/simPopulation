\name{simCategorical}
\Rdversion{1.1}
\alias{simCategorical}
\title{
  Simulate categorical variables of population data
}
\description{
  Simulate categorical variables of population data.  The household structure 
  of the population data needs to be simulated beforehand.
}
\usage{
simCategorical(dataS, dataP, w = "rb050", strata = "db040", 
               basic, additional = c("pl030", "pb220a"), 
               method = c("multinom", "distribution"), 
               maxit = 500, MaxNWts = 1500, seed)
}
\arguments{
  \item{dataS}{a \code{data.frame} containing household survey data.}
  \item{dataP}{a \code{data.frame} containing the simulated population 
    household structure.}
  \item{w}{a character string specifying the column of \code{dataS} that 
    contains the (personal) sample weights.}
  \item{strata}{a character string specifying the columns of \code{dataS} and 
    \code{dataP}, respectively, that define strata.  The values are simulated 
    for each stratum separately.  Note that this is currently a required 
    argument and only one stratification variable is supported.}
  \item{basic}{a character vector specifying the columns of \code{dataS} and 
    \code{dataP}, respectively, that define the household structure, typically 
    age, gender and household size.  The default value is \code{c("age", 
    "rb090", "hsize")} if \code{method} is \code{"multinom"}, and 
    \code{c("age", "rb090")} if \code{method} is \code{"distribution"}.}
  \item{additional}{a character vector specifying additional categorical 
    variables of \code{dataS} that should be simulated for the population data.}
  \item{method}{a character string specifying the method to be used for 
    simulating the additional categorical variables.  Accepted values are 
    \code{"multinom"} (estimation of the conditional probabilities using 
    multinomial log-linear models and random draws from the resulting 
    distributions), or \code{"distribution"} (random draws from the observed 
    conditional distributions of their multivariate realizations).}
  \item{maxit, MaxNWts}{control parameters to be passed to 
    \code{\link[nnet]{multinom}} and \code{\link[nnet]{nnet}}.  See the help 
    file for \code{\link[nnet]{nnet}}.}
  \item{seed}{optional; an integer value to be used as the seed of the random 
    number generator, or an integer vector containing the state of the random 
    number generator to be restored.}
}
\value{
  A \code{data.frame} containing the simulated population data including the 
  categorical variables specified by \code{additional}.
}
\author{Andreas Alfons and Stefan Kraft}
\note{
  The basic household structure needs to be simulated beforehand with the 
  function \code{\link{simStructure}}.
}
\seealso{
  \code{\link{simStructure}}, \code{\link{simContinuous}}, 
  \code{\link{simComponents}}, \code{\link{simEUSILC}}
}
\examples{
\dontrun{

## these take some time and are not run automatically
## copy & paste to the R command line

set.seed(1234)  # for reproducibility
data(eusilcS)   # load sample data
eusilcP <- simStructure(eusilcS)
eusilcP <- simCategorical(eusilcS, eusilcP)
summary(eusilcP)
}
}
\keyword{datagen}
