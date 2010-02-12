\name{simContinuous}
\Rdversion{1.1}
\alias{simContinuous}
\title{
  Simulate continuous variables of population data
}
\description{
  Simulate continuous variables of population data using multinomial log-linear 
  models combined with random draws from the resulting categories or (two-step) 
  regression models combined with random error terms.  The household structure 
  of the population data and any other categorical predictors need to be 
  simulated beforehand.
}
\usage{
simContinuous(dataS, dataP, w = "rb050", strata = "db040", 
              basic = c("age", "rb090", "hsize"), 
              additional = "netIncome", 
              method = c("multinom", "lm"), zeros = TRUE, 
              breaks = NULL, lower = NULL, upper = NULL, 
              gpd = TRUE, threshold = NULL, est = "moments", 
              censor = NULL, log = TRUE, alpha = 0.01, 
              residuals = TRUE, keep = TRUE, 
              maxit = 500, MaxNWts = 1500, 
              tol = .Machine$double.eps^0.5, seed)
}
\arguments{
  \item{dataS}{a \code{data.frame} containing household survey data.}
  \item{dataP}{a \code{data.frame} containing the simulated population data.  
    Household structure and any other categorical predictors need to be 
    simulated beforehand.}
  \item{w}{a character string specifying the column of \code{dataS} that 
    contains the (personal) sample weights.}
  \item{strata}{a character string specifying the columns of \code{dataS} and 
    \code{dataP}, respectively, that define strata.  The regression models are 
    computed for each stratum separately.  Note that this is currently a 
    required argument and only one stratification variable is supported.}
  \item{basic}{a character vector specifying the columns of \code{dataS} and 
    \code{dataP}, respectively, that define the household structure and any 
    other categorical predictors, such as age, gender and household size.}
  \item{additional}{a character string specifying the additional continuous 
    variable of \code{dataS} that should be simulated for the population data.  
    Currently, only one additional variable can be simulated at a time.}
  \item{method}{a character string specifying the method to be used for 
    simulating personal income.  Accepted values are \code{"multinom"}, for 
    using multinomial log-linear models combined with random draws from the 
    resulting ategories, and \code{"lm"}, for using (two-step) regression 
    models combined with random error terms.}
  \item{zeros}{a logical indicating whether the variable specified by 
    \code{additional} is semi-continuous, i.e., contains a considerable amount 
    of zeros.  If \code{TRUE} and \code{method} is \code{"multinom"}, a 
    separate factor level for zeros in the response is used.  If \code{TRUE} 
    and \code{method} is \code{"lm"}, a two-step model is applied.  The first 
    step thereby uses a log-linear or multinomial log-linear model (see 
    \dQuote{Details}).}
  \item{breaks}{an optional numeric vector; if multinomial models are computed, 
    this can be used to supply two or more breakpoints for categorizing the 
    variable specified by \code{additional}.  If \code{NULL}, breakpoints are 
    computed using weighted quantiles.}
  \item{lower, upper}{optional numeric values; if multinomial models are 
    computed and \code{breaks} is \code{NULL}, these can be used to specify 
    lower and upper bounds other than minimum and maximum, respectively.  Note 
    that if \code{method} is \code{"multinom"} and \code{gpd} is \code{TRUE} 
    (see below), \code{upper} defaults to \code{Inf}.}
  \item{gpd}{logical; if \code{method} is \code{"multinom"}, this indicates 
    whether the upper tail of the variable specified by \code{additional} 
    should be simulated by random draws from a (truncated) generalized Pareto 
    distribution rather than a uniform distribution.}
  \item{threshold}{a numeric value; if \code{method} is \code{"multinom"}, 
    values for categories above \code{threshold} are drawn from  a (truncated) 
    generalized Pareto distribution.}
  \item{est}{a character string; if \code{method} is \code{"multinom"}, the 
    estimator to be used to fit the generalized Pareto distribution (see 
    \code{\link[POT]{fitgpd}}).}
  \item{censor}{an optional named list of \code{data.frame}s; if multinomial 
    models are computed, this can be used to account for structural zeros.  The 
    names of the list components specify the categories that should be 
    censored.  For each of these categories, a \code{data.frame} containing 
    levels of the predictor variables can be supplied.  The probability of the 
    specified categories is set to 0 for the respective predictor levels.  
    Currently, this is only implemented for more than two categories in the 
    response.}
  \item{log}{logical; if \code{method} is \code{"lm"}, this indicates whether 
    the linear model should be fitted to the logarithms of the variable 
    specified by \code{additional}.  The predicted values are then 
    back-transformed with the exponential function.  If \code{TRUE} and there 
    are negative values, a two-step model is applied  (see \dQuote{Details}).}
  \item{alpha}{numeric; if \code{method} is \code{"lm"}, this gives trimming 
    parameters for the sample data.  Trimming is thereby done with respect to 
    the variable specified by \code{additional}.  If a numeric vector of length 
    two is supplied, the first element gives the trimming proportion for the 
    lower part and the second element the trimming proportion for the upper 
    part.  If a single numeric is supplied, it is used for both.  With 
    \code{NULL}, trimming is suppressed.}
  \item{residuals}{logical; if \code{method} is \code{"lm"}, this indicates 
    whether the random error terms should be obtained by draws from the 
    residuals.  If \code{FALSE}, they are drawn from a normal distribution 
    (median and MAD of the residuals are used as parameters).}
  \item{keep}{logical; if multinomial models are computed, this indicates 
    whether the simulated categories should be stored as a variable in the 
    resulting population data.  If \code{TRUE}, the corresponding column name 
    is given by \code{additional} with postfix \code{"Cat"}.}
  \item{maxit, MaxNWts}{control parameters to be passed to 
    \code{\link[nnet]{multinom}} and \code{\link[nnet]{nnet}}.  See the help 
    file for \code{\link[nnet]{nnet}}.}
  \item{tol}{if \code{method} is \code{"lm"}, a small positive numeric value or 
    \code{NULL}.  When fitting a log-linear model within a stratum, factor 
    levels may not exist in the sample but are likely to exist in the 
    population.  However, the coefficient for such factor levels will be 0.  
    Therefore, coefficients smaller than \code{tol} in absolute value are 
    replaced by coefficients from an auxiliary model that is fit to the whole 
    sample.  If \code{NULL}, no auxiliary log-linear model is computed and no 
    coefficients are replaced.}
  \item{seed}{optional; an integer value to be used as the seed of the random 
    number generator, or an integer vector containing the state of the random 
    number generator to be restored.}
}
\details{
  If \code{method} is \code{"lm"}, the behavior for two-step models is 
  described in the following.   If \code{zeros} is \code{TRUE} and \code{log} 
  is not \code{TRUE} or the variable specified by \code{additional} does not 
  contain negative values, a log-linear model is used to predict whether an 
  observation is zero or not.  Then a linear model is used to predict the 
  non-zero values.  If \code{zeros} is \code{TRUE}, \code{log} is \code{TRUE} 
  and there are negative values, a multinomial log-linear model is used to 
  predict negative, zero and positive observations.  Categories for the 
  negative values are thereby defined by \code{breaks}. In the second step, a 
  linear model is used to predict the positive values and negative values are 
  drawn from uniform distributions in the respective classes.  If \code{zeros} 
  is \code{FALSE} and \code{log} is \code{TRUE}, a two-step model is used if 
  there are non-positive values in the variable specified by \code{additional}. 
  Whether a log-linear or a multinomial log-linear model is used depends on the 
  number of categories to be used for the non-positive values, as defined by 
  \code{breaks}.  Again, positive values are then predicted with a linear 
  model and non-positive values are drawn from uniform distributions.}
\value{
  A \code{data.frame} containing the simulated population data including the 
  continuous variable specified by \code{additional}.
}
\author{
  Original code by Stefan Kraft, redesign and generalizations by Andreas Alfons.
}
\note{
  The basic household structure and any other categorical predictors need to 
  be simulated beforehand with the functions \code{\link{simStructure}} and 
  \code{\link{simCategorical}}, respectively.
}
\seealso{
  \code{\link{simStructure}}, \code{\link{simCategorical}}, 
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
basic <- c("age", "rb090", "hsize", "pl030", "pb220a")

# multinomial model with random draws
eusilcM <- simContinuous(eusilcS, eusilcP, 
    basic = basic, upper = 200000)
summary(eusilcM)

# two-step regression
eusilcT <- simContinuous(eusilcS, eusilcP, 
    basic = basic, method = "lm")
summary(eusilcT)
}
}
\keyword{datagen}
