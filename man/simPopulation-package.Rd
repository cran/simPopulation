\name{simPopulation-package}
\alias{simPopulation-package}
\alias{simPopulation}
\docType{package}
\title{
Simulation of synthetic populations for surveys based on sample data
}
\description{
Simulate populations for surveys based on sample data with special application 
to EU-SILC.
}
\details{
\tabular{ll}{
Package: \tab simPopulation\cr
Type: \tab Package\cr
Version: \tab 0.1.4\cr
Date: \tab 2010-09-15\cr
Depends: \tab R(>= 2.7.1), nnet, POT, lattice, vcd\cr
Imports: \tab lattice, vcd\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

Index:
\preformatted{
contingencyWt           Weighted contingency coefficients
eusilcS                 Synthetic EU-SILC survey data
getBreaks               Compute breakpoints for categorizing
                        (semi-)continuous variables
getCat                  Categorize (semi-)continuous variables
meanWt                  Weighted mean, variance, covariance matrix and
                        correlation matrix
quantileWt              Weighted sample quantiles
simCategorical          Simulate categorical variables of population
                        data
simComponents           Simulate components of continuous variables of
                        population data
simContinuous           Simulate continuous variables of population
                        data
simEUSILC               Simulate EU-SILC population data
simStructure            Simulate the household structure of population
                        data
spBwplot                Weighted box plots
spBwplotStats           Weighted box plot statistics
spCdf                   (Weighted empirical) cumulative distribution
                        function
spCdfplot               Plot (weighted empirical) cumulative
                        distribution functions
spMosaic                Mosaic plots of expected and realized
                        population sizes
spTable                 Cross tabulations of expected and realized
                        population sizes
tableWt                 Weighted cross tabulation
}
}
\author{
Stefan Kraft and Andreas Alfons

Maintainer: Andreas Alfons <alfons@statistik.tuwien.ac.at>
}
\keyword{package}
