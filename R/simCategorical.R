# ----------------------------------------
# Authors: Stefan Kraft and Andreas Alfons
#          Vienna University of Technology
# ----------------------------------------

# TODO: further generalization (more than one stratification variable)

simCategorical <- function(dataS, dataP, w = "rb050", strata = "db040",
	    basic = c("age", "rb090", "hsize"), 
        additional = c("pl030", "pb220a"),
	    maxit = 500, MaxNWts = 1500, seed) {
    
	##### initializations
	if(!missing(seed)) set.seed(seed)  # set seed of random number generator
	if(length(strata) != 1) { 
		stop("currently 'strata' must specify exactly one column of 'data'")
	}
	varNames <- c(w=w, strata=strata, basic, additional)
	
	# check data
	if(all(varNames %in% names(dataS))) dataS <- dataS[, varNames]
	else stop("undefined variables in the sample data")
    if(!all(c(strata, basic) %in% names(dataP))) {
        stop("undefined variables in the population data")
    }
	
	# observations with missings are excluded from simulation
	exclude <- getExclude(dataS)		
	if(length(exclude)) dataS <- dataS[-exclude,]
		
	# variables are coerced to factors
#	select <- c(strata, basic, additional)
#	dataS[, select] <- as.data.frame(lapply(dataS[, select], as.factor))
#	select <- c(strata, basic)
#	dataP[, select] <- as.data.frame(lapply(dataP[, select], as.factor))
    dataS <- checkFactor(dataS, c(strata, basic, additional))
    dataP <- checkFactor(dataP, c(strata, basic))
    

	# list indStrata contains the indices of dataP split by strata
    N <- nrow(dataP)
    indStrata <- split(1:N, dataP[, strata, drop=FALSE])
	
	##### simulation
    
    # predictor variables
    predNames <- basic  # names of predictor variables
    
	for(i in additional) {
		# components of multinomial model are specified
		predS <- dataS[, predNames]
		response <- dataS[, i]
		f <- paste(predNames, collapse = " + ")
        formula <- paste(i, "~", f)
        
		# all possible combinations of the predictor variables 
		# within the sample are computed
		grid <- expand.grid(lapply(predS, levels))
		gridNames <- apply(grid, 1, paste, collapse=".")
        
		values <- lapply(levels(dataS[, strata]), 
            function(s) {
                # sample data
                dataSample <- dataS[dataS[, strata] == s,]
                if(!nrow(dataSample)) return(character())
                # population data
                dataPop <- dataP[indStrata[[s]],]
                predPop <- dataPop[, predNames, drop=FALSE]
                # combinations in the stratum of the population need to be 
                # computed for prediction
                # in sample, observations with NAs have been removed to fit the 
                # model, hence population can have additional levels
                # on the other hand, not all possible combinations (within the 
                # sample) need to be represented in the current stratum of the 
                # population
                indGrid <- split(indStrata[[s]], predPop, drop=TRUE)
                indGridNames <- names(indGrid)
                whichGrid <- which(gridNames %in% indGridNames) 
                whichIndGrid <- which(indGridNames %in% gridNames) 
                ncomb <- as.integer(sapply(indGrid[whichIndGrid], length))
                # fit multinomial model and predict probabilities
                # command needs to be constructed as string
                command <- paste("suppressWarnings(multinom(", formula, 
                    ", weights=", w, ", data=dataSample, trace=FALSE", 
                    ", maxit=maxit, MaxNWts=MaxNWts))", sep="")
                mod <- eval(parse(text=command))
                probs <- predict(mod, newdata=grid[whichGrid,], type="probs")
#                # generate realizations for each combination
#                sim <- as.list(rep.int(NA, length(indGrid)))
#                sim[whichIndGrid] <- lapply(1:length(ncomb), 
#                    function(k) spSample(ncomb[k], probs[k,]))
#                sim <- unsplit(sim, predPop, drop=TRUE)
#                # ensure it works for missing levels of response
#                ind <- as.integer(which(table(dataSample[, i]) > 0))
                # ensure it works for missing levels of response
                ind <- as.integer(which(table(dataSample[, i]) > 0))
                # local function for sampling from probabilities
                if(length(ind) == 1) {
                    resample <- function(k, n, p) rep.int(1, n[k])
                } else if(length(ind) == 2) {
                    resample <- function(k, n, p) spSample(n[k], c(1-p[k],p[k]))
                } else if(is.null(dim(probs))) {
                    # only one row of new data
                    resample <- function(k, n, p) spSample(n, p)
                } else resample <- function(k, n, p) spSample(n[k], p[k,])
                # generate realizations for each combination
                sim <- as.list(rep.int(NA, length(indGrid)))
                sim[whichIndGrid] <- lapply(1:length(ncomb), 
                    resample, ncomb, probs)
                sim <- unsplit(sim, predPop, drop=TRUE)
                # return realizations
                levels(response)[ind][sim]
            })
        values <- factor(unsplit(values, dataP[, strata, drop=FALSE]), 
            levels=levels(response))
        
		## add new categorical variable to data set
        dataP[, i] <- values
        predNames <- c(predNames, i)
	}
	
    # return simulated data
    dataP
}
