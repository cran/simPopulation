# ----------------------------------------
# Authors: Andreas Alfons and Stefan Kraft
#          Vienna University of Technology
# ----------------------------------------

## check break points for categorization
checkBreaks <- function(x) {
    if(!is.numeric(x) || length(x) < 2) {
        stop("'breaks' must be a numeric vector with length >= 2")
    }
    invisible()
}

## check whether selected columns of a data frame are factors
# TODO: add argument 'num': a logical indicating whether numeric variables other than integers should be converted to factors
checkFactor <- function(x, select) {
    convert <- select[sapply(x[, select], function(x) !is.factor(x))]
    n <- length(convert)
    if(n == 1) x[, convert] <- as.factor(x[, convert])
    else if(n > 1) {
        x[, convert] <- as.data.frame(lapply(x[, convert], as.factor))
    }
    x
}

# drop unused factor levels for factors in a data.frame
dropLevels <- function(x, select = names(x)) {
    check <- select[sapply(x[, select], function(x) is.factor(x))]
    n <- length(check)
    if(n == 1) x[, check] <- x[, check][, drop=TRUE]
    else if(n > 1) {
        x[, check] <- as.data.frame(lapply(x[, check], "[", drop=TRUE))
    }
    x
}

# create factor that includes NA
factorNA <- function(x, always = FALSE) {
    always <- isTRUE(always)
    if(is.factor(x)) {
        l <- levels(x)
#        if(NA %in% l) x
#        else if(always || any(is.na(x))) {
#            l <- c(l, NA)
#            factor(x, levels=c(levels(x), NA), exclude=c())
#        } else x
        if(NA %in% l || !(always || any(is.na(x)))) x
        else {
            l <- c(l, NA)
            factor(x, levels=c(levels(x), NA), exclude=c())
        }
    } else {
        if(always) {
            factor(c(NA, x), exclude=c())[-1]  # little trick
        } else factor(x, exclude=c())
    }
}


## get which observations contain NAs (and need to be excluded)
getExclude <- function(x, ...) UseMethod("getExclude")
getExclude.default <- function(x) which(is.na(x))
getExclude.data.frame <- function(x) unique(which(is.na(x), arr.ind=TRUE)[, 1])


## get breakpoints for categorizing continuous or semi-continuous variables
getBreaks <- function(x, weights = NULL, zeros = TRUE, 
        lower = NULL, upper = NULL, equidist = TRUE) {
    # initializations
    if(!is.numeric(x)) stop("'x' must be a numeric vector")
    if(!is.null(weights)) {
        if(!is.numeric(weights)) stop("'weights' must be a numeric vector")
        else if(length(weights) != length(x)) {
            stop("'weights' must have the same length as 'x'")
        }
    }
    zeros <- isTRUE(zeros)
    if(zeros) {
        pos <- which(x > 0)
        if(length(pos)) {
            if(isTRUE(equidist)) ppos <- seq(0.1, 1, by=0.1) 
            else ppos <- c(0.01, 0.05, 0.1, seq(0.2, 0.8, by=0.2), 0.9, 0.95, 0.99, 1)
            qpos <- quantileWt(x[pos], weights[pos], ppos)
        } else qpos <- NULL
        neg <- which(x < 0)
        if(length(neg)) {
            pneg <- seq(0, 0.9, by=0.1)
            qneg <- quantileWt(x[neg], weights[neg], pneg)
        } else qneg <- NULL
        breaks <- c(qneg, 0, qpos)
    } else {
        if(isTRUE(equidist)) p <- seq(0, 1, by=0.1) 
        else p <- c(0, 0.01, 0.05, 0.1, seq(0.2, 0.8, by=0.2), 0.9, 0.95, 0.99, 1)
        breaks <- quantileWt(x, weights, p)   
    }
    breaks <- unique(breaks)  # remove duplicated values
    if(!is.null(lower)) {
        if(!is.numeric(lower) || length(lower) == 0) {
            stop("'lower' is not numeric or has length 0")
        } else if(length(lower) > 1) lower <- lower[1]
        if(isTRUE(lower > breaks[1])) {
            warning("'lower' is larger than the smallest ", 
                "breakpoint and therefore disregarded")
        } else breaks[1] <- lower
    }
    if(!is.null(upper)) {
        if(!is.numeric(upper) || length(upper) == 0) {
            stop("'upper' is not numeric or has length 0")
        } else if(length(upper) > 1) upper <- upper[1]
        nb <- length(breaks)
        if(isTRUE(upper < breaks[nb])) {
            warning("'upper' is smaller than the largest ", 
                "breakpoint and therefore disregarded")
        } else breaks[nb] <- upper
    }
    breaks
}


## categorize continuous or semi-continuous variables
getCat <- function(x, breaks, zeros = TRUE, right = FALSE) {
    # initializations
    if(!is.numeric(x)) stop("'x' must be a numeric vector")
    checkBreaks(breaks)
    if(isTRUE(zeros)) {
        pos <- which(x > 0)
        neg <- which(x < 0)
        # positive values (also works if none exist)
        bpos <- c(0, breaks[breaks > 0])
        if(length(bpos) == 1) lpos <- NULL
        else {
            cpos <- cut(x[pos], bpos)
            lpos <- levels(cpos)
        }
        # negative values (also works if none exist)
        bneg <- c(breaks[breaks < 0], 0)
        if(length(bneg) == 1) lneg <- NULL 
        else {
            cneg <- cut(x[neg], bneg, right=FALSE)
            lneg <- levels(cneg)
        }
        # put it all together
        categories <- factor(ifelse(is.na(x), NA, 0), levels=c(lneg, 0, lpos))
        if(length(pos) > 0 && length(bpos) > 1) categories[pos] <- cpos
        if(length(neg) > 0 && length(bneg) > 1) categories[neg] <- cneg
        # return vector
        categories
    } else cut(x, breaks, include.lowest=TRUE, right=right)
}


## get name of categorized variable
getCatName <- function(name) paste(name, "Cat", sep="")


## truncated Pareto distribution
#truncPareto <- function(n, loc, scale, shape, lower, upper) {
#    # initializations
#    x <- numeric(n)
#    left <- n
#    new  <- n
#    # fit generalized pareto distribution with lower and upper bound
#    while(left > 0) {
#        xTmp <- rgpd(new, loc=loc, scale=scale, shape=shape)
#        ind  <- which(xTmp > lower & xTmp <= upper)
#        lind <- length(ind)
#        if(left < lind) {
#            ind <- ind[1:left]
#            lind <- length(ind)
#        }
#        x[(n - left + 1):(n - left + lind)]  <- xTmp[ind]
#        left <- left - lind
#        if(lind > 0) new  <- new / lind * left
#    }
#    return(x)
#}
truncPareto <- function(n, loc, scale, shape, lower, upper) {
    # initializations
    x <- numeric(n)
    left <- n
    # fit generalized pareto distribution with lower and upper bound
    while(left > 0) {
        xTmp <- rgpd(left, loc=loc, scale=scale, shape=shape)
        ind  <- which(xTmp > lower & xTmp <= upper)
        lind <- length(ind)
        if(lind > 0) {
            x[(n - left + 1):(n - left + lind)]  <- xTmp[ind]
            left <- left - lind
        }
    }
    return(x)
}


## logit regression (designed for internal use, hence no error handling)
logitreg <- function(x, y, weights = rep(1, length(y)), 
    intercept = TRUE, start = rep(0, p), ...) {
    # function to be minimized (log-likelihood)
    fmin <- function(beta, X, y, w) {
        p <- plogis(X %*% beta)
        -sum(2 * w * ifelse(y, log(p), log(1-p)))
    }
    # gradient
    gmin <- function(beta, X, y, w) {
        eta <- as.numeric(X %*% beta)
        p <- plogis(eta)
        -2 * (w * dlogis(eta) * ifelse(y, 1/p, -1/(1-p))) %*% X
    }
    # some preparations
    if(is.null(dim(x))) dim(x) <- c(length(x), 1)
    dn <- dimnames(x)[[2]]
    if(!length(dn)) dn <- paste("Var", 1:ncol(x), sep="")
    p <- ncol(x) + intercept
    if(intercept) {
        x <- cbind(1,x)
        dn <- c("(Intercept)", dn)
    }
    if(is.factor(y)) y <- (unclass(y) != 1)
    # optimize and return result
    fit <- optim(start, fmin, gmin, X = x, y = y, w = weights, method = "BFGS", ...)
    names(fit$par) <- dn
    return(fit)
}


## search for donor cells for splitting continuous variables
# i ........... indices corresponding to a zero of a table
# indDonors ... matrix containing indices of all non-zero elements
# donors ...... vector indices of the non-zero elements

# sequential minimum distance
seqMinDist <- function(i, indDonors, donors) {
    # initializations
    donor <- NA
    k <- 1
    # loop over dimensions to find donor cells with matching category
    while(is.na(donor) && k <= length(i)) {
        sel <- which(indDonors[, k] == i[k])  # donor cells in same category
        if(length(sel)) {
            tmpIndDonors <- indDonors[sel, , drop=FALSE]
            tmpDonors <- donors[sel]
            donor <- minDist(i[-k], tmpIndDonors[, -k, drop=FALSE], tmpDonors)
        }
        k <- k + 1
    }
    # if not successful, use donor with minimum overall distance
    if(is.na(donor)) donor <- minDist(i, indDonors, donors)
    donor
}

# minimum distance
minDist <- function(i, indDonors, donors) {
    # calculate distance from element defined by i for each element of m
    d <- colSums(abs(t(indDonors) - i))
    # return index of selected donor (minimum distance)
    donors[which.min(d)]
}



## weighted mean
meanWt <- function(x, weights, na.rm = TRUE) {
    na.rm <- isTRUE(na.rm)
    if(missing(weights)) mean(x, na.rm=na.rm)
    else weighted.mean(x, w=weights, na.rm=na.rm)
}


## weighted variance
varWt <- function(x, weights, na.rm = TRUE) {
    na.rm <- isTRUE(na.rm)
    if(missing(weights)) var(x, na.rm=na.rm)
    else {
        x <- as.numeric(x)
        weights <- as.numeric(weights)
        if(length(weights) != length(x)) {
            stop("'weights' must have the same length as 'x'")
        }
        if(na.rm) {
            select <- !is.na(x)
            x <- x[select]
            weights <- weights[select]
        }
        if(length(x) <= 1 || sum(weights > 0) <= 1) NA
        else sum((x - meanWt(x, weights))^2 * weights) / (sum(weights) - 1)
    }
}


## weighted covariance matrix

# generic function
covWt <- function(x, ...) UseMethod("covWt")

# default method
covWt.default <- function(x, y, weights, ...) {
    if(missing(y)) y <- x
    else if(length(x) != length(y)) {
        stop("'x' and 'y' must have the same length")
    }
    if(missing(weights)) cov(x, y, use = "complete.obs")
    else {
        if(length(weights) != length(x)) {
            stop("'weights' must have the same length as 'x' and 'y'")
        }
        select  <- !is.na(x) & !is.na(y) 
        x <- x[select]
        y <- y[select]
        weights <- weights[select]
        sum((x-meanWt(x, weights)) * (y-meanWt(y, weights)) * weights) / 
            (sum(weights)-1)
    }
}

# method for matrices
covWt.matrix <- function(x, weights, ...) {
    if(missing(weights)) cov(x, use = "pairwise.complete.obs")
    else {
        if(length(weights) != nrow(x)) {
            stop("length of 'weights' must equal the number of rows in 'x'")
        }
        center <- apply(x, 2, meanWt, weights=weights)
        x <- sweep(x, 2, center, check.margin = FALSE)
        crossprodWt(x, weights)
    }
}

# method for data.frames
covWt.data.frame <- function(x, weights, ...) covWt(as.matrix(x), weights)


## weighted correlation matrix

# generic function
corWt <- function(x, ...) UseMethod("corWt")

# default method
corWt.default <- function(x, y, weights, ...) {
    if(missing(y)) y <- x
    else if(length(x) != length(y)) {
        stop("'x' and 'y' must have the same length")
    }
    if(missing(weights)) cor(x, y, use = "complete.obs")
    else covWt(x, y, weights) / sqrt(varWt(x, weights) * varWt(y, weights))
}

# method for matrices
corWt.matrix <- function(x, weights, ...) {
    if(missing(weights)) cor(x, use = "pairwise.complete.obs")
    else {
        if(length(weights) != nrow(x)) {
            stop("length of 'weights' must equal the number of rows in 'x'")
        }
        cen <- apply(x, 2, meanWt, weights=weights)
        sc <- sqrt(apply(x, 2, varWt, weights=weights))
        x <- scale(x, center=cen, scale=sc)
        crossprodWt(x, weights)
    }
}

# method for data.frames
corWt.data.frame <- function(x, weights, ...) corWt(as.matrix(x), weights)


## weighted cross product
# designed for internal use, hence no error handling
# TODO: more efficient solution
crossprodWt <- function(x, weights) {
    ci <- 1:ncol(x)
    sapply(ci, function(j) sapply(ci, function(i) {
                    select <- !is.na(x[, i]) & !is.na(x[, j])
                    xi <- x[select, i]
                    xj <- x[select, j]
                    w <- weights[select]
                    sum(xi*xj*w) / (sum(w)-1)
                }))
}
