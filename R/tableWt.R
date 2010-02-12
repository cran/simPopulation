# ----------------------------------------
# Authors: Stefan Kraft and Andreas Alfons
#          Vienna University of Technology
# ----------------------------------------

tableWt <- function(x, weights = NULL) {
    # initializations
    if(!is.data.frame(x)) x <- as.data.frame(x)
    if(is.null(weights)) return(table(x))
    else if(!is.numeric(weights)) stop("'weights' must be a numeric vector")
    else if(length(weights) != nrow(x)) {
        stop("length of 'weights' must equal the number of rows in 'x'")
    } else if(!all(is.finite(weights))) stop("missing or infinite weights")
    # compute and return weighted table
    tab <- round(tapply(weights, x, sum))
    tab[is.na(tab)] <- 0
    class(tab) <- "table"
    return(tab) 
}
