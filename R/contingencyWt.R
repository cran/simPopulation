# ----------------------------------------
# Authors: Stefan Kraft and Andreas Alfons
#          Vienna University of Technology
# ----------------------------------------

contingencyWt <- function(x, y, weights = NULL) {
    tab <- tableWt(data.frame(x, y), weights)
    tab <- tab[rowSums(tab) > 0, ]
    chisq <- as.numeric(chisq.test(tab)$statistic)
    return(sqrt(chisq / (sum(tab) + chisq)))
}
