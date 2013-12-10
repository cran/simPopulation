### R code from vignette source 'simPopulation-eusilc.Rnw'

###################################################
### code chunk number 1: simPopulation-eusilc.Rnw:72-80
###################################################
library("lattice")
lattice.options(default.theme = canonical.theme(color = FALSE))
sl <- trellis.par.get("superpose.line")
sl$col[1] <- "#A9A9A9"  # set first color to dark grey
rl <- trellis.par.get("reference.line")
rl$lty <- 2
trellis.par.set(superpose.line=sl, reference.line=rl)
options(width=75, prompt="R> ")


###################################################
### code chunk number 2: simPopulation-eusilc.Rnw:149-152
###################################################
library("simPopulation")
data("eusilcS")
seed <- 1234


###################################################
### code chunk number 3: simPopulation-eusilc.Rnw:221-226
###################################################
eusilcMP <- simEUSILC(eusilcS, upper = 200000, equidist = FALSE, 
    seed = seed)
eusilcTR <- simEUSILC(eusilcS, method = "twostep", seed = seed)
eusilcTN <- simEUSILC(eusilcS, method = "twostep", residuals = FALSE, 
    seed = seed)


###################################################
### code chunk number 4: simPopulation-eusilc.Rnw:264-266
###################################################
eusilcP <- simStructure(eusilcS, hid = "db030", w = "db090", 
    strata = "db040", additional = c("age", "rb090"))


###################################################
### code chunk number 5: simPopulation-eusilc.Rnw:272-277
###################################################
breaks <- c(min(eusilcS$age), seq(15, 80, 5), max(eusilcS$age))
eusilcS$ageCat <- as.character(cut(eusilcS$age, 
        breaks=breaks, include.lowest=TRUE))
eusilcP$ageCat <- as.character(cut(eusilcP$age, 
        breaks=breaks, include.lowest=TRUE))


###################################################
### code chunk number 6: simPopulation-eusilc.Rnw:286-289
###################################################
basic <- c("ageCat", "rb090", "hsize")
eusilcP <- simCategorical(eusilcS, eusilcP, w = "rb050", strata = "db040", 
    basic = basic, additional = c("pl030", "pb220a"))


###################################################
### code chunk number 7: simPopulation-eusilc.Rnw:316-322 (eval = FALSE)
###################################################
## abb <- c("B", "LA", "Vi", "C", "St", "UA", "Sa", "T", "Vo")
## nam <- c(rb090 = "Gender", db040 = "Region", hsize = "Household size")
## lab <- labeling_border(set_labels = list(db040 = abb), 
##     set_varnames = nam)
## spMosaic(c("rb090", "db040", "hsize"), "rb050", eusilcS, 
##     eusilcP, labeling = lab)


###################################################
### code chunk number 8: simPopulation-eusilc.Rnw:331-337 (eval = FALSE)
###################################################
## nam <- c(rb090 = "Gender", pl030 = "Economic status", 
##     pb220a = "Citizenship")
## lab <- labeling_border(abbreviate = c(FALSE, FALSE, TRUE), 
##     set_varnames = nam)
## spMosaic(c("rb090", "pl030", "pb220a"), "rb050", eusilcS, 
##     eusilcP, labeling = lab)


###################################################
### code chunk number 9: simPopulation-eusilc.Rnw:343-347
###################################################
abb <- c("B","LA","Vi","C","St","UA","Sa","T","Vo")
nam <- c(rb090="Gender", db040="Region", hsize="Household size")
lab <- labeling_border(set_labels=list(db040=abb), set_varnames=nam)
spMosaic(c("rb090", "db040", "hsize"), "rb050", eusilcS, eusilcP, labeling=lab)


###################################################
### code chunk number 10: simPopulation-eusilc.Rnw:349-352
###################################################
nam <- c(rb090="Gender", pl030="Economic status", pb220a="Citizenship")
lab <- labeling_border(abbreviate=c(FALSE, FALSE, TRUE), set_varnames=nam)
spMosaic(c("rb090", "pl030", "pb220a"), "rb050", eusilcS, eusilcP, labeling=lab)


###################################################
### code chunk number 11: simPopulation-eusilc.Rnw:373-387
###################################################
seedP <- .Random.seed
basic <- c(basic, "pl030", "pb220a")
eusilcMP <- simContinuous(eusilcS, eusilcP, w = "rb050", 
    strata = "db040", basic = basic, additional = "netIncome", 
    upper = 200000, equidist = FALSE, seed=seedP)
seedMP <- .Random.seed
eusilcTR <- simContinuous(eusilcS, eusilcP, w = "rb050", 
    strata = "db040", basic = basic, additional = "netIncome", 
    method="lm", seed=seedP)
seedTR <- .Random.seed
eusilcTN <- simContinuous(eusilcS, eusilcP, w = "rb050", 
    strata = "db040", basic = basic, additional = "netIncome", 
    method="lm", residuals=FALSE, seed=seedP)
seedTN <- .Random.seed


###################################################
### code chunk number 12: simPopulation-eusilc.Rnw:390-391
###################################################
options(width=66)


###################################################
### code chunk number 13: simPopulation-eusilc.Rnw:413-419 (eval = FALSE)
###################################################
## subset <- which(eusilcS[, "netIncome"] > 0)
## q <- quantileWt(eusilcS[subset, "netIncome"], 
##     eusilcS[subset, "rb050"], probs = 0.99)
## listP <- list(MP=eusilcMP, TR=eusilcTR, TN=eusilcTN)
## spCdfplot("netIncome", "rb050", dataS=eusilcS, dataP=listP, xlim=c(0, q))
## spBwplot("netIncome", "rb050", dataS=eusilcS, dataP=listP, pch="|")


###################################################
### code chunk number 14: simPopulation-eusilc.Rnw:425-429
###################################################
subset <- which(eusilcS[, "netIncome"] > 0)
q <- quantileWt(eusilcS[subset, "netIncome"], eusilcS[subset, "rb050"], 
    probs=0.99)
listP <- list(MP=eusilcMP, TR=eusilcTR, TN=eusilcTN)


###################################################
### code chunk number 15: simPopulation-eusilc.Rnw:431-432
###################################################
print(spCdfplot("netIncome", "rb050", dataS=eusilcS, dataP=listP, xlim=c(0, q)))


###################################################
### code chunk number 16: simPopulation-eusilc.Rnw:434-435
###################################################
print(spBwplot("netIncome", "rb050", dataS=eusilcS, dataP=listP, pch="|"))


###################################################
### code chunk number 17: simPopulation-eusilc.Rnw:455-463 (eval = FALSE)
###################################################
## spBwplot("netIncome", "rb050", "rb090", dataS=eusilcS, 
##     dataP=listP, pch="|", layout=c(1,2))
## spBwplot("netIncome", "rb050", "pb220a", dataS=eusilcS, 
##     dataP=listP, pch="|", layout=c(1,3))
## spBwplot("netIncome", "rb050", "db040", dataS=eusilcS, 
##     dataP=listP, pch="|", layout=c(1,9))
## spBwplot("netIncome", "rb050", "pl030", dataS=eusilcS, 
##     dataP=listP, pch="|", layout=c(1,7))


###################################################
### code chunk number 18: simPopulation-eusilc.Rnw:469-471
###################################################
print(spBwplot("netIncome", "rb050", "rb090", dataS=eusilcS, 
    dataP=listP, pch="|", layout=c(1,2)))


###################################################
### code chunk number 19: simPopulation-eusilc.Rnw:473-475
###################################################
print(spBwplot("netIncome", "rb050", "pb220a", dataS=eusilcS, 
    dataP=listP, pch="|", layout=c(1,3)))


###################################################
### code chunk number 20: simPopulation-eusilc.Rnw:477-479
###################################################
print(spBwplot("netIncome", "rb050", "db040", dataS=eusilcS, 
    dataP=listP, pch="|", layout=c(1,9)))


###################################################
### code chunk number 21: simPopulation-eusilc.Rnw:481-483
###################################################
print(spBwplot("netIncome", "rb050", "pl030", dataS=eusilcS, 
    dataP=listP, pch="|", layout=c(1,7)))


###################################################
### code chunk number 22: simPopulation-eusilc.Rnw:503-509
###################################################
breaks <- getBreaks(eusilcS$netIncome, eusilcS$rb050, 
    upper = Inf, equidist = FALSE)
eusilcS$netIncomeCat <- getCat(eusilcS$netIncome, breaks)
eusilcMP$netIncomeCat <- getCat(eusilcMP$netIncome, breaks)
eusilcTR$netIncomeCat <- getCat(eusilcTR$netIncome, breaks)
eusilcTN$netIncomeCat <- getCat(eusilcTN$netIncome, breaks)


###################################################
### code chunk number 23: simPopulation-eusilc.Rnw:520-531
###################################################
components <- c("py010n", "py050n", "py090n", 
        "py100n", "py110n", "py120n", "py130n", "py140n")
eusilcMP <- simComponents(eusilcS, eusilcMP, w = "rb050", 
    total = "netIncome", components = components, 
    conditional = c("netIncomeCat", "pl030"), seed = seedMP)
eusilcTR <- simComponents(eusilcS, eusilcTR, w = "rb050", 
    total = "netIncome", components = components, 
    conditional = c("netIncomeCat", "pl030"), seed=seedTR)
eusilcTN <- simComponents(eusilcS, eusilcTN, w = "rb050", 
    total = "netIncome", components = components, 
    conditional = c("netIncomeCat", "pl030"), seed=seedTN)


###################################################
### code chunk number 24: simPopulation-eusilc.Rnw:541-544 (eval = FALSE)
###################################################
## listP <- list(MP=eusilcMP, TR=eusilcTR, TN=eusilcTN)
## spBwplot(components, "rb050", dataS=eusilcS, 
##     dataP=listP, pch="|", minRatio=0.2, layout=c(2,4))


###################################################
### code chunk number 25: simPopulation-eusilc.Rnw:550-551
###################################################
listP <- list(MP=eusilcMP, TR=eusilcTR, TN=eusilcTN)


###################################################
### code chunk number 26: simPopulation-eusilc.Rnw:553-555
###################################################
print(spBwplot(components, "rb050", dataS=eusilcS, 
    dataP=listP, pch="|", minRatio=0.2, layout=c(2,4)))


