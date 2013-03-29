### R code from vignette source 'PAPiPackage.Rnw'

###################################################
### code chunk number 1: LoadMetabolomicsData
###################################################
library(PAPi)
library(svDialogs)
data(metabolomicsData)
print(metabolomicsData)


###################################################
### code chunk number 2: LoadKeggLibrary
###################################################
data(keggLibrary)
print(keggLibrary)


###################################################
### code chunk number 3: UsingaddKeggCodes
###################################################
print(metabolomicsData)
print(keggLibrary)
AddedKegg <- addKeggCodes(
	metabolomicsData,
	keggLibrary,
	save = FALSE,
	addCodes = TRUE
)
print(AddedKegg)


###################################################
### code chunk number 4: ApplyingPAPi
###################################################
data(papiData)
print(papiData)
#papiResults <- papi(papiData, save = FALSE, offline = TRUE, localDatabase = "default")
data(papiResults)
head(papiResults)


###################################################
### code chunk number 5: papiResults
###################################################
data(papiResults)
head(papiResults)

###################################################
### code chunk number 6: papiHtest
###################################################
head(papiResults)
ApplyingHtest <- papiHtest(
	papiResults,
	save = FALSE,
	StatTest = "T"
)
head(ApplyingHtest)

###################################################
### code chunk number 7: papiLine
###################################################
head(papiResults)
papiLine(
	papiResults, 
	relative = TRUE, 
	setRef.cond = TRUE, 
	Ref.cond = "cond1", 
	save = FALSE
)