pkgname <- "compareGroups"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('compareGroups')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("cGroupsGUI")
### * cGroupsGUI

flush(stderr()); flush(stdout())

### Name: cGroupsGUI
### Title: Graphical user interface to create tables of descriptives by
###   groups (bivariate tables)
### Aliases: cGroupsGUI
### Keywords: utilities

### ** Examples

## Not run: 
##D data(regicor)
##D cGroupsGUI(regicor)
## End(Not run)



cleanEx()
nameEx("compareGroups")
### * compareGroups

flush(stderr()); flush(stdout())

### Name: compareGroups
### Title: Descriptives by groups
### Aliases: compareGroups compareGroups.default compareGroups.formula
###   print.compareGroups plot.compareGroups update.compareGroups
###   summary.compareGroups print.summary.compareGroups
### Keywords: misc

### ** Examples


require(compareGroups)   

# load REGICOR data
data(regicor)

# compute a time-to-cardiovascular event variable
regicor$tcv <- with(regicor, Surv(tocv, as.integer(cv=='Yes')))
label(regicor$tcv)<-"Cardiovascular"

# compute a time-to-overall death variable
regicor$tdeath <- with(regicor, Surv(todeath, as.integer(death=='Yes')))
label(regicor$tdeath) <- "Mortality"

# descriptives by sex
res <- compareGroups(sex ~ .-id-tocv-cv-todeath-death, data = regicor)
res

# summary of the first 4 row-variables
summary(res[1:4])

# univariate plots of all row-variables
## Not run: 
##D plot(res)
## End(Not run)

# plot of all row-variables by sex
## Not run: 
##D plot(res, bivar = TRUE)
## End(Not run)

# update changing the response: time-to-cardiovascular event.
# note that time-to-death must be removed since it is not possible 
# not compute descriptives of a 'Surv' class object by another 'Surv' class object.
update(res, tcv ~ . + sex - tdeath - tcv)






cleanEx()
nameEx("compareSNPs")
### * compareSNPs

flush(stderr()); flush(stdout())

### Name: compareSNPs
### Title: Summarise genetic data by groups.
### Aliases: compareSNPs print.compareSNPs
### Keywords: misc

### ** Examples


require(compareGroups)   

# load example from SNPassoc package
data(SNPs)

# visualize first rows
head(SNPs)

# select casco and all SNPs
myDat <- SNPs[,c(2,6:40)]

# QC of three SNPs by groups of cases and controls
res<-compareSNPs(casco ~ .-casco, myDat)
res

# QC of three SNPs of the whole data set
res<-compareSNPs( ~ .-casco, myDat)
res




cleanEx()
nameEx("createTable")
### * createTable

flush(stderr()); flush(stdout())

### Name: createTable
### Title: Table of descriptives by groups: bivariate table
### Aliases: createTable print.createTable summary.createTable
###   print.summary.createTable plot.createTable
### Keywords: misc

### ** Examples


require(compareGroups)

# load REGICOR data
data(regicor)

# compute a time-to-cardiovascular event variable
regicor$tcv <- with(regicor,Surv(tocv, as.integer(cv=='Yes')))
label(regicor$tcv)<-"Cardiovascular incidence"

# compute a time-to-overall death variable
regicor$tdeath <- with(regicor, Surv(todeath, as.integer(death=='Yes')))
label(regicor$tdeath) <- "Mortality incidence"

# remove variables used to create time-to variables
regicor<-remove.vars(regicor,c("todeath","death","tocv","cv"))

# descriptives by time-to-cardiovascular event, taking 'no' category as 
# the reference in computing HRs.
res <- compareGroups(tcv ~ .-id-tdeath, regicor, ref.no='no')

# build table showing HR and hiding the 'no' category
restab<-createTable(res, show.ratio = TRUE, hide.no = 'no')
restab

# prints available info table
summary(restab)

# Adds the 'available data' column
update(restab, show.n=TRUE)

# Descriptive of the entire cohort
update(restab, x = update(res, ~ . ))

# .. changing the response variable to sex
# Odds Ratios (OR) are displayed instead of Hazard Ratios (HR).
# note that now it is possible to compute descriptives by time-to-death 
# or time-to-cv but not the ORs . 
# We set timemax to 5 years, to report the probability of death and CV at 5 years:
update(restab, x = update(res, sex ~ . - sex + tdeath + tcv, timemax = 5*365.25))


## Combining tables:

# a) By rows: takes the first four variables as a group and the rest as another group:
rbind("First group of variables"=restab[1:4],"Second group of variables"=
  restab[5:length(res)])

# b) By columns: puts stratified tables by sex one beside the other:
res1<-compareGroups(year ~ . - id - sex, regicor)
restab1<-createTable(res1, hide.no = 'no')
restab2<-update(restab1, x = update(res1, subset = sex == 'Male'))
restab3<-update(restab1, x = update(res1, subset = sex == 'Female'))
cbind("ALL" = restab1, "MALES" = restab2, "FEMALES" = restab3)





cleanEx()
nameEx("export2csv")
### * export2csv

flush(stderr()); flush(stdout())

### Name: export2csv
### Title: Exporting descriptives table to plain text (CSV) format
### Aliases: export2csv
### Keywords: utilities

### ** Examples


## Not run: 
##D require(compareGroups)
##D data(regicor)
##D res <- compareGroups(sex ~. -id-todeath-death-tocv-cv, regicor)
##D export2csv(createTable(res, hide.no = 'n'), file="table1")
## End(Not run)




cleanEx()
nameEx("export2html")
### * export2html

flush(stderr()); flush(stdout())

### Name: export2html
### Title: Exporting descriptives table to HTML format
### Aliases: export2html
### Keywords: utilities

### ** Examples


## Not run: 
##D require(compareGroups)
##D data(regicor)
##D res <- compareGroups(sex ~. -id-todeath-death-tocv-cv, regicor)
##D export2html(createTable(res, hide.no = 'n'), file="table1")
## End(Not run)




cleanEx()
nameEx("export2latex")
### * export2latex

flush(stderr()); flush(stdout())

### Name: export2latex
### Title: Exporting descriptives table to LaTeX format
### Aliases: export2latex export2latex.createTable
###   export2latex.cbind.createTable
### Keywords: utilities

### ** Examples


## Not run: 
##D require(compareGroups)
##D data(regicor)
##D res <- compareGroups(sex ~. -id-todeath-death-tocv-cv, regicor)
##D export2latex(createTable(res, hide.no = 'n'), file="table1")
## End(Not run)




cleanEx()
nameEx("export2pdf")
### * export2pdf

flush(stderr()); flush(stdout())

### Name: export2pdf
### Title: Exports tables to PDF files.
### Aliases: export2pdf
### Keywords: utilities

### ** Examples


## Not run: 
##D 
##D require(compareGroups)
##D data(regicor)
##D 
##D  # example on an ordinary table
##D res <- createTable(compareGroups(year ~ . -id, regicor), hide = c(sex=1), hide.no = 'no')
##D export2pdf(res, "table", size="small")
##D 
## End(Not run)




cleanEx()
nameEx("missingTable")
### * missingTable

flush(stderr()); flush(stdout())

### Name: missingTable
### Title: Table of missingness counts by groups.
### Aliases: missingTable
### Keywords: utilities

### ** Examples


require(compareGroups)

# load regicor data
data(regicor)

# table of descriptives by recruitment yeear
res<-compareGroups(year~.-id-sex,regicor,subset=sex=='Male')
restab1 <- createTable(res, hide.no = "no")  # table for men
restab2 <- update(restab1, x = update(res, subset = sex == 'Female')) # table for women

# missingness table for men and for women
miss1<-missingTable(restab1,type=1)
miss2<-missingTable(restab2,type=1)
miss1
miss2

# sex stratified table of missingness.
cbind("Men"=miss1,"Women"=miss2)

# from a compareGroups object
missingTable(res)

## Not run: 
##D 
##D # some methods that works for createTable objects also works for objects 
##D #   computed by missTable function.
##D miss1[1:4]
##D varinfo(miss1)
##D plot(miss1)
##D 
##D #... but update methods cannot be applied (this returns an error).
##D update(miss1,type=2)
##D 
## End(Not run)





cleanEx()
nameEx("predimed")
### * predimed

flush(stderr()); flush(stdout())

### Name: predimed
### Title: PREDIMED randomized trial
### Aliases: predimed
### Keywords: datasets

### ** Examples

require(compareGroups)
data(predimed)
summary(predimed)



cleanEx()
nameEx("printTable")
### * printTable

flush(stderr()); flush(stdout())

### Name: printTable
### Title: 'Nice' table format.
### Aliases: printTable
### Keywords: utilities

### ** Examples


require(compareGroups)

data(regicor)

# example of the coefficients table from a linear regression
model <- lm(chol ~ age + sex + bmi, regicor)
results <- coef(summary(model))
results <- cbind(Var = rownames(results), round(results, 4))
printTable(results)

# or visualize the first rows of the iris data frame. 
# In this example, the first column is not treated as a row.names column and it is right justified.
printTable(head(iris), FALSE)

# the same example with columns centered
printTable(head(iris), FALSE, 'centre')





cleanEx()
nameEx("radiograph")
### * radiograph

flush(stderr()); flush(stdout())

### Name: radiograph
### Title: Lists the values in the data set.
### Aliases: radiograph
### Keywords: utilities

### ** Examples


## Not run: 
##D 
##D require(compareGroups)
##D 
##D # read example data of regicor in plain text format with variables separated by '\t'.
##D datafile <- system.file("exdata/regicor.txt", package="compareGroups")
##D radiograph(datafile)
##D 
## End(Not run)




cleanEx()
nameEx("regicor")
### * regicor

flush(stderr()); flush(stdout())

### Name: regicor
### Title: REGICOR cross-sectional data
### Aliases: regicor
### Keywords: datasets

### ** Examples

require(compareGroups)
data(regicor)
summary(regicor)



cleanEx()
nameEx("report")
### * report

flush(stderr()); flush(stdout())

### Name: report
### Title: Report of descriptive tables and plots.
### Aliases: report
### Keywords: utilities

### ** Examples


## Not run: 
##D 
##D require(compareGroups)
##D data(regicor)
##D 
##D  # example on an ordinary table
##D res <- createTable(compareGroups(year ~ . -id, regicor), hide = c(sex=1), hide.no = 'no')
##D report(res, "report" ,size="small", title="\Huge \textbf{REGICOR study}", 
##D        author="Isaac Subirana \\ IMIM-Parc de Salut Mar")
##D 
##D  # example on an stratified table by sex
##D res.men <- createTable(compareGroups(year ~ . -id-sex, regicor, subset=sex=='Male'), 
##D                        hide.no = 'no')
##D res.wom <- createTable(compareGroups(year ~ . -id-sex, regicor, subset=sex=='Female'), 
##D                        hide.no = 'no')
##D res <- cbind("Men"=res.men, "Wom"=res.wom)
##D report(res[[1]], "reportmen", size="small", 
##D         title="\Huge \textbf{REGICOR study \\ Men}", date="") # report for men / no date
##D report(res[[2]], "reportwom", size="small", 
##D         title="\Huge \textbf{REGICOR study \\ Women}", date="") # report for wom / no date
##D 
## End(Not run)




cleanEx()
nameEx("varinfo")
### * varinfo

flush(stderr()); flush(stdout())

### Name: varinfo
### Title: Variable names and labels extraction
### Aliases: varinfo varinfo.compareGroups varinfo.createTable
### Keywords: utilities

### ** Examples

require(compareGroups)
data(regicor)
res<-compareGroups(sex ~ . ,regicor)
createTable(res, hide.no = 'no')
varinfo(res)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
