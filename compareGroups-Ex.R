pkgname <- "compareGroups"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('compareGroups')

assign(".oldSearch", search(), pos = 'CheckExEnv')
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
nameEx("createTable")
### * createTable

flush(stderr()); flush(stdout())

### Name: createTable
### Title: Table of descriptives by groups: bivariate table
### Aliases: createTable print.createTable summary.createTable
###   print.summary.createTable
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
nameEx("varinfo")
### * varinfo

flush(stderr()); flush(stdout())

### Name: varinfo
### Title: Variable names and labels extraction
### Aliases: varinfo varinfo.compareGroups
### Keywords: utilities

### ** Examples

require(compareGroups)
data(regicor)
res<-compareGroups(sex ~ . ,regicor)
createTable(res, hide.no = 'no')
varinfo(res)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
