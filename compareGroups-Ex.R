pkgname <- "compareGroups"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
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
##D data(myData)
##D cGroupsGUI(myData)
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


data(myData)

# by formula
ans<-compareGroups(y~.,data=myData)
ans
summary(ans)
update(ans,y~.-a)

# by data.frame
X<-myData[,c("a","b","c")]
y<-myData[,"y"]
ans<-compareGroups(X,y)
ans
summary(ans)




cleanEx()
nameEx("createTable")
### * createTable

flush(stderr()); flush(stdout())

### Name: createTable
### Title: Table of descriptives by groups: bivariate table
### Aliases: createTable print.createTable
### Keywords: misc

### ** Examples


data(myData)

ans<-compareGroups(y~.,myData)
anstab<-createTable(ans)
anstab
update(anstab,show.n=TRUE)





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
##D data(myData)
##D ans<-compareGroups(y~.,myData)
##D export2csv(createTable(ans),file="c:/example/tables/table1")
## End(Not run)




cleanEx()
nameEx("export2latex")
### * export2latex

flush(stderr()); flush(stdout())

### Name: export2latex
### Title: Exporting descriptives table to Latex format
### Aliases: export2latex
### Keywords: utilities

### ** Examples


## Not run: 
##D data(myData)
##D ans<-compareGroups(y~.,myData)
##D export2latex(createTable(ans),file="c:/example/tables/table1")
## End(Not run)




cleanEx()
nameEx("myData")
### * myData

flush(stderr()); flush(stdout())

### Name: myData
### Title: Simulated data
### Aliases: myData
### Keywords: datasets

### ** Examples

data(myData)
head(myData)
summary(myData)



cleanEx()
nameEx("regicor")
### * regicor

flush(stderr()); flush(stdout())

### Name: regicor
### Title: REGICOR cross-sectional data
### Aliases: regicor
### Keywords: datasets

### ** Examples

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

data(myData)
label(myData$a)<-"variable a"
label(myData$b)<-"variable b"
label(myData$c)<-"variable c"
label(myData$y)<-"grouping variable"
ans<-compareGroups(y~.,data=myData)
createTable(ans)
varinfo(ans)



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
