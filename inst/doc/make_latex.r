rm(list=ls())
setwd("C:/compareGroups/compareGroups/inst/doc")                 
treball<-"C:/compareGroups/compareGroups/inst/doc"
Sweave(file.path(treball,"compareGroups_vignette"))
shell(paste("pdflatex \"",file.path(treball,"compareGroups_vignette"),"\"",sep=""))
shell(paste("bibtex \"",file.path(treball,"compareGroups_vignette"),"\"",sep=""))
shell(paste("pdflatex \"",file.path(treball,"compareGroups_vignette"),"\"",sep=""))
shell(paste("pdflatex \"",file.path(treball,"compareGroups_vignette"),"\"",sep=""))
Stangle(file.path(treball,"compareGroups_vignette.Rnw"))  # creates chunks
shell.exec(file.path(treball,"compareGroups_vignette.pdf"))




