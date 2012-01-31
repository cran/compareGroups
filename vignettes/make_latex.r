rm(list=ls())
setwd("C:/programs/Dropbox/CompareGroups/package/compareGroups_without_odfWeave/compareGroups/vignettes")

Stangle("compareGroups_vignette.rnw")

Sweave("compareGroups_vignette")

shell("pdflatex compareGroups_vignette")
shell("bibtex compareGroups_vignette")
shell("pdflatex compareGroups_vignette")


file.remove("compareGroups_vignette.bbl")
file.remove("compareGroups_vignette.toc")
file.remove("compareGroups_vignette.out")
file.remove("compareGroups_vignette.log")
file.remove("compareGroups_vignette.aux")
file.remove("compareGroups_vignette.blg")


shell.exec("compareGroups_vignette.pdf")
