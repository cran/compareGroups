print.summary.compareGroups <-
function(x, digits=6,...) {
   if (!inherits(x,"summary.compareGroups"))
    stop("x must be of class 'summary.compareGroups'")
   nn<-names(x)
   yname<-attr(x,"yname")
   cat("\n --- Descriptives of each row-variable by groups of '",yname,"' ---\n", sep="")
   for (i in 1:length(x)) {
     cat("\n")
     cat("row-variable:", nn[i], "\n")
     cat("------------------- \n")
     x.i <- as.matrix(x[[i]])
     x.i <- ifelse(is.nan(x.i), ".", signifdec(x.i, digits=digits))
     x.i <- ifelse(x.i=="NA", "", x.i)
     print(x.i, na.print="", quote=FALSE)
  }
}

