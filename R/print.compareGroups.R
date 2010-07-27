print.compareGroups <-
function(x, digits=3, ...) {
  if(!inherits(x, "compareGroups"))
   stop("argument 'x' must be of class 'compareGroups'")

  yname<-attr(x,"yname")
  pval<-unlist(lapply(x, FUN=function(y) y$p.overall))
  nn<-unlist(lapply(x, FUN=function(y) y$sam["[ALL]"]))
  method<-unlist(lapply(x, FUN=function(y) paste(attr(y,"method"),collapse=" ")))
  selec<-unlist(lapply(x, FUN=function(y) attr(y,"selec")))
  varnames<-names(x)
  sig.pval<-ifelse(pval<0.05,"**",ifelse(pval<0.1,"*",""))
  sig.pval<-ifelse(is.na(sig.pval),"",sig.pval)
  dd<-data.frame("var"=varnames,"N"=nn,"p.value"=pval,"method"=method,"selection"=I(selec))
  cat("\n\n-------- Summary of results by groups of '",yname,"'---------\n",sep="")
  dd$p.value<-format2(dd$p.value,digits)
  dd$p.value<-ifelse(is.na(dd$p.value),".",dd$p.value)
  dd$p.value<-paste(dd$p.value,sig.pval,sep="")
  dd$selection<-ifelse(is.na(dd$selection),"ALL",dd$selection)
  rownames(dd)<-NULL
  dd<-format(dd,justify="left")
  cat("\n\n")
  print(as.matrix(dd), na.print="", quote=FALSE)
  cat("-----\n")
  cat("Signif. codes:  0 '**' 0.05 '*' 0.01 ' ' 1 \n\n")
}

