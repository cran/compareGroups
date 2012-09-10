"[.createTable"<-function(x,i,...){
  if (inherits(x,"rbind.createTable"))
    stop("x cannot be of class rbind.createTable")
  cc<-x$call
  hide<-attr(x,"hide")[i]
  hide<-sapply(hide,function(x) if(is.character(x) && !is.na(x)) paste("'",x,"'",sep="") else x)
  digits<-attr(x,"digits")[i]
  digits.ratio<-attr(x,"digits.ratio")[i]
  hide<-paste("c(",paste(hide,collapse=","),")")
  digits<-paste("c(",paste(digits,collapse=","),")")  
  digits.ratio<-paste("c(",paste(digits.ratio,collapse=","),")")  
  assign(".xxx",attr(x,"x")[[1]][i],envir=.GlobalEnv)
  eval(parse(text=paste("update(x,x=.xxx,hide=",hide,",digits=",digits,")",sep="")))
}








