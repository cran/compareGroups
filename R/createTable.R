createTable <-
function(x,hide=NA,digits=NA,type=NA,show.all=TRUE,show.p.trend=FALSE,show.p.mul=FALSE,show.n=FALSE){

  if (!inherits(x,"compareGroups"))
    stop("x must be of class 'compareGroups'")

  cl<-match.call()

  if (!is.null(attr(digits,"names"))){
   temp<-rep(NA,length(x))
   names(temp)<-attr(x,"varnames.orig")
   if (!all(names(digits)%in%names(temp)))
     stop(paste("variables",paste(names(digits)[!names(digits)%in%names(temp)],collapse=", "),"specified in 'digits' not found"))
   temp[names(digits)]<-digits
   digits<-temp
  } else 
    if (length(digits)==1)
      digits<-rep(digits,length(x))


  if (!is.null(attr(hide,"names"))){
   temp<-rep(NA,length(x))
   names(temp)<-attr(x,"varnames.orig")
   if (!all(names(hide)%in%names(temp)))
     stop(paste("variables",paste(names(hide)[!names(hide)%in%names(temp)],collapse=", "),"specified in 'hide' not found"))
   temp[names(hide)]<-hide
   hide<-temp
  } else 
    if (length(hide)==1)
      hide<-rep(hide,length(x))
      
    
  ans<-list()
  ans$descr<-NULL
  ans$avail<-NULL
  varnames<-names(x)
  nr<-NULL
  for (i in 1:length(x)){
    t.i<-t(table.i(x[[i]],hide.i=hide[i],digits=digits[i],type=type,varname=varnames[i],show.all,show.p.trend,show.p.mul,show.n))
    nr<-c(nr,nrow(t.i))
    ans$descr<-rbind(ans$descr,t.i)
    s.i<-attr(x[[i]],"selec")
    s.i<-ifelse(is.na(s.i),"ALL",s.i)
    ans$avail<-rbind(ans$avail,c(x[[i]]$sam,paste(attr(x[[i]],"method"),collapse="-"),s.i))
  }
  rownames(ans$avail)<-varnames
  if (!show.all)
    ans$avail<-ans$avail[,-1]
  nc<-ncol(ans$avail)
  colnames(ans$avail)[(nc-1):nc]<-c("method","select")
  ans$call<-cl
  attr(ans,"yname")<-attr(x,"yname")
  attr(ans,"nr")<-nr
  attr(ans,"varnames")<-varnames
  attr(ans,"ny")<-attr(x,"ny")
  attr(ans,"show.all")<-show.all
  class(ans)<-"createTable"
  ans
}

