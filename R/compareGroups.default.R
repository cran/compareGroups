compareGroups.default <-
function(X, y, selec=NA, method=1, alpha=0.05, min.dis=5, max.ylev=5, max.xlev=10, include.label=TRUE, ...) {

   if (!is.matrix(X) & !is.data.frame(X))
    stop("X must be a matrix or a data.frame")

   cl <- match.call()
   yname <- as.character(cl)[3]
    
   if (is.matrix(X))
    X <- as.data.frame(X) 
   
   nvars<-ncol(X)
   
   if (!is.null(attr(selec,"names"))){
    temp<-rep(NA,ncol(X))
    names(temp)<-names(X)
    if (!all(names(selec)%in%names(temp)))
      stop(paste("variables",paste(names(selec)[!names(selec)%in%names(temp)],collapse=", "),"specified in 'selec' not found"))
    temp[names(selec)]<-selec
    selec<-temp
   } else
     if (length(selec)==1) 
      selec=rep(selec,nvars)
   
   if (!is.null(attr(method,"names"))){
    temp<-rep(1,ncol(X))
    names(temp)<-names(X)
    if (!all(names(method)%in%names(temp)))
      stop(paste("variables",paste(names(method)[!names(method)%in%names(temp)],collapse=", "),"specified in 'method' not found"))
    temp[names(method)]<-method
    method<-temp
   } else 
     if (length(method)==1) 
       method=rep(method,nvars)

   varnames.orig<-names(X)
   yname.orig<-yname
   
   # PENDING: comprovar que y sigui un vector!!!!!

   names.X<-names(X)
   if (!is.null(attr(y,"label")) & include.label)
     yname<-attr(y,"label")
   nX <- lapply(X, function(x){
    nn<-attr(x,"label")  
    if (!is.null(nn) & include.label) 
      nn
    else
      NA
   })
   names.X<-ifelse(is.na(nX),names.X,nX)
   
   if (is.character(y))
    y <- as.factor(y)
    
   if (is.numeric(y))
    y <- as.factor(y)    

   if (!is.factor(y))
    stop("variable 'y' must be a factor")       # y must be a factor, numeric or a character

   tt<-table(y)
   if (any(tt==0)){ 
    y <- factor(y)
    warning("Some levels of y are removed since no observation in that/those levels")
   }

   ny <- length(levels(y))
   if (ny>max.ylev)
    stop(paste("number of groups must be less or equal to",max.ylev))

   if (nrow(X)!=length(y))
    stop("data doesn't mach")

   ans <- lapply(1:nvars, function(i) try(compare.i(X[,i],y=y, selec.i=selec[i], method.i=method[i], alpha=alpha, min.dis=min.dis, max.xlev=max.xlev, varname=names(X)[i])))
   
   names(ans)<-names.X    
   
   ww<-character()
   k<-1
   for (i in names(ans)){     
    if (inherits(ans[[i]],"try-error")){
      ans[[i]]<-NULL
      ww<-c(ww,names(X)[k])
    }
    k<-k+1
   }
   if (length(ans)==0)
    stop("None variable can be computed")
    
   if (length(ww)>0)
    warning(paste("Variables '",paste(ww,collapse="', '"),"' have been removed since some errors ocurred",sep=""))
  
   attr(ans,"yname")<-yname
   attr(ans,"call")<-list()
   attr(ans,"call")$call<-cl
   attr(ans,"ny")<-ny
   attr(ans,"varnames.orig")<-varnames.orig[which(!varnames.orig%in%ww)]
   attr(ans,"yname.orig")<-yname.orig
   class(ans)<-"compareGroups"
   ans
   
}

