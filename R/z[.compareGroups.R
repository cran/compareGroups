"[.compareGroups"<-function(x,i,...){

    nn <- names(x)           
    nn.orig <- attr(x, "varnames.orig")

    if (is.integer(i))
      i <- i
    if (is.character(i)){
      if (all(i%in%nn))
        i <- which(nn%in%i)
      else{
        if (all(i%in%nn.orig))
          i <- which(nn.orig%in%i)
        else
          stop("some specified variables in subsetting don't exist")
      }
    }
    
    vars.orig <- attr(x, "varnames.orig")[i]
    X<-attr(x,"call")$call$X
    if (inherits(eval(X),"formula")){
        if (attr(x,"groups"))
          resp<-X[[2]]
        else
          resp<-NULL
        pred<-paste(vars.orig,collapse="+")
        formul<-as.formula(paste(resp,pred,sep="~"))
        y<-update(x,X=formul)
    } else {
      X <- eval(X)
      y <- update(x,X=X[,vars.orig])
    }
    class(y) <- "compareGroups"
    y

}
     