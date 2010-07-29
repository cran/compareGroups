table.i <-
function(x,hide.i,digits,type,varname,show.all,show.p.trend,show.p.mul,show.n){
  method<-attr(x,"method")
  if (is.na(digits))
    digits<-NULL
  if (is.na(type))
    type<-2
  pvals<-c(x$p.overall,x$p.trend,x$p.mul)
  names(pvals)[1:2]<-c("p.overall","p.trend")
  pvals<-format2(pvals,3)
  pvals<-ifelse(is.na(pvals),".",pvals)
  N<-x$sam[1]
  if (method[1]=="categorical"){
    nn<-x$descriptive
    nn<-ifelse(is.na(nn),".",nn)
    pp<-format2(x$prop,digits)
    pp<-ifelse(is.na(pp),".",pp)
    ans<-pp
    if (type==2)
      ans<-matrix(paste(nn," (",pp,"%)",sep=""),nrow=nrow(ans),ncol=ncol(ans))
    else  
      ans<-matrix(paste(pp,"%",sep=""),nrow=nrow(ans),ncol=ncol(ans))    
    colnames(ans)<-paste(varname,colnames(nn),sep=": ")
    rownames(ans)<-rownames(nn)
    ansp<-matrix(NA,nrow=length(pvals),ncol=ncol(ans))
    ansp[,1]<-pvals
    rownames(ansp)<-names(pvals)
    if (!is.na(hide.i)){
      if (hide.i==Inf)
        hide.i<-ncol(ans)
      if (ncol(ans)>1){
        if (hide.i==1)
          ansp[,2]<-pvals
        ans<-ans[,-hide.i,drop=FALSE]
        ansp<-ansp[,-hide.i,drop=FALSE]
      }
    }
    ans<-rbind(ans,ansp)
    ans<-rbind(ans,rep(NA,ncol(ans)))
    ans[nrow(ans),1]<-N
    rownames(ans)[nrow(ans)]<-"N"
  } else {
    nn<-x$descriptive
    if (method[1]=="no-data"){
      ans<-cbind(nn)
      ans<-ifelse(is.nan(ans),".",ans)
    } else {
      nn<-format2(x$descriptive,digits)
      nn<-ifelse(is.na(nn),".",nn)
      if (method[2]=="normal")
        ans<-cbind(apply(nn,1,function(y) paste(y[1]," (",y[2],")",sep="")))
      else
        ans<-cbind(apply(nn,1,function(y) paste(y[1]," [",y[2],"; ",y[3],"]",sep="")))
    }
    rn<-rownames(ans)
    ans<-cbind(c(ans,pvals,N))
    rownames(ans)[1:length(rn)]<-rn
    rownames(ans)[nrow(ans)]<-"N"
    colnames(ans)<-varname
  }
  ny<-nrow(x$descriptive)-1
  if (ny<3)
    show.p.trend <- show.p.mul <-FALSE
  if (!show.all)
    ans<-ans[-which(row.names(ans)=='[ALL]'),,drop=FALSE]
  if (!show.p.trend)
    ans<-ans[-which(row.names(ans)=='p.trend'),,drop=FALSE]
  if (!show.p.mul)
    ans<-ans[-which(row.names(ans)%in%names(x$p.mul)),,drop=FALSE]
  if (!show.n)
    ans<-ans[-nrow(ans),,drop=FALSE]
  ans
}

