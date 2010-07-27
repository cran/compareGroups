export2csv<-function(x, file, sep=",", ...){

  if (!inherits(x,"createTable"))
    stop("x must be of class 'createTable'")

  if (sep!="," & sep!=";")
    stop("sep must be ',' or ';'")
    
  varnames<-attr(x,"varnames")
  nr<-attr(x,"nr")

  ii<-attr(x,"ny")+attr(x,"show.all")+1
  Navail<-apply(x$avail[,1:(ii-1),drop=FALSE],2,as.integer)
  if (nrow(x$avail[,1:(ii-1),drop=FALSE])==1)
    Nmax<-Navail
  if (ncol(x$avail[,1:(ii-1),drop=FALSE])==1)
    Nmax<-max(Navail)
  if (nrow(x$avail[,1:(ii-1),drop=FALSE])>1 & ncol(x$avail[,1:(ii-1),drop=FALSE])>1)
    Nmax<-apply(Navail,2,max) 
  
  # table 1 #
  desc<-x$desc
  j<-1
  ans<-NULL
  for (i in 1:length(varnames)){
    if (nr[i]==1){
      t.i<-desc[j,,drop=FALSE]
    } else{
      t.i<-rbind(rep(NA,ncol(desc)),desc[j:(j+nr[i]-1),])
      rownames(t.i)[1]<-paste(varnames[i],":",sep="")
      rownames(t.i)[-1]<-sub(varnames[i],"",rownames(t.i)[-1])
      rownames(t.i)[-1]<-sub(": ","    ",rownames(t.i)[-1])
      t.i[1,ii:ncol(t.i)]<-t.i[2,ii:ncol(t.i)]
      t.i[2,ii:ncol(t.i)]<-rep(NA,length(ii:ncol(t.i))) 
    }
    ans<-rbind(ans,t.i)
    j<-j+nr[i]                    
  }

  if (sep==",")
    write.csv(ans,file=paste(file,".csv",sep=""),na="",...)
  else 
    write.csv2(ans,file=paste(file,".csv",sep=""),na="",...)  

  # table 2 #
  avail<-x$avail
  if (sep==",")
    write.csv(avail,file=paste(file,"_appendix.csv",sep=""),na="",...)
  else
    write.csv2(avail,file=paste(file,"_appendix.csv",sep=""),na="",...)

}

