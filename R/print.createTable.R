print.createTable <-
function(x,...){
  if (!inherits(x,"createTable"))
    stop("x must be of class 'createTable'")
    
  if (!inherits(x,"createTable"))
    stop("x must be of class 'createTable'")
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
  table1<-NULL
  for (i in 1:length(varnames)){
    if (nr[i]==1){
      t.i<-desc[j,,drop=FALSE]
    } else{
      t.i<-rbind(rep(NA,ncol(desc)),desc[j:(j+nr[i]-1),])
      rownames(t.i)[1]<-paste(varnames[i],":",sep="")
      rownames(t.i)[-1]<-sub(varnames[i],"",rownames(t.i)[-1])
      rownames(t.i)[-1]<-sub(": ","    ",rownames(t.i)[-1])
      ii<-attr(x,"ny")+attr(x,"show.all")+1  
      t.i[1,ii:ncol(t.i)]<-t.i[2,ii:ncol(t.i)]
      t.i[2,ii:ncol(t.i)]<-rep(NA,length(ii:ncol(t.i))) 
    }
    table1<-rbind(table1,t.i)
    j<-j+nr[i]
  }
  
  table1<-rbind(colnames(table1),c(paste("N=",Nmax,sep=""),rep("",ncol(table1)-ii+1)),table1)
  table1<-ifelse(is.na(table1),"",table1)
  table1<-format(table1,justify="centre")    
  colnames(table1)<-rep("",ncol(table1))
    
  yname<-attr(x,"yname")
  cat("\n--------Summary descriptives table by '",yname,"'---------\n\n",sep="")
  print.default(table1,quote=FALSE,...)  

  # table 2
  cat("\n\n\n---Available data----\n\n")
  print.default(x[[2]],na.print="",quote=FALSE,...)  
}

