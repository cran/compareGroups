prepare <- function(x, nmax)
{      

  varnames<-attr(x,"varnames")
  nr<-attr(x,"nr")
  
  desc<-x$desc
  avail<-x$avail
  
  nmax.pos<-attr(x,"nmax.pos")
  nmax.avail.pos<-NULL
  if (length(nmax.pos[[1]])==0 & length(nmax.pos[[2]])==0)
    nmax.avail.pos<-integer(0)
  if (length(nmax.pos[[1]])==0 & length(nmax.pos[[2]])>0)   
    nmax.avail.pos<-nmax.pos[[2]]+1
  if (length(nmax.pos[[1]])>0 & length(nmax.pos[[2]])==0)       
    nmax.avail.pos<-1
  if (length(nmax.pos[[1]])>0 & length(nmax.pos[[2]])>0)         
    nmax.avail.pos<-c(1,nmax.pos[[2]])
    
  if (length(nmax.avail.pos)>0 && nmax){
    Xlong<-attr(x,"Xlong")
    ylong<-attr(x,"ylong")
    uu<-unique(ylong)
    uu<-uu[!is.na(uu)]
    if (length(uu)>1){
      Nmax<-sum(!apply(is.na(Xlong[!is.na(ylong),,drop=FALSE]),1,all))
      Nmax<-c(Nmax,sapply(sort(uu), function(i) sum(!apply(is.na(subset(Xlong,ylong==i)),1,all))))
    } else {
      Nmax<-sum(!apply(is.na(Xlong),1,all))
    }  
    Nmax<-Nmax[nmax.avail.pos]
  }else{
    Nmax<-NULL
    nmax<-FALSE
  }  

  dd.pos <- attr(x,"dd.pos")

  j<-1
  table1<-NULL
  if (!is.null(attr(x,"caption")))
    cc<-character(0)
  for (i in 1:length(varnames)){
    if (nr[i]==1){
      t.i<-desc[j,,drop=FALSE]
    } else{
      t.i<-rbind(rep(NA,ncol(desc)),desc[j:(j+nr[i]-1),,drop=FALSE])
      rownames(t.i)[1]<-paste(varnames[i],":",sep="")
      rownames(t.i)[-1]<-sub(varnames[i],"",rownames(t.i)[-1],fixed = TRUE)
      rownames(t.i)[-1]<-sub(": ","    ",rownames(t.i)[-1])
      if (length(dd.pos)<ncol(t.i)){
        t.i[1,-dd.pos]<-t.i[2,-dd.pos]
        t.i[2,-dd.pos]<-NA
      }
    }
    table1<-rbind(table1,t.i)
    j<-j+nr[i]
    if (!is.null(attr(x,"caption"))){
      if (attr(x,"caption")[[i]]=="")
        cc<-c(cc,rep("",NROW(t.i)))
      else
        cc<-c(cc,attr(x,"caption")[[i]],rep("",NROW(t.i)-1))
    }
  }
  if (ncol(table1)==0)
    table1<-table1[-1,]
  
  if (nmax)
    table1<-rbind(colnames(table1),c(paste("N=",Nmax,sep=""),rep("",ncol(table1)-length(Nmax))),table1)
  else
    table1<-rbind(colnames(table1),table1)  
  table1<-ifelse(is.na(table1),"",table1)
  table1<-format(table1,justify="centre")    
  colnames(table1)<-rep("",ncol(table1))

  table2<-x[[2]]
  table2<-as.matrix(table2)
  table2<-ifelse(is.na(table2),"",table2)    
  table2<-rbind(colnames(table2),table2)
  table2[,-1]<-apply(table2[,-1,drop=FALSE],2,format,justify="centre")
  table2[,1]<-format(table2[,1],justify="left")
  colnames(table2)<-rep("",ncol(table2))

  out<-list(table1=table1,table2=table2)
  if (!is.null(attr(x,"caption")))
    attr(out,"cc")<-cc
  attr(out,"nmax")<-nmax
  out

}

