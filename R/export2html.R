export2html<-function(x, file, which.table="descr", nmax = TRUE, ...){

  if (!inherits(x,"createTable"))
    stop("x must be of class 'createTable'")

  ww <- charmatch(which.table, c("descr","avail","both"))
  if (is.na(ww))
    stop(" argument 'which.table' must be either 'descr', 'avail' or 'both'")

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
      Nmax<-apply(avail[,nmax.avail.pos,drop=FALSE],2,max)  
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
      rownames(t.i)[-1]<-sub(": ","&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",rownames(t.i)[-1])
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
    colnames(table1)[1:length(Nmax)]<-paste(colnames(table1)[1:length(Nmax)]," <br/> N=",Nmax,sep="")
 
  table1<-cbind(rownames(table1),table1)
  
  if (!is.null(attr(x,"caption"))){
    table1[,1]<-paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",table1[,1])
    aux<-NULL
    for (i in 1:nrow(table1)){
      if (cc[i]!=""){
        aux<-rbind(aux,c(cc[i],rep("",ncol(table1)-1)))
        aux<-rbind(aux,table1[i,])
      }else {
        aux<-rbind(aux,table1[i,])      
      }
    }
    table1<-aux
  }

  table2<-avail
  table2<-cbind(rownames(table2),table2)
  
  if (!is.null(attr(x,"caption"))){
    cc<-attr(x,"caption")
    table2[,1]<-paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",table2[,1])
    aux<-NULL
    for (i in 1:nrow(table2)){
      if (cc[i]!=""){
        aux<-rbind(aux,c(cc[i],rep("",ncol(table2)-1)))
        aux<-rbind(aux,table2[i,])
      }else {
        aux<-rbind(aux,table2[i,])      
      }
    }
    table2<-aux
  }
  
  if (ww%in%c(1,3)){
    align<-c("l","l",rep("c",ncol(table1)-1))
    colnames(table1)[1]<-"Var"  
    print(xtable(table1,align=align),type="html",file=paste(file,".html",sep=""),include.rownames=FALSE,sanitize.text.function=function(x) x)
  }

  if (ww%in%c(2,3)){
    colnames(table2)[1]<-"Var"
    align<-c("l","l",rep("c",ncol(table2)-1))
    print(xtable(table2,align=align),type="html",file=paste(file,"_appendix.html",sep=""),include.rownames=FALSE,sanitize.text.function=function(x) x)
  }
  
}
