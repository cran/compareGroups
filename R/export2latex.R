export2latex<-function(x, file, ...){

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
  ans<-NULL
  for (i in 1:length(varnames)){
    if (nr[i]==1){
      t.i<-desc[j,,drop=FALSE]
    } else{
      t.i<-rbind(rep(NA,ncol(desc)),desc[j:(j+nr[i]-1),])
      rownames(t.i)[1]<-paste(varnames[i],":",sep="")
      rownames(t.i)[-1]<-sub(varnames[i],"",rownames(t.i)[-1])
      rownames(t.i)[-1]<-sub(": ","$\\\\qquad$",rownames(t.i)[-1])
      t.i[1,ii:ncol(t.i)]<-t.i[2,ii:ncol(t.i)]
      t.i[2,ii:ncol(t.i)]<-rep(NA,length(ii:ncol(t.i))) 
    }
    ans<-rbind(ans,t.i)
    j<-j+nr[i]
  }



  ans<-ifelse(is.na(ans),"",ans)
  ans<-sub("%","\\\\%",ans)
  ans<-gsub("<","$<$",ans)
  ans<-gsub(">","$>$",ans)
  
  rownames(ans)<-sub("%","\\\\%",rownames(ans))  
  rownames(ans)<-gsub("_","\\\\_",rownames(ans))
  rownames(ans)<-gsub(">=","$\\\\geq$",rownames(ans))
  rownames(ans)<-gsub("<=","$\\\\leq$",rownames(ans))
  rownames(ans)<-gsub(">","$>$",rownames(ans))
  rownames(ans)<-gsub("<","$<$",rownames(ans))

  colnames(ans)<-sub("%","\\\\%",colnames(ans))  
  colnames(ans)<-gsub("_","\\\\_",colnames(ans))
  colnames(ans)<-gsub(">=","$\\\\geq$",colnames(ans))
  colnames(ans)<-gsub("<=","$\\\\leq$",colnames(ans))  
  colnames(ans)<-gsub(">","$>$",colnames(ans))
  colnames(ans)<-gsub("<","$<$",colnames(ans))
  
  colnames(ans)[ii:ncol(ans)]<-paste("\\multirow{2}{*}{",colnames(ans)[ii:ncol(ans)],"}",sep="")
  tex<-paste(paste(c("",colnames(ans)),collapse=" & "),"\\\\ \n")
  tex<-paste(tex, " & ", paste(paste("N=",Nmax,sep=""),collapse=" & "))
  tex <- paste(tex,  paste(rep(" & ",ncol(ans)-ii+1),collapse=""),"\\\\ \n")
  tex<-paste(tex,"\\hline \n")
  tex<-paste(tex,"\\hline \n")
  tex<-paste(tex,paste(rep(" & ",ncol(ans)),collapse=""),"\\\\ \n")
  for (i in 1:nrow(ans)){
    tex<-paste(tex,paste(paste(c(rownames(ans)[i],ans[i,]),collapse=" & "),"\\\\ \n"))
  }
  tex<-paste(tex,"\\hline \n")
  
  head.loc<-paste(c("l",rep("c",ncol(ans))),collapse="")
  
  caption<-paste("Summary descriptives table by groups of '",sub("\\$","\\\\$",attr(x,"yname")),"'",sep="")
  
  tex<-paste("
  \\begin{table}[ht]
  \\caption{",caption,"}
  \\begin{center}     
  \\begin{tabular}{",head.loc,"} 
  \\hline \n ",
  tex,
  "\\end{tabular} 
  \\end{center}   
  \\end{table}",sep="")
  
  #write(tex,ff<-tempfile())
  #file.show(ff)
  
  write(tex,file=paste(file,".tex",sep=""))

  # table 2 #

  avail<-x$avail
  print(xtable(avail,align=c("l",rep("c",ncol(avail)))),file=paste(file,"_appendix.tex",sep=""))

}

