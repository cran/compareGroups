descripSurv <-
function(x, y, timemax)
{

  nn<-by(!is.na(x[,1]),y,sum)
  nn[is.na(nn)]<-0
  oo<-by(x[,2]==1,y,sum,na.rm=TRUE)
  
  ss<-try(summary(survfit(x~y)),silent=TRUE)

  if (inherits(ss,"try-error")){
    Pmax.i<-rep(NaN,nlevels(y))
  }else{
    SS<-ss$surv
    TT<-ss$time
    if (!is.null(ss$strata)){
      GG<-ss$strata
      GG<-sub("^y=","",as.character(GG))
      gg<-unique(GG)
    } else {
      gg<-names(table(y)[table(y)>0])
      GG<-rep(gg,length(SS))
    }
    Pmax.i<-rep(NA,nlevels(y))
    Pmax.i[oo==0]<-0
    names(Pmax.i)<-levels(y)
    for (i in 1:length(gg)){
      ww<-grep(gg[i],levels(y),fixed=TRUE)
      SS.temp<-SS[GG==gg[i]]
      TT.temp<-TT[GG==gg[i]]
      if (any(TT.temp>=timemax)){
        Smax<-SS.temp[TT.temp>=timemax][1]
      }else{
        Smax<-SS.temp[length(SS.temp)]
      }
      Pmax.i[ww]<-1-Smax
    }
  }  

  nn.all<-sum(nn,na.rm=TRUE)

  ss.all<-try(summary(survfit(x~1)),silent=TRUE)

  if (inherits(ss.all,"try-error")){
    Pmax.all<-NaN
  } else {
    SS.all<-ss.all$surv
    TT.all<-ss.all$time
    if (any(TT.all>=timemax)){
      Smax.all<-SS.all[TT.all>=timemax][1]
    }else{
      Smax.all<-SS.all[length(SS.all)]  
    }
    if (nn.all==0) {
      Pmax.all<-NA
    } else {
      if (sum(oo[i],na.rm=TRUE)==0){
        Pmax.all<-0
      } else {
        Pmax.all<-1-Smax.all
      }
    }
  }
  ans<-cbind(c(nn.all,nn),c(Pmax.all,Pmax.i)*100)
  colnames(ans) <- c("n", "inc")
  rownames(ans) <- c("[ALL]",levels(y))  
  ans
  
}