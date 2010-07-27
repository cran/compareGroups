compare.i <-
function(x, y, selec.i, method.i, alpha, min.dis, max.xlev, varname) {

  if (!is.factor(x) && !is.na(method.i) && method.i==3)
    x <- as.factor(x)
    
  if (!is.factor(x) & is.na(method.i) & length(unique(x))<min.dis){
    warning(paste("variable '",varname,"' converted to factor since few different values contained",sep=""))
    x <- as.factor(x)
  }    
  
  if (!is.na(selec.i)){
    x <- eval(parse(text=paste("x[",selec.i,"]",sep="")))
    y <- eval(parse(text=paste("y[",selec.i,"]",sep="")))
  }
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  ny<-length(levels(y))

  if (is.character(x))
    x <- as.factor(x)

  if (is.factor(x)){
    if (any(table(x)==0)){
      warning(paste("Some levels of '",varname,"' are removed since no observation in that/those levels",sep=""))
      x <- factor(x)
    }
  }

  if (!is.factor(x) & is.na(method.i) & length(unique(x))<min.dis){
    warning(paste("variable '",varname,"' converted to factor since few different values contained after possible subseting and NA removed",sep=""))
    x <- as.factor(x)
  }

  if (!is.factor(x) & is.na(method.i)){ # test for normality after subsetting!!!!!!
    sh <- try(shapiro.test(x))             # aquí fer un norm.plot!!??
    if (inherits(sh,"try-error"))
      method.i <- 2
    else       
      method.i <- ifelse(sh$p.value>alpha,1,2)
  }

  if (is.factor(x) & length(levels(x))>max.xlev)
    stop(paste("too many values for variable '",varname,"'",sep=""))

  if (length(x)==0){   # no data...
    sam<-rep(0,ny+1)
    names(sam)<-c("[ALL]",levels(y))
    nn<-matrix(NaN,ncol=1,nrow=ny+1)
    colnames(nn)<-"No data"
    rownames(nn)<-c("[ALL]",levels(y))
    if (ny<=2){
      p.mul<-NaN
    }else{
      p.mul <- rep(NaN, choose(ny,2))
      names(p.mul) <- paste("p",apply(combn2(levels(y)),2,paste,collapse="-"),sep=".")
    }
    ans<-list(descriptive=nn, sam=sam, p.overall=NaN, p.trend=NaN, p.mul=p.mul)
    attr(ans, "method") <- "no-data" 
  } else {
    if (is.factor(x)) { # Chi-squared test
      tt <- table(y,x) 
      nn <- rbind(table(x), tt)
      prop <- rbind(prop.table(table(x)),prop.table(tt,margin=1))
      colnames(prop)<-paste(colnames(prop),"%",sep="")
      rownames(nn)[1]<-rownames(prop)[1]<-"[ALL]"
      prop<-prop*100
      p.overall <- chisq.test2(tt)
      if (ny<=2){
        p.trend <- p.overall
        p.mul <- p.overall
        if (ny==2)
          names(p.mul) <- paste("p.",levels(y)[1],"-",levels(y)[2],sep="")
        else
          names(p.mul) <- paste("p.",levels(y)[1],"-",levels(y)[1],sep="") 
      } else {
        p.trend <- try(1-pchisq(cor(as.integer(x),as.integer(y))^2*(length(x)-1),1))
        if (inherits(p.trend,"try-error"))
          p.trend <- NaN      
        if (is.na(p.trend))
          p.trend <- NaN                             
        pp<-np<-NULL
        for (i in 1:(ny-1))
          for (j in (i+1):ny) {
            np<-c(np,paste(levels(y)[i],levels(y)[j],sep="-"))
            p.ij<-try(chisq.test2(tt[c(i,j),]))
            if (inherits(p.ij,"try-error"))
              p.ij<-NaN
            pp<-c(pp,p.ij)
          }    
        p.mul <- structure(p.adjust(pp,"BH"),names=paste("p",np,sep="."))
      }
      ans<-list(descriptive=nn, prop=prop, sam=rowSums(nn), p.overall=p.overall, p.trend=p.trend, p.mul=p.mul)
      attr(ans, "method") <- "categorical" 
    } else {
      x <- as.double(x)
      if (method.i == 1){ # parametric  # t-test
        tt<-descrip(x, y) # descriptive
        if (ny<=2) {
          p.overall<-try(t.test(x~y)$p.value)
          if (inherits(p.overall,"try-error"))
            p.overall<-NaN
          p.trend<-p.overall
          p.mul<-p.trend
          if (ny==2)
            names(p.mul) <- paste("p.",levels(y)[1],"-",levels(y)[2],sep="")
          else
            names(p.mul) <- paste("p.",levels(y)[1],"-",levels(y)[1],sep="")        
        } else {
          p.overall<-try(anova(lm(x~y),lm(x~1))[2,"Pr(>F)"])
          if (inherits(p.overall,"try-error"))
            p.overall<-NaN
          p.trend<-try(cor.test(x,as.integer(y))$p.value)
          if (inherits(p.trend,"try-error"))
            p.trend<-NaN 
          if (is.na(p.trend))
            p.trend <- NaN
          temp<-try(TukeyHSD(aov(x~y)))
          if (inherits(temp,"try-error")){
            p.mul <- rep(NaN, choose(ny,2))
            names(p.mul) <- paste("p",apply(combn2(levels(y)),2,paste,collapse="-"),sep=".")
          } else{
            temp<-t(temp[[1]][,4,drop=FALSE])
            temp<-structure(as.vector(temp),names=flip(colnames(temp)))
            p.mul<-structure(rep(NaN,choose(ny,2)),names=apply(combn(sort(levels(y)),2),2,paste,collapse="-"))
            p.mul[names(temp)]<-temp
            names(p.mul)<-paste("p",names(p.mul),sep=".")
          }        
        }
        ans<-list(descriptive=tt[,-1], sam=tt[,1], p.overall=p.overall, p.trend=p.trend, p.mul=p.mul)
        attr(ans, "method") <- c("continuous", "normal") 
      } else { # non-parametric   # kruskall-wallis
        tt<-descrip(x, y, method="no") # descriptive
        p.overall<-try(kruskal.test(x~y)$p.value)
        if (inherits(p.overall,"try-error"))
          p.overall<-NaN
        if (ny<=2){
          p.trend <- p.overall
          p.mul <- p.overall
          if (ny==2)
            names(p.mul) <- paste("p.",levels(y)[1],"-",levels(y)[2],sep="")
          else
            names(p.mul) <- paste("p.",levels(y)[1],"-",levels(y)[1],sep="")       
        } else {
          p.trend<-try(cor.test(x,as.integer(y),method="spearman")$p.value)
          if (inherits(p.trend,"try-error"))
            p.trend <- NaN        
          if (is.na(p.trend))
            p.trend <- NaN
          pp<-np<-NULL
          for (i in 1:(ny-1)){
            for (j in (i+1):ny) {
              np<-c(np,paste(levels(y)[i],levels(y)[j],sep="-"))
              p.ij<-try(kruskal.test(x~y,subset=y%in%c(levels(y)[i],levels(y)[j]))$p.value)
              if (inherits(p.ij,"try-error"))
                p.ij<-NaN            
            pp<-c(pp,p.ij)
            }
          }
          p.mul<-structure(p.adjust(pp,"BH"),names=paste("p",np,sep="."))
        }
        ans<-list(descriptive=tt[,-1], sam=tt[,1], p.overall=p.overall, p.trend=p.trend, p.mul=p.mul)
        attr(ans, "method") <- c("continuous", "non-normal")
      }
    }
  }
  attr(ans,"x")<-x
  attr(ans,"y")<-y
  attr(ans,"selec")<-selec.i
  ans
}

