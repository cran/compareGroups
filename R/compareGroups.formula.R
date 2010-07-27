compareGroups.formula <-
function(X, data, subset, na.action=NULL, include.label=TRUE, ...){
    formula<-X
    call <- match.call()
    if (missing(data)) 
        data <- environment(formula)    
    frame.call <- call("model.frame", formula = X)     
    k = length(frame.call)
    for (i in c("data", "subset", "na.action", "drop.unused.levels")) {
        if (!is.null(call[[i]])) {
            frame.call[[i]] <- call[[i]]
            k <- k + 1
            if (is.R()) 
                names(frame.call)[k] = i
        }
    }
    if (is.null(frame.call$drop.unused.levels)) 
        frame.call$drop.unused.levels <- TRUE            
    if (is.null(frame.call$na.action)) 
        frame.call$na.action = na.pass
    m <- eval(frame.call, sys.parent())
    if (!all(names(m)%in%names(data)))
      stop("Invalid formula terms")    
    mt <- attr(m, "terms")
    pn <- attr(mt,"term.labels")
    if (!all(pn%in%names(data)))
      stop("Invalid formula terms")
    y <- m[,1]
    rv<-strsplit(as.character(mt)[3]," ")[[1]]
    rv<-trim(rv)
    rv<-sub("^\\(","",rv)
    rv<-sub("\\)$","",rv)
    if (rv[1]%in%names(data)){
      rv <- c("+",rv)
    }else{
      rv[1] <- trim(sub("^-","",rv[1]))
      rv <- c("-",rv)
    }
    pos<-neg<-integer()
    for (i in 1:(length(rv)/2)){
      if (rv[i*2-1]=='+')
        pos<-c(pos,which(names(data)==rv[i*2]))
      if (rv[i*2-1]=='-')
        neg<-c(neg,which(names(data)==rv[i*2]))
    }   
    kk<-pos[!pos%in%neg]
    if (!length(pos)>0)
      stop("no row-variables selected")
    X<-data[rownames(m),kk,drop=FALSE]
    ans <- compareGroups(X=X, y=y, include.label=include.label, ...)
    if (!is.null(attr(y,"label")) & include.label)
      attr(ans,"yname")<-attr(y,"label")
    else    
      attr(ans,"yname")<-names(m)[1]
    attr(ans,"call")<-list()
    attr(ans,"call")$call<-call
    if (any(names(call)=="subset")){
      nf<-as.character(call)
      nfs<-nf[which(names(call)=="subset")]
      for (i in 1:length(ans)){
        selec.i<-attr(ans[[i]],"selec")
        attr(ans[[i]],"selec")<-ifelse(is.na(selec.i),nfs,paste("(",nfs,") & (",selec.i,")",sep=""))
      }
    }
    attr(ans,"yname.orig")<-names(m)[1]
    attr(ans,"form")<-list()
    attr(ans,"form")$formula<-formula
    attr(ans,"form")$terms<-mt
    ans
}

