cbind.createTable <- function(..., caption)
{

  cl<-match.call()
  list.names <- function(...) {
      deparse.level<-1
      l <- as.list(substitute(list(...)))[-1L]
      nm <- names(l)
      fixup <- if (is.null(nm)) 
          seq_along(l)
      else nm == ""
      dep <- sapply(l[fixup], function(x) switch(deparse.level + 1, "", if (is.symbol(x)) as.character(x) else "", 
          deparse(x, nlines = 1)[1L]))
      if (is.null(nm)) 
          dep
      else {
          nm[fixup] <- dep
          nm
      }
  }
  
  args<-list(...)  

  if (missing(caption))
    caption<-list.names(...)
  else{
    if (!is.null(caption))
      if (length(caption)!=length(args))
        stop("length of caption must be the number of 'createTable' objects to be combined")
  }

  cc<-unlist(lapply(args, function(x) !class(x)[1]%in%c("rbind.createTable","createTable")))
  if (any(cc))
    stop("arguments must be of class 'createTable' and cannot be of class 'cbind.createTable'")
    
  out<-args

  if (is.null(caption) || all(caption=='')) 
    caption=unlist(lapply(args,function(vv) ifelse(is.null(attr(vv,"yname")),"[No groups]",paste("By",attr(vv,"yname")))))
  
  attr(out,"caption")<-caption

  class(out)<-c("cbind.createTable","createTable")
  
  out

}