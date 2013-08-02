"[.cbind.createTable"<-function(x,i,...){
  nn <- names(x)           
  nn.orig <- attr(x, "varnames.orig")

  if (is.integer(i))
    i <- i
  if (is.character(i)){
    if (all(i%in%nn))
      i <- which(nn%in%i)
    else{
      if (all(i%in%nn.orig))
        i <- which(nn.orig%in%i)
      else
        stop("some specified variables in subsetting don't exist")
    }
  }

  y<-x
  for (kk in 1:length(y))
    y[[kk]]<-y[[kk]][i]
  attributes(y)<-attributes(x)
  y  
  
}