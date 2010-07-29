plot.compareGroups<-function(x, z=1.5 ,n.breaks="Sturges", ...){

  if (!inherits(x,"compareGroups"))
    stop("x must be of class 'compareGroups'")

  var.labels<-names(x)  
  for (i in 1:length(x))
    norm.plot(attr(x[[i]],"x"), var.label=var.labels[i], z=z, n.breaks=n.breaks)

}


