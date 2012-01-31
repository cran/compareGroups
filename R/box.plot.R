box.plot <-
function(x, y, file, var.label.x, var.label.y) 
{

  kk<-!is.na(x) & !is.na(y)
  x<-x[kk]
  y<-y[kk]

  if (length(unique(x))<5){
    return(NULL)
    warning(paste("too few valid different values for variable",var.label.x))
  }

  if (is.null(file))
    X11()
  else
    pdf(file) 
    
  boxplot(x ~ y, main = paste("Boxplot of '",var.label.x,"' by '",var.label.y,"'", sep=""))

  if (!is.null(file))
    dev.off()  


}
