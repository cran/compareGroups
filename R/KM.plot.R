KM.plot<-function(x, file, var.label.x)
{

  x<-x[!is.na(x)]

  if (is.null(file))
    X11()
  else
    pdf(file) 

  plot(survfit(x~1),xlab="time",ylab="survival", mark.time=FALSE)
  title(main = paste("K-M survival of '",var.label.x,"'", sep=""))
      
  if (!is.null(file))
    dev.off()  

} 
