KMg.plot <-
function(x, y, file, var.label.x, var.label.y)   
{

  kk<-!is.na(x) & !is.na(y)
  x<-x[kk]
  y<-y[kk]

  if (is.null(file))
    X11()
  else
    pdf(file) 

  plot(survfit(y~x),xlab="time",ylab="survival", mark.time=FALSE, lty=1:nlevels(x))
  title(main = paste("K-M survival of '",var.label.y,"' by '",var.label.x,"'", sep=""))
  legend("bottomleft",levels(x), lty=1:nlevels(x))
  
  if (!is.null(file))
    dev.off() 
     
}




