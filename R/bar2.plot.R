bar2.plot<-function(x, y, file, var.label.x, var.label.y)     
{

  kk<-!is.na(x) & !is.na(y)
  x<-x[kk]
  y<-y[kk]

  if (is.null(file))
    X11()
  else
    pdf(file) 
    
  tt <- table(x, y)
  barplot(tt, beside=TRUE, main = paste("Barplot of '",var.label.x,"' by '",var.label.y,"'", sep=""),ylim=c(0,max(tt)*1.3),ylab="Freq") 
  legend("topleft",levels(x),fill=grey.colors(nlevels(x)),bty="n")


  if (!is.null(file))
    dev.off()  

}
