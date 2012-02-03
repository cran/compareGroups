bar.plot <-
function(x, file, var.label.x)
{
  x<-x[!is.na(x)]
  
  if (is.null(file))
    X11()
  else
    pdf(file)    

  barplot(table(x),main=paste("Barplot of '",var.label.x,"'",sep=""),ylab="Freq")

  if (!is.null(file))
    dev.off()

}
