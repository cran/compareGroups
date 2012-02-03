norm.plot<-function(x, file, var.label.x, z, n.breaks)
{

  x<-x[!is.na(x)]
  if (length(unique(x))<5){
    return(NULL)
    warning(paste("too few valid different values for variable",var.label.x))
  }

  if (is.null(file))
    X11()
  else
    pdf(file)    

  mean<-mean(x,na.rm=T)
  sd<-sd(x,na.rm=T)
  Z.x<-(x-mean)/sd
  qqnorm<-qqnorm(Z.x,plot=F)
  dif<-qqnorm$y-qqnorm$x
  par(mfrow=c(2,2),mar=c(3,3,3,1),mgp=c(2,0.5,0),oma=c(3,0,3,0))
  hist(x,probability=TRUE,main="Histogram",xlab="",ylab="",col="light grey",breaks=n.breaks)
  dens<-dnorm(seq(min(x),max(x),len=1000),mean,sd)
  lines(seq(min(x),max(x),len=1000),dens)
  mtext(paste("mean=",format2(mean)," SD=",format2(sd)))
  box()
  qqnorm(x,datax=TRUE,pch=19,cex=0.09)
  qqline(x,datax=TRUE)
  boxplot(x,main="Boxplot",pch=19,cex=0.09,outline=T)
	plot(qqnorm$y*sd+mean,dif,pch=19,cex=0.01,xlab="",ylab="desv.",main="Standard deviation\nfrom normality")
	abline(h=c(-z,0,z),lty=c(2,4,2))
  p.sh<-try(shapiro.test(x)$p.value,silent=TRUE)
  if (inherits(p.sh,"try-error"))
    mtitle(paste("Normality plots of '",var.label.x,"'",sep=""),lr="",cex.m=1.5, cex.l=1)
  else
    mtitle(paste("Normality plots of '",var.label.x,"'",sep=""),lr="",lc=paste("Shapiro-Wilks p-value: ",format2(p.sh,3),sep=""),cex.m=1.5, cex.l=1)

  if (!is.null(file))
    dev.off()

}
