norm.plot <-
function(var,var.label,z,n.breaks,plot.new=TRUE)
{
  if (is.factor(var))
    return(NULL)
  if (plot.new)
    x11()
  if (is.null(var.label)) 
    var.label<-as.character(match.call()[2])
  var<-var[!is.na(var)]
  if (length(unique(var))<5){
    return(NULL)
    warning(paste("too few valid different values for variable",var.label))
  }
  mean<-mean(var,na.rm=T)
  sd<-sd(var,na.rm=T)
  Z.var<-(var-mean)/sd
  qqnorm<-qqnorm(Z.var,plot=F)
  dif<-qqnorm$y-qqnorm$x
  par(mfrow=c(2,2),mar=c(3,3,3,1),mgp=c(2,0.5,0),oma=c(3,0,3,0))
  hist(var,probability=TRUE,main="Histogram",xlab="",ylab="",col="light grey",breaks=n.breaks)
  dens<-dnorm(seq(min(var),max(var),len=1000),mean,sd)
  lines(seq(min(var),max(var),len=1000),dens)
  mtext(paste("mean=",format2(mean)," SD=",format2(sd)))
  box()
  qqnorm(var,datax=TRUE,pch=19,cex=0.09)
  qqline(var,datax=TRUE)
  boxplot(var,main="Boxplot",pch=19,cex=0.09,outline=T)
	plot(qqnorm$y*sd+mean,dif,pch=19,cex=0.01,xlab="",ylab="desv.",main="Standard deviation\nfrom normality")
	abline(h=c(-z,0,z),lty=c(2,4,2))
  p.sh<-shapiro.test(var)$p.value
  mtitle(var.label,lr="",lc=paste("Shapiro-Wilks p-value: ",format2(p.sh,3),sep=""),cex.m=1.5, cex.l=1)
}
