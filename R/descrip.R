descrip <-
function(x, y, method="param") {
   n <- tapply(x, y, length)
   n[is.na(n)]<-0
   n.all <- length(x)
   if (method=="param") {
     mm <- tapply(x, y, mean)
     ss <- tapply(x, y, sd)
     mm.all <- mean(x)
     ss.all <- sd(x)
     ans <- cbind(n, mm, ss)
     ans <- rbind(c(n.all, mm.all, ss.all),ans) 
     colnames(ans) <- c("n", "mean", "sd")
     rownames(ans) <- c("[ALL]",levels(y))
   } else {
     med <- tapply(x, y, median)
     q1 <- tapply(x, y, quantile, prob=0.25)
     q3 <- tapply(x, y, quantile, prob=0.75)
     med.all <- median(x)
     q1.all <- quantile(x,prob=0.25)
     q3.all <- quantile(x,prob=0.75)
     ans<-cbind(n, med, q1, q3)
     ans <- rbind(c(n.all,med.all,q1.all,q3.all), ans)
     colnames(ans) <- c("n","med","Q1","Q3")
     rownames(ans) <- c("[ALL]",levels(y))
   }
   ans <- ifelse(is.na(ans),NaN,ans)
   ans
}

