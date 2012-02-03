chisq.test2 <-
function(obj){
  if (any(dim(obj)<2) | is.null(dim(obj)))
    return(NaN)
  expect<-outer(rowSums(obj),colSums(obj))/sum(obj)    
  if (any(expect<5)){
    test <- try(fisher.test(obj),silent=TRUE)
  } else {
    test <- try(chisq.test(obj),silent=TRUE)
  }
  if (inherits(test,"try-error")){
    test<-try(chisq.test(obj, simulate.p.value=TRUE),silent=TRUE)  
  }
  if (inherits(test,"try-error"))
    return(NaN)
  ans <- test$p.value
  ans
}

