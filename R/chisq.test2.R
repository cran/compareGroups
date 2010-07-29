chisq.test2 <-
function(obj){
  if (any(dim(obj)<2) | is.null(dim(obj)))
    return(NaN)
  test<-try(chisq.test(obj))
  if (inherits(test,"try-error"))
    return(NaN)
  if (any(test$exp<5)){
    test <- try(fisher.test(obj))
    if (inherits(test,"try-error"))
      return(NaN)
  }
  ans <- test$p.value
  ans
}

