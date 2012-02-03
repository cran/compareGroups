summary.createTable <-
function(object, ...) {
  if(!inherits(object, "createTable"))
    stop("'object' must be of class 'createTable'")
  ans <- object
  class(ans) <- c("summary.createTable",class(ans))
  ans
}