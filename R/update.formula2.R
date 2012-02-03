update.formula2<-
function (old, new, ...) 
{
    old <- as.formula(old)
    new <- as.formula(new)
    tmp <- .Internal(update.formula(old, new))
    if (length(new)==2 & length(old)==3)
      tmp[[2]]<-NULL
    out <- formula(terms.formula(tmp, simplify = FALSE))
    return(out)
}
