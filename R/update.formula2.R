update.formula2<-
function (old, new, ...) 
{
    tmp <- .Internal(update.formula(as.formula(old), as.formula(new)))
    out <- formula(terms.formula(tmp, simplify = FALSE))
    return(out)
}
