update.compareGroups<-
function (object, X., ..., evaluate = TRUE) 
{
    call <- attr(object,"call")$call
    if (is.null(call)) 
        stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(X.)) 
        call$X <- update.formula2(formula(object), X.)
    if (length(extras)) {
        existing <- !is.na(match(names(extras), names(call)))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate) 
        eval(call, parent.frame())
    else call
}