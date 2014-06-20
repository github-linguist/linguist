
df.residual.mira <- function(object, ...) {
    fit <- object$analyses[[1]]
    return(df.residual(fit))
}

df.residual.lme <- function(object, ...) {
    return(object$fixDF[["X"]][1])
}

df.residual.mer <- function(object, ...) {
    return(sum(object@dims[2:4] * c(1, -1, -1)) + 1)
}

df.residual.default <- function(object, q = 1.3, ...) {
    df <- object$df.residual
    if (!is.null(df)) 
        return(df)
    
    mk <- try(c <- coef(object), silent = TRUE)
    mn <- try(f <- fitted(object), silent = TRUE)
    if (inherits(mk, "try-error") | inherits(mn, "try-error")) 
        return(NULL)
    n <- ifelse(is.data.frame(f) | is.matrix(f), nrow(f), length(f))
    k <- length(c)
    if (k == 0 | n == 0) 
        return(NULL)
    return(max(1, n - q * k))
}
