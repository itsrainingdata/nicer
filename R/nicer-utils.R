# compute L2 norm of vector
vnorm <- function(v){
    sqrt(sum(v*v))
}

# rescale matrix to have zero mean, unit norm columns
rescale <- function(x){
    xmeans <- apply(x, 2, mean)
    for(j in 1:ncol(x)){
        x[, j] <- x[, j] - xmeans[j]
    }

    xnorms <- apply(x, 2, vnorm)
    for(j in 1:ncol(x)){
        x[, j] <- x[, j] / xnorms[j]
    }

    x
}

#' @export
innerprod <- function(x){
    # check.numeric <- (col_classes(x) != "numeric")
    # if( any(check.numeric)){
    #     not.numeric <- which(check.numeric)
    #     stop(paste0("Input columns must be numeric! Columns ", paste(not.numeric, collapse = ", "), " are non-numeric."))
    # }

    if( any(dim(x) < 2)){
        stop("Input must have at least 2 rows and columns!") # 2-8-15: Why do we check this here?
    }

    x <- as.matrix(rescale(x))
    ip <- crossprod(x)

    ip
}

ip_to_vector <- function(ip){
    ip <- ip[upper.tri(ip, diag = TRUE)]

    ip
}

# compute the penalized least squares loss
loss <- function(x, y, coef, lambda, scale = FALSE){
    res <- x%*%coef - y
    loss <- sum(res*res)
    loss <- ifelse(scale, loss / nrow(x), loss) # if scale = TRUE, normalize LS term by 1/n
    pen <- lambda * sum(abs(coef))
    loss <- 0.5 * loss + pen

    loss
}
