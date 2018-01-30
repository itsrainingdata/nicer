#' @export
nicer.run <- function(data,
                      lambdas = NULL,
                      blocks.lambda = 0.5,
                      error.tol = 1e-2,
                      max.iters = NULL,
                      alpha = 10,
                      verbose = FALSE
){
    nnode <- ncol(data$data)
    ccdr.run(data,
             sigmas = rep(1, nnode), ###
             lambdas = lambdas,
             blocks = -3,                     ###
             blocks.lambda = blocks.lambda,
             randomize = FALSE,               ###
             gamma = -1,                      ###
             error.tol = error.tol,
             max.iters = max.iters,
             alpha = alpha,
             verbose = verbose)
}
