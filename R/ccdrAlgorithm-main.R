#
#  ccdr-main-R.R
#  ccdrAlgorithm
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE CCDRALGORITHM: Main CCDr methods
#
#   CONTENTS:
#     ccdr.run
#     ccdr_call
#     ccdr_gridR
#     ccdr_singleR
#

###--- These two lines are necessary to import the auto-generated Rcpp methods in RcppExports.R---###
#' @useDynLib Rccdr2
#' @importFrom Rcpp sourceCpp
NULL

#' @export
matrix2blocks <- function(x, lower = FALSE, diag = FALSE){
    stopifnot(sparsebnUtils::check_if_matrix(x))
    stopifnot(nrow(x) == ncol(x)) # enforce that input is adj matrix

    ### Pull out (i,j) indices of nonzero entries
    blocks <- Matrix::which(as.matrix(x) != 0, arr.ind = T)

    ### Remove diagonal entries
    if(!diag){
        blocks <- blocks[which(blocks[,1] != blocks[,2]), ]
    }

    ### Remove duplicate blocks by enforcing i < j
    if(lower){
        blocks <- blocks[which(blocks[,1] < blocks[,2]), ]
    }

    blocks
}

#' @export
allBlocks <- function(nodes){
    blocks <- lapply(nodes, function(x){
            # Allow all off-diagonal entries since we are no longer using the block decomposition
            row <- (nodes)[nodes != x]
            col <- rep(x, length(col))
            cbind(row, col)
        })
    blocks <- do.call("rbind", blocks)

    blocks
}

#' Main CCDr Algorithm
#'
#' Estimate a Bayesian network (directed acyclic graph) from observational data using the
#' CCDr algorithm as described in \href{http://jmlr.org/papers/v16/aragam15a.html}{Aragam and Zhou (2015)}.
#'
#' Instead of producing a single estimate, this algorithm computes a solution path of estimates based
#' on the values supplied to \code{lambdas} or \code{lambdas.length}. The CCDr algorithm approximates
#' the solution to a nonconvex optimization problem using coordinate descent. Instead of AIC or BIC,
#' CCDr uses continuous regularization based on concave penalties such as the minimax concave penalty
#' (MCP).
#'
#' This implementation includes two options for the penalty: (1) MCP, and (2) L1 (or Lasso). This option
#' is controlled by the \code{gamma} argument.
#'
#' @param data Data as \code{\link[sparsebnUtils]{sparsebnData}}. Must be numeric and contain no missing values.
#' @param betas Initial guess for the algorithm. Represents the weighted adjacency matrix
#'              of a DAG where the algorithm will begin searching for an optimal structure.
#' @param lambdas (optional) Numeric vector containing a grid of lambda values (i.e. regularization
#'                parameters) to use in the solution path. If missing, a default grid of values will be
#'                used based on a decreasing log-scale  (see also \link{generate.lambdas}).
#' @param lambdas.length Integer number of values to include in the solution path. If \code{lambdas}
#'                       has also been specified, this value will be ignored. Note also that the final
#'                       solution path may contain fewer estimates (see
#'                       \code{alpha}).
#' @param gamma Value of concavity parameter. If \code{gamma > 0}, then the MCP will be used
#'              with \code{gamma} as the concavity parameter. If \code{gamma < 0}, then the L1 penalty
#'              will be used and this value is otherwise ignored.
#' @param error.tol Error tolerance for the algorithm, used to test for convergence.
#' @param max.iters Maximum number of iterations for each internal sweep.
#' @param alpha Threshold parameter used to terminate the algorithm whenever the number of edges in the
#'              current DAG estimate is \code{> alpha * ncol(data)}.
#' @param verbose \code{TRUE / FALSE} whether or not to print out progress and summary reports.
#'
#' @return A \code{\link[sparsebnUtils]{sparsebnPath}} object.
#'
#' @examples
#'
#' \dontrun{
#'
#' ### Generate some random data
#' dat <- matrix(rnorm(1000), nrow = 20)
#' dat <- sparsebnData(dat, type = "continuous")
#'
#' # Run with default settings
#' ccdr.run(data = dat)
#'
#' ### Optional: Adjust settings
#' pp <- ncol(dat)
#'
#' # Initialize algorithm with a random initial value
#' init.betas <- matrix(0, nrow = pp, ncol = pp)
#' init.betas[1,2] <- init.betas[1,3] <- init.betas[4,2] <- 1
#'
#' # Run with adjusted settings
#' ccdr.run(data = dat, betas = init.betas, lambdas.length = 10, alpha = 10, verbose = TRUE)
#' }
#'
#' @export
ccdr.run <- function(data,
                     betas,
                     sigmas = NULL,
                     lambdas = NULL,
                     lambdas.length = NULL,
                     blocks = NULL,
                     blocks.lambda = 0.5,
                     randomize = FALSE,
                     gamma = 2.0,
                     error.tol = 1e-2,
                     max.iters = NULL,
                     alpha = 10,
                     verbose = FALSE
){
    ### Check data format
    if(!sparsebnUtils::is.sparsebnData(data)) stop(sparsebnUtils::input_not_sparsebnData(data))

    ### Extract the data (CCDr only works on observational data, so ignore the intervention part)
    data_matrix <- data$data

    ### Call the CCDr algorithm
    ccdr_call(data = data_matrix,
              betas = betas,
              sigmas = sigmas,
              lambdas = lambdas,
              lambdas.length = lambdas.length,
              gamma = gamma,
              error.tol = error.tol,
              rlam = NULL,
              max.iters = max.iters,
              alpha = alpha,
              blocks = blocks,
              blocks.lambda = blocks.lambda,
              randomize = randomize,
              verbose = verbose)
} # END CCDR.RUN

# ccdr_call
#
#   Handles most of the bookkeeping for CCDr. Sets default values and prepares arguments for
#    passing to ccdr_gridR and ccdr_singleR. Some type-checking as well, although most of
#    this is handled internally by ccdr_gridR and ccdr_singleR.
#
ccdr_call <- function(data,
                      betas,
                      sigmas,
                      lambdas,
                      lambdas.length,
                      gamma,
                      error.tol,
                      rlam,
                      max.iters,
                      alpha,
                      blocks,
                      blocks.lambda,
                      randomize,
                      verbose = FALSE
){
#     ### Allow users to input a data.frame, but kindly warn them about doing this
#     if(is.data.frame(data)){
#         warning(sparsebnUtils::alg_input_data_frame())
#         data <- sparsebnUtils::sparsebnData(data)
#     }
#
#     ### Check data format
#     if(!sparsebnUtils::is.sparsebnData(data)) stop(sparsebnUtils::input_not_sparsebnData(data))
#
#     ### Extract the data (CCDr only works on observational data, so ignore the intervention part)
#     data_matrix <- data$data

    ### Check data format
    if(!sparsebnUtils::check_if_data_matrix(data)) stop("'data' argument must be a data.frame or matrix!")

    # Could use check_if_complete_data here, but we avoid this in order to save a (small) amount of computation
    #  and give a more informative error message
    num_missing_values <- sparsebnUtils::count_nas(data)
    if(num_missing_values > 0) stop(sprintf("%d missing values detected!", num_missing_values))

    ### Get the dimensions of the data matrix
    nn <- as.integer(nrow(data))
    pp <- as.integer(ncol(data))

    ### Set default for sigmas (negative values => ignore initial value and update as usual)
    if(is.null(sigmas)){
        sigmas <- rep(-1., pp)
    }

    ### Use default values for lambda if not specified
    if(is.null(lambdas)){
        if(is.null(lambdas.length)){
            stop("Both lambdas and lambdas.length unspecified: Must specify a value for at least one of these arguments!")
        } else{
            ### Check lambdas.length if specified
            if(!is.numeric(lambdas.length)) stop("lambdas.length must be numeric!")
            if(lambdas.length <= 0) stop("lambdas.length must be positive!")
        }

        if(missing(rlam)){
            ### Even though ccdr_call should never be called on its own, this behaviour is left for testing backwards-compatibility
            stop("rlam must be specified if lambdas is not explicitly specified.")
        } else if(is.null(rlam)){
            ### rlam = NULL is used as a sentinel value to indicate a default value should be used
            rlam <- 1e-2
        } else{
            ### Check rlam if specified
            if(!is.numeric(rlam)) stop("rlam must be numeric!")
            if(rlam < 0) stop("rlam must be >= 0!")
        }

        # If no grid of lambdas is passed, then use the standard log-scale that starts at
        #  max.lam = sqrt(nn) and descends to min.lam = rlam * max.lam
        lambdas <- sparsebnUtils::generate.lambdas(lambda.max = sqrt(nn),
                                                   lambdas.ratio = rlam,
                                                   lambdas.length = as.integer(lambdas.length),
                                                   scale = "log")
    }

    ### Check lambdas
    if(!is.numeric(lambdas)) stop("lambdas must be a numeric vector!")
    if(any(lambdas < 0)) stop("lambdas must contain only nonnegative values!")

#     if(length(lambdas) != lambdas.length){
#         warning("Length of lambdas vector does not match lambdas.length. The specified lambdas vector will be used and lambdas.length will be overwritten.")
#     }

    ### By default, set the initial guess for betas to be all zeroes
    if(missing(betas)){
        betas <- matrix(0, nrow = pp, ncol = pp)
        # betas <- SparseBlockMatrixR(betas) # 2015-03-26: Deprecated and replaced with .init_sbm below
        betas <- .init_sbm(betas, rep(0, pp))

        # If the initial matrix is the zero matrix, indexing does not matter so we don't need to use reIndexC here
        #   Still need to set start = 0, though.
        betas$start <- 0
    } # Type-checking for betas happens in ccdr_singleR

    # This parameter can be set by the user, but in order to prevent the algorithm from taking too long to run
    #  it is a good idea to keep the threshold used by default which is O(sqrt(pp))
    if(is.null(max.iters)){
        max.iters <- sparsebnUtils::default_max_iters(pp)
    }

    t1.cor <- proc.time()[3]
    #     cors <- cor(data)
    #     cors <- cors[upper.tri(cors, diag = TRUE)]
    # cors <- sparsebnUtils::cor_vector(data)
    ip <- innerprod(data)
    t2.cor <- proc.time()[3]

    ### Process blocks
    if(is.null(blocks)){
        pp <- ncol(data)

        # blocks <- lapply((1:pp), function(x){
        #     # Allow all off-diagonal entries since we are no longer using the block decomposition
        #     row <- (1:pp)[-x]
        #     col <- rep(x, length(col))
        #     cbind(row, col)
        # })
        blocks <- allBlocks(1:pp)

        # i <- rep(1:pp, each = pp)
        # j <- rep(1:pp, times = pp)
        # blocks <- cbind(i, j)
    } else if(!is.matrix(blocks)){
        if(blocks == -1){
            stop("Invalid input for argument <blocks>!")
        } else if(blocks == -2){
            stop("Invalid input for argument <blocks>!")
        } else if(blocks == -3){
            # Determine order by decreasing maximum absolute inner product, breaking ties however order does it
            pp <- ncol(data)
            # ip <- innerprod(data) # now pre-computed (see above)
            absip <- abs(ip)
            diag(absip) <- rep(0, pp)
            node_order <- order(apply(absip, 2, max), decreasing = TRUE) # order according to high maximum absolute inner product, breaking ties however order does it

            # Screen by CI graph
            t1quic <- proc.time()[3]
            moralgraph <- BigQuic::BigQuic(X = as.matrix(data), lambda = blocks.lambda,
                                           numthreads = 1, memory_size = 2048, seed = 1,
                                           use_ram = TRUE)
            moralgraph <- moralgraph$precision_matrices[[1]]
            t2quic <- proc.time()[3]
            cat(sprintf("Time spent in QUIC: %fs\n", t2quic-t1quic))

            # A better solution that avoids block1hash altogether: Re-order blocks2 according to node_order
            #  from http://stackoverflow.com/questions/1568511/how-do-i-sort-one-vector-based-on-values-of-another?rq=1
            blocks2 <- matrix2blocks(moralgraph)
            blocks <- blocks2[order(match(blocks2[, 2], node_order)), ]
            rownames(blocks) <- NULL
        } else if(blocks == -4){
            stop("Invalid input for argument <blocks>!")
        } else if(blocks == -5){
            stop("Invalid input for argument <blocks>!")
        } else{
            stop("Invalid input for argument <blocks>!")
        }
    }

    message(sprintf("Number of blocks allowed: %d", nrow(blocks)))
    # print(blocks)

    ### Converts blocks into vector
    ### NOTE: This basically gets undone in rcpp_wrap, better to pass directly as a matrix
    blocks <- as.vector(t(blocks))

    ### Convert inner products to vector
    ip <- ip_to_vector(ip)

    fit <- ccdr_gridR(ip,
                      as.integer(pp),
                      as.integer(nn),
                      betas,
                      as.numeric(sigmas),
                      as.numeric(lambdas),
                      as.numeric(gamma),
                      as.numeric(error.tol),
                      as.integer(max.iters),
                      as.numeric(alpha),
                      as.integer(blocks),
                      as.logical(randomize),
                      verbose)

    #
    # Output DAGs as edge lists (i.e. edgeList objects).
    #  This is NOT the same as sbm$rows since some of these rows may correspond to edges with zero coefficients.
    #  See docs for SparseBlockMatrixR class for details.
    #
    for(k in seq_along(fit)){
        ### Coerce sbm output to edgeList
        names(fit[[k]])[1] <- "edges" # rename 'sbm' slot to 'edges': After the next line, this slot will no longer be an SBM object
        fit[[k]]$edges <- sparsebnUtils::as.edgeList(fit[[k]]$edges) # Before coercion, li$edges is actually an SBM object

        ### Add node names to output
        fit[[k]] <- append(fit[[k]], list(names(data)), after = 1) # insert node names into second slot
        names(fit[[k]])[2] <- "nodes"

        ### Fix number of edges (bugfix for Rccdr2 w/ small world graphs)
        fit[[k]]$nedge <- num.edges(fit[[k]]$edges)
    }

    fit <- lapply(fit, sparsebnUtils::sparsebnFit)    # convert everything to sparsebnFit objects
    sparsebnUtils::sparsebnPath(fit)                  # wrap as sparsebnPath object

    #
    # Useful for debugging: Output as matrix instead
    #
    # lapply(fit, function(x) as.matrix(x$sbm)) #as.matrix(fit[[1]]$sbm)
} # END CCDR_CALL

# ccdr_gridR
#
#   Main subroutine for running the CCDr algorithm on a grid of lambda values.
ccdr_gridR <- function(ip,
                       pp, nn,
                       betas,
                       sigmas,
                       lambdas,
                       gamma,
                       eps,
                       maxIters,
                       alpha,
                       blocks,
                       randomize,
                       verbose
){

    ### Check alpha
    if(!is.numeric(alpha)) stop("alpha must be numeric!")
    if(alpha < 0) stop("alpha must be >= 0!")

    ### nlam is now set automatically
    nlam <- length(lambdas)

    ccdr.out <- list()
    for(i in 1:nlam){
        if(verbose) message("Working on lambda = ", round(lambdas[i], 5), " [", i, "/", nlam, "]")

        t1.ccdr <- proc.time()[3]
        ccdr.out[[i]] <- ccdr_singleR(ip,
                                      pp, nn,
                                      betas,
                                      sigmas,
                                      lambdas[i],
                                      gamma = gamma,
                                      eps = eps,
                                      maxIters = maxIters,
                                      alpha = alpha,
                                      blocks = blocks,
                                      randomize = randomize,
                                      verbose = verbose
        )
        t2.ccdr <- proc.time()[3]

        betas <- ccdr.out[[i]]$sbm
        betas <- sparsebnUtils::reIndexC(betas) # use C-friendly indexing

        if(verbose){
            test.nedge <- sum(sparsebnUtils::as.sparse(betas)$vals != 0)
            message("  Estimated number of edges: ", ccdr.out[[i]]$nedge)
            # message("  Estimated total variance: ", sum(1 / (betas$sigmas)^2))
        }

        # 7-16-14: Added code below to check edge threshold via alpha parameter
        if(ccdr.out[[i]]$nedge > alpha * pp){
            if(verbose) message("Edge threshold met, terminating algorithm with ", ccdr.out[[i-1]]$nedge, " edges.")
            ccdr.out <- ccdr.out[1:(i-1)] # only return up to i - 1 since the last (ith) model did not finish
            break
        }
    }

    ccdr.out
} # END CCDR_GRIDR

# ccdr_singleR
#
#   Internal subroutine for handling calls to singleCCDr: This is the only place where C++ is directly
#    called. Type-checking is strongly enforced here.
ccdr_singleR <- function(ip,
                         pp, nn,
                         betas,
                         sigmas,
                         lambda,
                         gamma,
                         eps,
                         maxIters,
                         alpha,     # 2-9-15: No longer necessary in ccdr_singleR, but needed since the C++ call asks for it
                         blocks,
                         randomize,
                         verbose = FALSE
){

    ### Check ip
    if(!is.numeric(ip)) stop("ip must be a numeric vector!")
    if(length(ip) != pp*(pp+1)/2) stop(paste0("ip has incorrect length: Expected length = ", pp*(pp+1)/2, " input length = ", length(ip)))

    ### Check dimension parameters
    if(!is.integer(pp) || !is.integer(nn)) stop("Both pp and nn must be integers!")
    if(pp <= 0 || nn <= 0) stop("Both pp and nn must be positive!")

    ### Check betas
    if(sparsebnUtils::check_if_matrix(betas)){ # if the input is a matrix, convert to SBM object
        betas <- .init_sbm(betas, rep(0, pp)) # if betas is non-numeric, SparseBlockMatrixR constructor should throw error
        betas <- reIndexC(betas) # use C-friendly indexing
    } else if(!is.SparseBlockMatrixR(betas)){ # otherwise check that it is an object of class SparseBlockMatrixR
        stop("Incompatible data passed for betas parameter: Should be either matrix or list in SparseBlockMatrixR format.")
    }

    ### Check lambda
    if(!is.numeric(lambda)) stop("lambda must be numeric!")
    if(lambda < 0) stop("lambda must be >= 0!")

    ### Check gamma
    if(!is.numeric(gamma)) stop("gamma must be numeric!")
    if(gamma < 0 && gamma != -1) stop("gamma must be >= 0 (MCP) or = -1 (Lasso)!")

    ### Check eps
    if(!is.numeric(eps)) stop("eps must be numeric!")
    if(eps <= 0){
        if(eps < 0) stop("eps must be >= 0!")
        if(eps == 0) warning("eps is set to zero: This may cause the algorithm to fail to converge, and maxIters will be used to terminate the algorithm.")
    }

    ### Check maxIters
    if(!is.integer(maxIters)) stop("maxIters must be an integer!")
    if(maxIters <= 0) stop("maxIters must be > 0!")

    ### alpha check is in ccdr_gridR

    ### blocks
    blocks <- blocks - 1

    if(verbose) cat("Opening C++ connection...")
    t1.ccdr <- proc.time()[3]
    ccdr.out <- singleCCDr(ip,
                           betas,
                           sigmas,
                           nn,
                           lambda,
                           c(gamma, eps, maxIters, alpha, randomize),
                           blocks,
                           verbose = verbose)
    t2.ccdr <- proc.time()[3]
    if(verbose) cat("C++ connection closed. Total time in C++: ", t2.ccdr-t1.ccdr, "\n")

    #
    # Convert output back to SBM format
    #
    ccdr.out <- list(sbm = SparseBlockMatrixR(list(rows = ccdr.out$rows, vals = ccdr.out$vals, blocks = ccdr.out$blocks, sigmas = ccdr.out$sigmas, start = 0)),
                     lambda = ccdr.out$lambda,
                     nedge = ccdr.out$length,
                     pp = pp,
                     nn = nn,
                     time = t2.ccdr - t1.ccdr)
    ccdr.out$sbm <- sparsebnUtils::reIndexR(ccdr.out$sbm)

    # sparsebnFit(ccdr.out)
    ccdr.out
} # END CCDR_SINGLER
