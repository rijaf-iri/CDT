
#' Vectorization, regression
#'
#' Regression with a matrix and a vector performed by column \code{lm(Y~X)}.
#' 
#' @param X A vector of length N.
#' @param Y A numeric NxK matrix.
#' @param min.len An integer specify the minimum length of non missing data.
#' 
#' @return Returns a numeric 10xK matrix.
#' 
#' @examples
#' library(CDT)
#' 
#' set.seed(1)
#' X <- rnorm(20)
#' Y <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' res <- regression.Vector(X, Y, 10)
#' 
#' @export

regression.Vector <- function(X, Y, min.len){
    Y[is.na(X) | is.na(Y)] <- NA
    nbY <- colSums(!is.na(Y))
    ix <- nbY >= min.len
    RES <- matrix(NA, nrow = 10, ncol = ncol(Y))
    dimnames(RES)[[1]] <- c("slope", "std.slope", "t-value.slope",
                            "p-value.slope", "intercept",
                            "std.intercept", "t-value.intercept",
                            "p-value.intercept", "R2", "sigma")
    if(!any(ix)) return(RES)
    Y <- Y[, ix, drop = FALSE]
    nbY <- nbY[ix]

    mX <- mean(X, na.rm = TRUE)
    mY <- colMeans(Y, na.rm = TRUE)
    vX <- stats::var(X, na.rm = TRUE)
    vY <- matrixStats::colVars(Y, na.rm = TRUE)

    X1 <- X - mX
    Y1 <- sweep(Y, 2, mY, FUN = "-")
    COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
    alpha <- COV / vX
    beta <- mY - alpha * mX

    hatY <- t(sapply(X, `*`, e2 = alpha) + beta)
    SSE <- colSums((hatY - Y)^2, na.rm = TRUE)
    MSE <- SSE / (nbY - 2)
    sigma <- sqrt(MSE)
    std.alpha <- sigma / (sqrt(nbY - 1) * sqrt(vX))
    std.beta <- sigma * sqrt((1 / nbY) + (mX^2 / ((nbY - 1) * vX)))
    SXX <- (nbY - 1) * vX
    tvalue.alpha <- alpha / sqrt(MSE / SXX)
    tvalue.beta <- beta / sqrt(MSE * ((1 / nbY) + (mX^2/SXX)))
    pvalue.alpha <- 2 * stats::pt(-abs(tvalue.alpha), nbY - 2)
    pvalue.beta <- 2 * stats::pt(-abs(tvalue.beta), nbY - 2)
    R2 <- COV^2 / (vX * vY)
    RES[, ix] <- rbind(alpha, std.alpha, tvalue.alpha, pvalue.alpha,
                       beta, std.beta, tvalue.beta, pvalue.beta, R2, sigma)
    return(RES)
}

#############################################

#' Vectorization, regression
#'
#' Regression between two matrices performed by column \code{lm(Y~X)}.
#' 
#' @param X A numeric NxK matrix.
#' @param Y A numeric NxK matrix.
#' @param min.len An integer specify the minimum length of non missing data.
#' 
#' @return Returns a numeric 10xK matrix.
#' 
#' @examples
#' library(CDT)
#' 
#' set.seed(1)
#' X <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' set.seed(2)
#' Y <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' res <- regression.Matrix(X, Y, 10)
#' 
#' @export

regression.Matrix <- function(X, Y, min.len){
    ina <- is.na(X) | is.na(Y)
    X[ina] <- NA
    Y[ina] <- NA
    nbY <- colSums(!is.na(Y))
    ix <- nbY >= min.len
    RES <- matrix(NA, nrow = 10, ncol = ncol(Y))
    dimnames(RES)[[1]] <- c("slope", "std.slope", "t-value.slope",
                            "p-value.slope", "intercept",
                            "std.intercept", "t-value.intercept",
                            "p-value.intercept", "R2", "sigma")
    if(!any(ix)) return(RES)
    Y <- Y[, ix, drop = FALSE]
    X <- X[, ix, drop = FALSE]
    nbY <- nbY[ix]

    mX <- colMeans(X, na.rm = TRUE)
    mY <- colMeans(Y, na.rm = TRUE)
    vX <- matrixStats::colVars(X, na.rm = TRUE)
    vY <- matrixStats::colVars(Y, na.rm = TRUE)

    X1 <- sweep(X, 2, mX, FUN = "-")
    Y1 <- sweep(Y, 2, mY, FUN = "-")
    COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
    alpha <- COV / vX
    beta <- mY - alpha * mX

    hatY <- sweep(sweep(X, 2, alpha, FUN = "*"), 2, beta, FUN = "+")
    SSE <- colSums((hatY - Y)^2, na.rm = TRUE)
    MSE <- SSE / (nbY - 2)
    sigma <- sqrt(MSE)
    std.alpha <- sigma / (sqrt(nbY - 1) * sqrt(vX))
    std.beta <- sigma * sqrt((1 / nbY) + (mX^2 / ((nbY - 1) * vX)))
    SXX <- (nbY - 1) * vX
    tvalue.alpha <- alpha / sqrt(MSE/SXX)
    tvalue.beta <- beta / sqrt(MSE * ((1 / nbY) + (mX^2 / SXX)))
    pvalue.alpha <- 2 * stats::pt(-abs(tvalue.alpha), nbY - 2)
    pvalue.beta <- 2 * stats::pt(-abs(tvalue.beta), nbY - 2)
    R2 <- COV^2 / (vX * vY)

    RES[, ix] <- rbind(alpha, std.alpha, tvalue.alpha, pvalue.alpha,
                       beta, std.beta, tvalue.beta, pvalue.beta, R2, sigma)
    return(RES)
}
