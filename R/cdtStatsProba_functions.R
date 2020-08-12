
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
    vX <- var(X, na.rm = TRUE)
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

#############################################

## Sample Quantiles Type 8
quantile8 <- function(x, probs){
    x <- x[!is.na(x)]
    nl <- length(x)
    if(nl == 0) return(rep(NA, length(probs)))
    xs <- sort.int(x, decreasing = FALSE)
    xq <- nl * probs + 0.3333 * probs + 0.3333
    ix <- trunc(xq)
    if(length(ix) >= nl)
        xs[ix]
    else
        xs[ix] + (xq - ix) * (xs[ix + 1] - xs[ix])
}

#############################################

## Initial values of parameters
startnorm <- function(x){
    m <- mean(x)
    s <- sd(x)
    list(mean = m, sd = s)
}

startsnorm <- function(x){
    m <- mean(x)
    s <- sd(x)
    # xi <- min(0.99, abs(mean(((x-m)/s)^3)))
    xi <- 1
    list(mean = m, sd = s, xi = xi)
}

startlnorm <- function(x){
    m <- mean(x)
    v <- var(x)
    slog2 <- log((v + m^2) / m^2)
    mlog <- log(m) - slog2 / 2
    slog <- sqrt(slog2)
    list(meanlog = mlog, sdlog = slog)
}

startgamma <- function(x){
    m <- mean(x)
    v <- var(x)
    shape <- m^2 / v
    scale <- v / m
    list(shape = shape, scale = scale)
}

startexp <- function(x){
    m <- mean(x)
    rate <- 1 / m
    list(rate = rate)
}

startweibull <- function(x){
    m <- mean(x)
    s <- sd(x)
    # Ramírez and Carta (2005)
    # shape <- (m / s)^1.086
    shape <- (0.9874 / (s/m))^1.0983
    scale <- m / gamma(1 + 1/shape)
    list(shape = shape, scale = scale)
}

#############################################

## Fitting empirical distributions to theoretical models
fit.distributions <- function(x, distr = c("norm", "snorm", "lnorm", "gamma", "weibull"),
                              method = 'mle', ...)
{
    fit.distr <- lapply(distr, function(dm){
        if(dm %in% c("lnorm", "gamma", "weibull") & any(x <= 0)) return(NULL)
        start.pars <- do.call(paste0("start", dm), list(x))
        fit.mod <- try(fitdistrplus::fitdist(x, dm, method = method, start = start.pars, ...), silent = TRUE)
        fit.mod
    })
    idist <- sapply(fit.distr, function(d) if(!is.null(d)) !inherits(d, "try-error") else FALSE)
    fit.distr <- if(any(idist)) fit.distr[idist] else NULL
    return(fit.distr)
}

#############################################

## Fit Bernoulli-Gamma distribution
fit.berngamma.rain <- function(x, min.len = 7, alpha = 0.05, method = 'mle',
                               lower = c(0, 1e-10, 1e-10), upper = c(1, Inf, Inf),
                               keepdata = FALSE, keepdata.nb = 3, ...)
{
    x <- x[!is.na(x)]
    ret <- NULL
    if(length(x) > min.len){
        if(length(x[x > 0]) > 2){
            if(var(x[x > 0]) == 0)
                x[x > 0] <- x[x > 0] + stats::runif(length(x[x > 0]))
            if(length(which(x == 0)) == 0) x <- c(x, 0)
        }else return(NULL)

        start.pars <- startberngamma(x)
        fit.mod <- try(fitdistrplus::fitdist(x, "berngamma", method = method, start = start.pars,
                                             lower = lower, upper = upper, keepdata = keepdata,
                                             keepdata.nb = keepdata.nb, ...),
                       silent = TRUE)

        if(!inherits(fit.mod, "try-error")){
            # Anderson-Darling Test
            goftest <- ADGofTest::ad.test(x, pberngamma,
                                          prob = fit.mod$estimate['prob'],
                                          scale = fit.mod$estimate['scale'],
                                          shape = fit.mod$estimate['shape'])
            test <- if(goftest$p.value > alpha) 'yes' else 'no'
            ret <- list(fitted.distr = fit.mod, ADgoftest = goftest, h0 = test)
        }else{
            ret <- list(fitted.distr = list(estimate = unlist(start.pars)), ADgoftest = NULL, h0 = 'null')
        }
    }

    return(ret)
}

#############################################

## Fit normal distribution for temp
fit.norm.temp <- function(x, min.len, alpha = 0.05, method = 'mle',
                          lower = c(-20, 0), upper = c(60, 10),
                          keepdata = FALSE, keepdata.nb = 3, ...)
{
    x <- x[!is.na(x)]
    ret <- NULL
    if(length(x) > min.len){
        xmoy <- mean(x)
        xsd <- sd(x)
        fit.mod <- try(fitdistrplus::fitdist(x, "norm", method = method,
                                             start = list(mean = xmoy, sd = xsd),
                                             lower = lower, upper = upper,
                                             keepdata = keepdata, keepdata.nb = keepdata.nb, ...),
                       silent = TRUE)

        if(!inherits(fit.mod, "try-error")){
            # Shapiro-Wilk normality test
            swnt <- stats::shapiro.test(x)
            test <- if(swnt$p.value > alpha) 'yes' else 'no'
            ret <- list(fitted.distr = fit.mod, SWNtest = swnt, h0 = test)

            # # Anderson-Darling Test
            # goftest <- ADGofTest::ad.test(x, pnorm,
            #                               mean = fit.mod$estimate['mean'],
            #                               sd = fit.mod$estimate['sd'])
            # test <- if(goftest$p.value > alpha) 'yes' else 'no'
            # ret <- list(fitted.distr = fit.mod, ADgoftest = goftest, h0 = test)
        }else{
            start.pars <- c(mean = xmoy, sd = xsd)
            ret <- list(fitted.distr = list(estimate = start.pars), SWNtest = NULL, h0 = 'null')
        }
    }

    return(ret)
}
