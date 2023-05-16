
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

skewnessV <- function(x){
    x <- x[!is.na(x)]
    n <- length(x)
    x <- x - mean(x)
    sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
}

skewnessM <- function(mat){
    nr <- colSums(!is.na(mat))
    M <- colMeans(mat, na.rm = TRUE)
    X <- sweep(mat, 2, M, FUN = "-")
    S2 <- colSums(X^2, na.rm = TRUE)
    S3 <- colSums(X^3, na.rm = TRUE)
    sqrt(nr) * S3/(S2^(3/2))
}

#############################################

## Initial values of parameters

## Normal
startnorm <- function(x){
    m <- mean(x)
    s <- sd(x)
    list(mean = m, sd = s)
}

## Skew Normal
startsnorm <- function(x){
    m <- mean(x)
    s <- sd(x)
    xi <- 1
    # location: mean
    # scale: sd
    # shape or skewness: xi
    list(mean = m, sd = s, xi = xi)
}

## Log-normal
startlnorm <- function(x){
    m <- mean(x)
    v <- var(x)
    slog2 <- log((v + m^2) / m^2)
    mlog <- log(m) - slog2 / 2
    slog <- sqrt(slog2)
    list(meanlog = mlog, sdlog = slog)
}

## Gamma
startgamma <- function(x){
    m <- mean(x)
    v <- var(x)
    shape <- m^2 / v
    scale <- v / m
    list(shape = shape, scale = scale)
}

## Exponential
startexp <- function(x){
    m <- mean(x)
    rate <- 1 / m
    list(rate = rate)
}

## Weibull
startweibull <- function(x){
    m <- mean(x)
    s <- sd(x)
    # Ramírez and Carta (2005)
    # shape <- (m / s)^1.086
    shape <- (0.9874 / (s/m))^1.0983
    scale <- m / gamma(1 + 1/shape)
    list(shape = shape, scale = scale)
}

## Gumbel
startgumbel <- function(x){
    m <- mean(x)
    s <- sd(x)
    # euler <- 0.5772156649015323

    scale <- s * sqrt(6) / pi
    ## maximum
    loc <- m - 0.45006 * s
    ## or
    ## loc <- m - euler * scale

    ## minimum
    # loc <- m + 0.45006 * s
    ## or
    ## loc <- m + euler * scale

    list(loc = loc, scale = scale)
}

startgumbel_lmom <- function(x){
    euler <- 0.5772156649015323

    lmom <- lmomco::TLmoms(x, nmom = 2)
    scale <- lmom$lambdas[2]/log(2)
    loc <- lmom$lambdas[1] - euler * scale

    list(loc = loc, scale = scale)
}

#############################################
## distribution name
## norm, lnorm, snorm, gamma, exp, weibull, gumbel
## berngamma, bernexp, bernlnorm, bernweibull

get_distr_parameters <- function(dat, distr, min_length, thres = NA){
    ## dat: matrix of data
    ## distr: list(name = "norm", pars = c('mean', 'sd'))
    ## min_length: minimum length of data
    ## thres: threshold for mixture distr 

    coef <- lapply(seq_along(distr$pars), function(i) rep(NA, ncol(dat)))
    names(coef) <- distr$pars

    ina <- colSums(!is.na(dat))
    ix <- ina >= min_length
    if(!any(ix)) return(coef)
    dat <- dat[, ix, drop = FALSE]

    dname <- paste0(distr$name, '_distr_params')
    args <- list(mat = dat)
    if(!is.na(thres)) args$thres <- thres
    out <- do.call(dname, args)

    coef <- lapply(seq_along(coef), function(i){
        x <- coef[[i]]
        x[ix] <- out[[i]]
        x
    })
    names(coef) <- distr$pars

    return(coef)
}

######################

## Normal
norm_distr_params <- function(mat){
    M <- colMeans(mat, na.rm = TRUE)
    S <- matrixStats::colSds(mat, na.rm = TRUE)
    S[S == 0] <- 1e-3

    coef <- list()
    coef$mean <- M
    coef$sd <- S

    return(coef)
}

## Log-normal
lnorm_distr_params <- function(mat){
    M <- colMeans(mat, na.rm = TRUE)
    V <- matrixStats::colVars(mat, na.rm = TRUE)
    SL <- log((V + M^2) / M^2)
    ML <- log(M) - SL / 2
    SL <- sqrt(SL)

    coef <- list()
    coef$meanlog <- ML
    coef$sdlog <- SL

    return(coef)
}

## Skew Normal
snorm_distr_params <- function(mat){
    M <- colMeans(mat, na.rm = TRUE)
    S <- matrixStats::colSds(mat, na.rm = TRUE)
    S[S == 0] <- 1e-3
    Xi <- snorm_shape(mat)

    coef <- list()
    coef$mean <- M
    coef$sd <- S
    coef$xi <- Xi

    return(coef)
}

## Gamma
gamma_distr_params <- function(mat){
    M <- colMeans(mat, na.rm = TRUE)
    V <- matrixStats::colVars(mat, na.rm = TRUE)
    shape <- M^2 / V
    scale <- V / M

    coef <- list()
    coef$shape <- shape
    coef$scale <- scale

    return(coef)
}

## Exponential
exp_distr_params <- function(mat){
    M <- colMeans(mat, na.rm = TRUE)
    rate <- 1 / M

    coef <- list()
    coef$rate <- rate

    return(coef)
}

## Weibull
weibull_distr_params <- function(mat){
    M <- colMeans(mat, na.rm = TRUE)
    S <- matrixStats::colSds(mat, na.rm = TRUE)
    # Ramírez and Carta (2005)
    # shape <- (M / S)^1.086
    shape <- (0.9874 / (S/M))^1.0983
    scale <- M / gamma(1 + 1/shape)

    coef <- list()
    coef$shape <- shape
    coef$scale <- scale

    return(coef)
}

## Gumbel
gumbel_distr_params <- function(mat){
    M <- colMeans(mat, na.rm = TRUE)
    S <- matrixStats::colSds(mat, na.rm = TRUE)
    scale <- S * sqrt(6) / pi
    # loc <- M - 0.45006 * S
    loc <- M - 0.5772156649015323 * scale

    coef <- list()
    coef$loc <- loc
    coef$scale <- scale

    return(coef)
}

## Bernoulli-Gamma
berngamma_distr_params <- function(mat, thres = 1){
    if(thres == 0) thres <- 1e-2
    nb <- colSums(!is.na(mat))
    P <- colSums(mat >= thres, na.rm = TRUE) / nb
    P[P == 0] <- 1e-6

    mat[mat < thres] <- NA
    nna <- colSums(!is.na(mat))
    miss <- nna < 7

    M <- colMeans(mat, na.rm = TRUE)
    V <- matrixStats::colVars(mat, na.rm = TRUE)
    M[miss] <- NA
    V[miss] <- NA
    V[V == 0] <- 0.001

    coef <- list()
    coef$proba <- P
    coef$scale <- V / M
    coef$shape <- M^2 / V

    return(coef)
}

## Bernoulli-Exponential
bernexp_distr_params <- function(mat, thres = 1){
    if(thres == 0) thres <- 1e-2
    nb <- colSums(!is.na(mat))
    P <- colSums(mat >= thres, na.rm = TRUE) / nb
    P[P == 0] <- 1e-6

    mat[mat < thres] <- NA
    nna <- colSums(!is.na(mat))
    miss <- nna < 7

    M <- colMeans(mat, na.rm = TRUE)
    M[miss] <- NA

    coef <- list()
    coef$proba <- P
    coef$rate <- 1 / M

    return(coef)
}

## Bernoulli-Log-Normal 
bernlnorm_distr_params <- function(mat, thres = 1){
    if(thres == 0) thres <- 1e-2
    nb <- colSums(!is.na(mat))
    P <- colSums(mat >= thres, na.rm = TRUE) / nb
    P[P == 0] <- 1e-6

    mat[mat < thres] <- NA
    nna <- colSums(!is.na(mat))
    miss <- nna < 7

    M <- colMeans(mat, na.rm = TRUE)
    V <- matrixStats::colVars(mat, na.rm = TRUE)
    M[miss] <- NA
    V[miss] <- NA

    SL <- log((V + M^2) / M^2)
    ML <- log(M) - SL / 2
    SL <- sqrt(SL)

    coef <- list()
    coef$proba <- P
    coef$meanlog <- ML
    coef$sdlog <- SL

    return(coef)
}

## Bernoulli-Weibull
bernweibull_distr_params <- function(mat, thres = 1){
    if(thres == 0) thres <- 1e-2
    nb <- colSums(!is.na(mat))
    P <- colSums(mat >= thres, na.rm = TRUE) / nb
    P[P == 0] <- 1e-6

    mat[mat < thres] <- NA
    nna <- colSums(!is.na(mat))
    miss <- nna < 7

    M <- colMeans(mat, na.rm = TRUE)
    S <- matrixStats::colSds(mat, na.rm = TRUE)
    M[miss] <- NA
    S[miss] <- NA
    S[S == 0] <- 0.001

    ## Ramírez and Carta (2005)
    # shape <- (M / S)^1.086
    shape <- (0.9874 / (S/M))^1.0983
    scale <- M / gamma(1 + 1/shape)

    ## qmap
    # shape <- 1.2/S
    # scale <- exp(M + 0.572/shape)

    coef <- list()
    coef$proba <- P
    coef$scale <- scale
    coef$shape <- shape

    return(coef)
}

#############################################
## replace berngamma_dist_params & normal_dist_params by get_distr_parameters

## Bernoulli-Gamma distribution
berngamma_dist_params <- function(dat, min.length, thres = 1){
    ## dat matrix of data
    nadat <- rep(NA, ncol(dat))
    coef <- list(proba = nadat, shape = nadat, scale = nadat)

    ina <- colSums(!is.na(dat))
    ix <- ina >= min.length

    if(!any(ix)) return(coef)

    dat <- dat[, ix, drop = FALSE]
    nb <- ina[ix]

    if(thres == 0) thres <- 1e-2

    P <- colSums(dat >= thres, na.rm = TRUE) / nb
    P[P == 0] <- 1e-6

    dat[dat < thres] <- NA
    nna <- colSums(!is.na(dat))
    miss <- nna < 7

    M <- colMeans(dat, na.rm = TRUE)
    V <- matrixStats::colVars(dat, na.rm = TRUE)
    M[miss] <- NA
    V[miss] <- NA
    V[V == 0] <- 0.075

    coef$proba[ix] <- P
    coef$shape[ix] <- M^2 / V
    coef$scale[ix] <- V / M

    return(coef)
}

##
normal_dist_params <- function(dat, min.length){
    nadat <- rep(NA, ncol(dat))
    coef <- list(mean = nadat, sd = nadat)

    ina <- colSums(!is.na(dat))
    ix <- ina >= min.length

    if(!any(ix)) return(coef)
    dat <- dat[, ix, drop = FALSE]

    M <- colMeans(dat, na.rm = TRUE)
    S <- matrixStats::colSds(dat, na.rm = TRUE)
    S[S == 0] <- 1e-3

    coef$mean[ix] <- M
    coef$sd[ix] <- S

    return(coef)
}

#############################################

## Fitting empirical distributions to theoretical models
fit.distributions <- function(x, distr = c("norm", "snorm", "lnorm",
                              "gamma", "exp", "weibull", "gumbel"),
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

#############################################

#### from package fGarch
## https://cran.r-project.org/web/packages/fGarch/index.html

heaviside_fun <- function(x, a = 0){
    (sign(x - a) + 1)/2
}

dsnorm <- function(x, mean = 0, sd = 1, xi = 1.5, log = FALSE){
    x <- (x - mean)/sd
    m1 <- 2/sqrt(2 * pi)
    mu <- m1 * (xi - 1/xi)
    sigma <- sqrt((1 - m1^2) * (xi^2 + 1/xi^2) + 2 * m1^2 - 1)
    z <- x * sigma + mu
    Xi <- xi^sign(z)
    g <- 2 / (xi + 1/xi)
    denst <- g * stats::dnorm(x = z/Xi)
    out <- denst * sigma / sd
    if(log) out <- log(out)

    out
}

psnorm <- function(q, mean = 0, sd = 1, xi = 1.5){
    q <- (q - mean)/sd
    m1 <- 2/sqrt(2 * pi)
    mu <- m1 * (xi - 1/xi)
    sigma <- sqrt((1 - m1^2) * (xi^2 + 1/xi^2) + 2 * m1^2 - 1)
    z <- q * sigma + mu
    Xi <- xi^sign(z)
    g <- 2 / (xi + 1/xi)
    
    heaviside_fun(z) - sign(z) * g * Xi * pnorm(-abs(z)/Xi)
}

qsnorm <- function(p, mean = 0, sd = 1, xi = 1.5){
    m1 <- 2/sqrt(2 * pi)
    mu <- m1 * (xi - 1/xi)
    sigma <- sqrt((1 - m1^2) * (xi^2 + 1/xi^2) + 2 * m1^2 - 1)

    g <- 2 / (xi + 1/xi)
    sig <- sign(p - 1/2)
    Xi <- xi^sig
    p <- (heaviside_fun(p - 1/2) - sig * p) / (g * Xi)
    quant <- (-sig * qnorm(p = p, sd = Xi) - mu) / sigma

    quant * sd + mean  
}

rsnorm <- function(n, mean = 0, sd = 1, xi = 1.5){
    weight <- xi / (xi + 1/xi)
    z <- runif(n, -weight, 1 - weight)
    Xi <- xi^sign(z)
    rand <- -abs(rnorm(n))/Xi * sign(z)
    m1 <- 2/sqrt(2 * pi)
    mu <- m1 * (xi - 1/xi)
    sigma <- sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    rand <- (rand - mu) / sigma

    rand * sd + mean
}


###

snormFit <- function(x, ...){
    start <- c(mean = mean(x), sd = sqrt(var(x)), xi = 1)
    loglik <- function(x, y = x) -sum(log(dsnorm(y, x[1], x[2], x[3])))
    fit <- stats::nlminb(start = start, objective = loglik, 
                         lower = c(-Inf, 0, 0),
                         upper = c( Inf, Inf, Inf), 
                         y = x, ...)
    names(fit$par) <- c("mean", "sd", "xi")
    fit$par
}

###############

# https://en.wikipedia.org/wiki/Skew_normal_distribution

snorm_shape <- function(x){
    if(is.matrix(x)){
        skewFun <- skewnessM
    }else if(is.vector(x)){
        skewFun <- skewnessV
    }else{
        return(NA)
    }

    S <- skewFun(x)
    S2 <- abs(S)^(2/3)
    delta <- sqrt((pi/2) * S2 / (S2 + ((4 - pi)/2)^(2/3)))

    # sign(S) * delta / sqrt(1 - delta^2)
    delta / sqrt(1 - delta^2)
}

#############################################

#### from package evd
## https://cran.r-project.org/web/packages/evd/index.html

dgumbel <- function(x, loc = 0, scale = 1, log = FALSE){
    x <- (x - loc)/scale
    d <- log(1/scale) - x - exp(-x)
    if(!log) d <- exp(d)

    d
}

pgumbel <- function(q, loc = 0, scale = 1, lower.tail = TRUE){
    q <- (q - loc)/scale
    p <- exp(-exp(-q))
    if(!lower.tail) p <- 1 - p

    p
}

qgumbel <- function(p, loc = 0, scale = 1, lower.tail = TRUE){
    if(!lower.tail) p <- 1 - p
    loc - scale * log(-log(p))
}

rgumbel <- function(n, loc = 0, scale = 1){
    loc - scale * log(stats::rexp(n))
}
