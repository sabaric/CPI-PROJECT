
TVAR1 <-function (data, lag, include = c("const", "trend", "none", "both"), 
          model = c("TAR", "MTAR"), commonInter = FALSE, nthresh = 1, 
          thDelay = 1, mTh = 1, thVar, trim = 0.1, ngrid, gamma = NULL, 
          around, plot = FALSE, dummyToBothRegimes = TRUE, trace = TRUE, 
          trick = "for", max.iter = 2) 
{
  y <- as.matrix(data)
  Torigin <- nrow(y)
  T <- nrow(y)
  p <- lag
  t <- T - p
  k <- ncol(y)
  t <- T - p
  if (is.null(colnames(data))) 
    colnames(data) <- paste("Var", c(1:k), sep = "")
  if (max(thDelay) > p) 
    stop("Max of thDelay should be smaller or equal to the number of lags")
  if (dummyToBothRegimes == FALSE & nthresh != 1) 
    warning("The 'dummyToBothRegimes' argument is only relevant for one threshold models")
  model <- match.arg(model)
  include <- match.arg(include)
  Y <- y[(p + 1):T, ]
  Z <- embed(y, p + 1)[, -seq_len(k)]
  if (include == "const") 
    Z <- cbind(1, Z)
  else if (include == "trend") 
    Z <- cbind(seq_len(t), Z)
  else if (include == "both") 
    Z <- cbind(rep(1, t), seq_len(t), Z)
  if (commonInter & include != "const") 
    stop("commonInter argument only avalaible with include = const")
  npar <- ncol(Z)
  if (!missing(thVar)) {
    if (length(thVar) > Torigin) {
      z <- thVar[seq_len(Torigin)]
      warning("The external threshold variable is not of same length as the original variable")
    }
    else z <- thVar
    z <- embed(z, p + 1)[, seq_len(max(thDelay)) + 1]
    combin <- NULL
  }
  else {
    if (!length(mTh) %in% c(1, k)) 
      stop("length of 'mTh' should be equal to the number of variables, or just one")
    if (length(mTh) == 1) {
      if (mTh > k) 
        stop("Unable to select the ", mTh, "variable for the threshold. Please see again mTh ")
      combin <- matrix(0, ncol = 1, nrow = k)
      combin[mTh, ] <- 1
    }
    else combin <- matrix(mTh, ncol = 1, nrow = k)
    zcombin <- y %*% combin
    if (model == "MTAR") {
      if (max(thDelay) < p) 
        z <- embed(diff(zcombin), p)[, seq_len(max(thDelay)) + 
                                       1]
      else if (max(thDelay) == p) {
        z <- embed(diff(zcombin), p + 1)[, seq_len(max(thDelay)) + 
                                           1]
        z <- rbind(0, as.matrix(z))
      }
    }
    else z <- embed(zcombin, p + 1)[, seq_len(max(thDelay)) + 
                                      1]
  }
  trans <- as.matrix(z)
  allgammas <- sort(unique(trans[, 1]))
  nga <- length(allgammas)
  ninter <- round(trim * nga)
  gammas <- allgammas[(trim * nga):((1 - trim) * nga)]
  if (!missing(ngrid)) {
    gammas <- allgammas[seq(from = ceiling(trim * nga), to = floor((1 - 
                                                                      trim) * nga), length.out = ngrid)]
  }
  if (!missing(gamma)) {
    gammas <- gamma
    plot <- FALSE
  }
  Y <- t(Y)
  if (!missing(around)) {
    if (missing(ngrid)) 
      ngrid <- 20
    if (length(around) == 1) 
      gammas <- aroundGrid(around, allgammas, ngrid, trim, 
                           trace = trace)
    if (length(around) == 2) {
      gammas <- aroundGrid(around[1], allgammas, ngrid, 
                           trim, trace = trace)
      gammas2 <- aroundGrid(around[2], allgammas, ngrid, 
                            trim, trace = trace)
    }
  }
  loop1_onedummy <- function(gam1, thDelay) {
    dummyDown <- ifelse(trans[, thDelay] <= gam1, 1, 0) * 
      Z
    ndown <- mean(dummyDown)
    regimeDown <- dummyDown * Z
    if (min(ndown, 1 - ndown) >= trim) {
      Z1 <- t(cbind(regimeDown, Z))
      B1 <- tcrossprod(Y, Z1) %*% solve(tcrossprod(Z1))
      res <- crossprod(c(Y - B1 %*% Z1))
    }
    else res <- NA
    return(res)
  }
  loop1_twodummy <- function(gam1, thDelay) {
    d1 <- ifelse(trans[, thDelay] <= gam1, 1, 0)
    ndown <- mean(d1)
    if (min(ndown, 1 - ndown) >= trim) {
      Z1 <- t(cbind(d1 * Z, (1 - d1) * Z))
      B1 <- tcrossprod(Y, Z1) %*% solve(tcrossprod(Z1))
      res <- crossprod(c(Y - B1 %*% Z1))
    }
    else res <- NA
    return(res)
  }
  loop1_twodummy_oneIntercept <- function(gam1, thDelay) {
    d1 <- ifelse(trans[, thDelay] <= gam1, 1, 0)
    ndown <- mean(d1)
    if (min(ndown, 1 - ndown) >= trim) {
      Z1 <- t(cbind(1, d1 * Z[, -1], (1 - d1) * Z[, -1]))
      B1 <- tcrossprod(Y, Z1) %*% solve(tcrossprod(Z1))
      res <- crossprod(c(Y - B1 %*% Z1))
    }
    else res <- NA
    return(res)
  }
  loop2 <- function(gam1, gam2, thDelay) {
    dummydown <- ifelse(trans[, thDelay] <= gam1, 1, 0)
    regimedown <- dummydown * Z
    ndown <- mean(dummydown)
    dummyup <- ifelse(trans[, thDelay] > gam2, 1, 0)
    regimeup <- dummyup * Z
    nup <- mean(dummyup)
    if (min(nup, ndown, 1 - nup - ndown) > trim) {
      Z2 <- t(cbind(regimedown, (1 - dummydown - dummyup) * 
                      Z, regimeup))
      res <- crossprod(c(Y - tcrossprod(Y, Z2) %*% solve(tcrossprod(Z2)) %*% 
                           Z2))
    }
    else res <- NA
    return(res)
  }
  loop2_oneIntercept <- function(gam1, gam2, thDelay) {
    dummydown <- ifelse(trans[, thDelay] <= gam1, 1, 0)
    regimedown <- dummydown * Z[, -1]
    ndown <- mean(dummydown)
    dummyup <- ifelse(trans[, thDelay] > gam2, 1, 0)
    regimeup <- dummyup * Z[, -1]
    nup <- mean(dummyup)
    if (min(nup, ndown, 1 - nup - ndown) > trim) {
      Z2 <- t(cbind(1, regimedown, (1 - dummydown - dummyup) * 
                      Z, regimeup))
      res <- crossprod(c(Y - tcrossprod(Y, Z2) %*% solve(tcrossprod(Z2)) %*% 
                           Z2))
    }
    else res <- NA
    return(res)
  }
  if (!missing(around)) 
    gammas <- aroundGrid(around, allgammas, ngrid, trim, 
                         trace = trace)
  if (dummyToBothRegimes) {
    if (commonInter) 
      func <- loop1_twodummy_oneIntercept
    else func <- loop1_twodummy
  }
  else func <- loop1_onedummy
  bestone <- onesearch(thDelay, gammas, fun = func, trace = trace, 
                       gamma = gamma, plot = plot)
  bestThresh <- bestone$bestThresh
  bestDelay <- bestone$bestDelay
  allThSSR <- bestone$allres
  if (nthresh == 2) {
    if (commonInter) 
      func2 <- loop2_oneIntercept
    else func2 <- loop2
    last <- condiStep(allgammas, threshRef = bestThresh, 
                      delayRef = bestDelay, fun = func2, trim = trim, trace = trace)
    i <- 1
    while (i < max.iter) {
      b <- condiStep(allgammas, threshRef = last$newThresh, 
                     delayRef = bestDelay, fun = func2, trim = trim, 
                     trace = trace)
      if (b$SSR < last$SSR) {
        i <- i + 1
        last <- b
      }
      else {
        i <- max.iter
        last <- b
      }
    }
    bests <- c(last$threshRef, last$newThresh)
    smallThresh <- min(bests)
    gammasDown <- aroundGrid(around = smallThresh, allgammas, 
                             ngrid = 30, trim = trim, trace = trace)
    bigThresh <- max(bests)
    gammasUp <- aroundGrid(around = bigThresh, allgammas, 
                           ngrid = 30, trim = trim, trace = trace)
    bestThresh <- grid(gammasUp, gammasDown, fun = func2, 
                       method = trick, thDelay = bestDelay, trace = trace)
  }
  if (nthresh == 3) {
    bestDelay <- thDelay
    if (missing(gamma) == FALSE) {
      gammas <- gamma[1]
      gammas2 <- gamma[2]
      ninter <- 2
      cat("To be corrected!!")
    }
    if (missing(around) == FALSE) {
      if (length(around) != 2) 
        stop("Please give two thresholds possible values to search around")
      gammas <- aroundGrid(min(around), allgammas, ngrid = ngrid, 
                           trim = trim, trace = trace)
      gammas2 <- aroundGrid(max(around), allgammas, ngrid = ngrid, 
                            trim = trim, trace = trace)
    }
    else {
      gammas2 <- gammas
      if (length(gammas) * length(gammas2)/2 > 10000) 
        cat("The function will compute about", length(gammas) * 
              length(gammas2)/2, "operations. Take a coffee and come back\n")
    }
    if (length(thDelay) > 1) 
      stop("length of thDelay should not be bigger than 1. The whole search is made only upon the thresholds with given delay")
    store3 <- matrix(NA, ncol = length(gammas2), nrow = length(gammas))
    for (i in seq_len(length(gammas))) {
      gam1 <- gammas[i]
      for (j in seq_len(length(gammas))) {
        gam2 <- gammas2[j]
        store3[i, j] <- loop2(gam1, gam2, thDelay = bestDelay)
      }
    }
    position <- which(store3 == min(store3, na.rm = TRUE), 
                      arr.ind = TRUE)
    r <- position[1]
    c <- position[2]
    gamma1 <- gammas[r]
    gamma2 <- gammas2[c]
    bestThresh <- c(gamma1, gamma2)
  }
  if (commonInter) 
    val <- 1
  else val <- -(seq_len(ncol(Z)))
  if (nthresh == 1) {
    dummydown <- ifelse(trans[, bestDelay] <= bestThresh, 
                        1, 0)
    ndown <- mean(dummydown)
    regimeDown <- dummydown * Z[, -val]
    dummyup <- 1 - dummydown
    if (dummyToBothRegimes) 
      regimeUp <- dummyup * Z[, -val]
    else regimeUp <- Z
    if (commonInter) 
      Zbest <- t(cbind(1, regimeDown, regimeUp))
    else Zbest <- t(cbind(regimeDown, regimeUp))
  }
  if (nthresh == 2 | nthresh == 3) {
    dummydown <- ifelse(trans[, bestDelay] <= bestThresh[1], 
                        1, 0)
    ndown <- mean(dummydown)
    regimedown <- dummydown * Z[, -val]
    dummyup <- ifelse(trans[, bestDelay] > bestThresh[2], 
                      1, 0)
    nup <- mean(dummyup)
    regimeup <- dummyup * Z[, -val]
    dummymid <- 1 - dummydown - dummyup
    if (commonInter) 
      Zbest <- t(cbind(1, regimedown, dummymid * Z[, -1], 
                       regimeup))
    else Zbest <- t(cbind(regimedown, dummymid * Z, regimeup))
  }
  reg <- if (nthresh == 1) 
    dummydown + 2 * dummyup
  else dummydown + 2 * dummymid + 3 * dummyup
  regime <- c(rep(NA, T - t), reg)
  Bbest <- Y %*% t(Zbest) %*% solve(Zbest %*% t(Zbest))
  fitted <- Bbest %*% Zbest
  resbest <- t(Y - fitted)
  SSRbest <- as.numeric(crossprod(c(resbest)))
  nparbest <- nrow(Bbest) * ncol(Bbest)
  Sigmabest <- matrix(1/t * crossprod(resbest), ncol = k, dimnames = list(colnames(data), 
                                                                          colnames(data)))
  SigmabestOls <- Sigmabest * (t/(t - ncol(Bbest)))
  rownames(Bbest) <- paste("Equation", colnames(data))
  LagNames <- c(paste(rep(colnames(data), p), -rep(seq_len(p), 
                                                   each = k)))
  Bnames <- c(switch(include, const = "Intercept", trend = "Trend", 
                     both = c("Intercept", "Trend"), none = NULL), LagNames)
  Blist <- nameB(mat = Bbest, commonInter = commonInter, Bnames = Bnames, 
                 nthresh = nthresh, npar = npar)
  BnamesVec <- if (class(Blist) == "list") 
    c(sapply(Blist, colnames))
  else colnames(Blist)
  colnames(Bbest) <- BnamesVec
  if (nthresh == 1) 
    nobs <- c(ndown = ndown, nup = 1 - ndown)
  else if (nthresh == 2) 
    nobs <- c(ndown = ndown, nmiddle = 1 - nup - ndown, nup = nup)
  tZbest <- t(Zbest)
  naX <- rbind(matrix(NA, ncol = ncol(tZbest), nrow = p), tZbest)
  YnaX <- cbind(data, naX)
  BlistMod <- nameB(mat = Bbest, commonInter = commonInter, 
                    Bnames = Bnames, nthresh = nthresh, npar = npar, sameName = FALSE)
  BnamesVecMod <- if (class(BlistMod) == "list") 
    c(sapply(BlistMod, colnames))
  else colnames(BlistMod)
  colnames(YnaX) <- c(colnames(data), BnamesVecMod)
  specific <- list()
  specific$allgammas <- allgammas
  specific$gammas <- gammas
  specific$thDelay <- bestDelay
  specific$Thresh <- bestThresh
  specific$nthresh <- nthresh
  specific$transCombin <- combin
  specific$regime <- regime
  specific$nreg <- nthresh + 1
  specific$nrowB <- npar
  specific$nobs <- nobs
  specific$oneMatrix <- commonInter
  specific$threshEstim <- ifelse(is.null(gamma), TRUE, FALSE)
  specific$allThSSR <- allThSSR
  specific$Bnames <- Bnames
  specific$timeAttributes <- attributes(data[, 1])
  z <- list(coefficients = Blist, coeffmat = Bbest, residuals = resbest, 
            model = YnaX, nobs_regimes = nobs, k = k, t = t, T = T, 
            nparB = nparbest, fitted.values = fitted, lag = lag, 
            include = include, model.specific = specific, usedThVar = trans[, 
                                                                            bestDelay], trim = trim)
  class(z) <- c("TVAR", "nlVar")
  attr(z, "levelTransVar") <- model
  attr(z, "transVar") <- if (!missing(thVar)) 
    "external"
  else "internal"
  attr(z, "varsLevel") <- "level"
  if (plot) {
    layout(matrix(1:ifelse(z$model.specific$threshEstim, 
                           3, 2), ncol = 1))
    plot1(bestThresh, nthresh, usedThVar = z$usedThVar)
    plot2(bestThresh, nthresh, usedThVar = z$usedThVar, trim = z$trim)
    if (z$model.specific$threshEstim) 
      plot3(bestThresh, nthresh, allTh = z$model.specific$allThSSR)
  }
  return(z)
}
