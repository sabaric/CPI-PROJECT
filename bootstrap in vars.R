	p <- varrap$p
    K <- varrap$K
    obs <- varrap$obs
    total <- varrap$totobs
    type <- varrap$type
    B <- Bcoef(varrap)
    BOOT <- vector("list", 3)
    ysampled <- matrix(0, nrow = total, ncol = K)
    colnames(ysampled) <- colnames(varrap$y)
    Zdet <- NULL
    if (ncol(varrap$datamat) > (K * (p + 1))) {
        Zdet <- as.matrix(varrap$datamat[, (K * (p + 1) + 1):ncol(varrap$datamat)])
    }
    resorig <- scale(resid(varrap), scale = FALSE)
    B <- Bcoef(varrap)
         booted <- sample(c(1:obs), replace = TRUE)
        resid <- resorig[booted, ]
        lasty <- c(t(varrap$y[p:1, ]))
        ysampled[c(1:p), ] <- varrap$y[c(1:p), ]
        for (j in 1:obs) {
            lasty <- lasty[1:(K * p)]
            Z <- c(lasty, Zdet[j, ])
            ysampled[j + p, ] <- B %*% Z + resid[j, ]
            lasty <- c(ysampled[j + p, ], lasty)
        }
        varrapboot <- update(varrap, y = ysampled)
        if (class(x) == "svarrapest") {
            varrapboot <- update(x, x = varrapboot)
        }
      head(ysampled)
	head(booted)
	head(resid)