preSGT <- function(tname) {
        ## Process the n-gram freq DT via the SGT algorithm
        ## ref: https://www.grsampson.net/AGtf1.html
        
        #get the freq of freq table
        x <- table(tname$freq)
        SGT <- data.table(n = as.integer(x), r = as.integer(unlist(dimnames(x))))
        setorder(SGT, r)
        N <- sum(SGT$r * SGT$n)
        P0 <- SGT$n[1]/N 
        
        lastrow <- nrow(SGT)
        for (index in 1:lastrow) {
                # special cases for first and last rows
                if (index == 1) {
                        i <- 0
                } else {
                        i <- SGT$r[index - 1]
                }
                if (index == lastrow) {
                        k <- 2 * SGT$r[ index  ] - i
                } else {
                        k <- SGT$r[ index + 1 ]
                }
                SGT$Z[ index ] <- 2 * SGT$n[index] / (k - i)
                SGT$logr[ index ] <- log(SGT$r[ index ])
                SGT$logZ[index] <- log(SGT$Z[index])
        }
        fit <- lm(logr ~ logZ, data= SGT )
        smoother <- function(x) exp(fit$coef[1] + (fit$coef[2] * log(x)))
        for (index in 1:lastrow) {
                y = (SGT$r[ index ] + 1) * smoother(SGT$r[index] + 1) /
                        smoother(SGT$r[index])
                
        }
        return(SGT)
}