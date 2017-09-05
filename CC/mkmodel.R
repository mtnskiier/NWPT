## Set of functions to transform n-grams from a string of n tokens to 
## each word a named token. uni - quad.
##
## 
unimodel <- function(x) {
        ## process unigram into model format
        library(data.table)
       
        ut <- data.table(t4 = x$tokens, freq = x$freq)
        
        return(ut)
        
}
bimodel <- function(x) {
        ## process bigram into model format
        library(data.table)
        NR <- nrow(x) 
        wdl <- unlist(strsplit(x$tokens, " "))
        t3<-vector("character", length=NR)
        t4<-vector("character", length=NR)
        for (i in 1:(NR)) { # *apply() maybe useful here - FOR loop is easy to understand, albeit slow
                t3[i] <- wdl[2*i-1]
                t4[i] <- wdl[2*i]
        }
        bt <- data.table(t3=t3, t4 = t4, freq = x$freq)
               
        return(bt)
        
}

trimodel <- function(x) {
        ## process trigram into model format
        library(data.table)
        NR <- nrow(x)
        wdl <- unlist(strsplit(x$tokens, " "))
        t2<-vector("character", length=NR)
        t3<-vector("character", length=NR)
        t4<-vector("character", length=NR)
        for (i in 1:(NR)) {
                t2[i] <- wdl[3*i-2]
                t3[i] <- wdl[3*i-1]
                t4[i] <- wdl[3*i]
        }
        tt <- data.table(t2=t2, t3 = t3, t4 = t4, freq = x$freq)
        
        return(tt)
        
}

quadmodel <- function(x) {
        library(data.table)
        ## process quadgram into model format
        NR <- nrow(x)
        wdl <- unlist(strsplit(x$tokens, " "))
        t1<-vector("character", length=NR)
        t2<-vector("character", length=NR)
        t3<-vector("character", length=NR)
        t4<-vector("character", length=NR)
        
        for (i in 1:(NR)) {
                t1[i] <- wdl[4*i - 3]
                t2[i] <- wdl[4*i-2]
                t3[i] <- wdl[4*i-1]
                t4[i] <- wdl[4*i]
        }
        qt <- data.table(t1=t1, t2=t2, t3 = t3, t4 = t4, freq = x$freq)
        
        return(qt)
        
}





