isbo <- function(wd) {
        ## INPUT: 
        ## wd - a character string of one or more words to use as a input to the prediction algorithm
        ##
        
        # isbo is the interpolated simple back off - under development
        
        
        library(data.table)
        
        wd <- cleanr(list(wd))
        wdlist <- unlist(strsplit(wd, " ", fixed = TRUE))
        ntok <- length(wdlist)
        
        maxorder <- 4 # max length of ngrams 
        if (ntok >= maxorder) {
                # drop leading tokens down to maxorder - 1
                wdlist <- wdlist[(ntok - maxorder + 2):ntok]
                ntok <- length(wdlist)
                wd <- paste(wdlist, collapse = " ")
        } 
        
        lambda <- 1
        res4 <- qt[0,]
        res3 <- tt[0,]
        res2 <- bt[0,]
        res <-  ut[0,]
        NR <- 0
        
        
        if (ntok == 3) { # predict from quadgrams
                res4 <- qt[t1 == wdlist[1] &
                                  t2 == wdlist[2] &
                                  t3 == wdlist[3], .(t4, score = freq)]
                NR <- nrow(res4)
                if (NR > 0) {
                        lfreq <- tt[t2 == wdlist[1] &
                                            t3 == wdlist[2] &
                                            t4 == wdlist[3]]$freq
                        res4[, c("score"):= (score/lfreq*lambda)]
                        print("found match in qt")
                        ntok <- 2
                        lambda <- lambda * .4
                        wdlist[1] <- wdlist[2]
                        wdlist[2] <- wdlist[3]
                        
                }
        }
        
        if (ntok >= 2 & NR < 10) { #search the trigrams
                res3 <- tt[t2 == wdlist[1] &
                                  t3 == wdlist[2], .(t4, score = freq)]
                NR <- nrow(res3)
                if (NR > 0) {
                        lfreq <- bt[t3 == wdlist[1] & t4 == wdlist[2]]$freq
                        res3[, c("score"):= (score/lfreq*lambda)]
                        print("found match in tt")
                } else {
                        # decrement the count, shift the tokens and drop thru to bigram search
                        ntok <- 1
                        wdlist[1] <- wdlist[2]
                        lambda <- lambda * .4
                }
        }
        
        if (ntok >= 1 & NR < 10) { #search the bigrams
                res2 <- bt[t3 == wdlist[1], .(t4, score = freq)]
                if (nrow(res2) > 0) {
                        lfreq <- ut[t4 == wdlist[1]]$freq
                        res2[, c("score"):= (score/lfreq*lambda)]
                        print("found match in bt")
                        lambda <- lambda * .03
                }
        }
         lambda <- .005
        res <- ut[order(-freq)][1:10, .(t4, score = (freq/nrow(ut)*lambda))]  
        
        if (nrow(res4) > 0) {
                res <- merge(res, res4[1:10], by="t4", all = TRUE)
                res[is.na(res)]<-0
                res[, c("score"):=(score.x + score.y)]
                res[, c("score.x", "score.y"):=NULL]
                print("in merge")
                print(nrow(res))
        }
        
        if (nrow(res3) > 0) {
                res <- merge(res, res3[1:10], by = "t4", all = TRUE)
                res[is.na(res)]<-0
                res[, c("score"):=(score.x + score.y)]
                res[, c("score.x", "score.y"):=NULL]
                print("in merge3")
                print(nrow(res))
        }
        
        if (nrow(res2) > 0) {
                res <- merge(res, res2[1:10], by = "t4", all = TRUE)
                res[is.na(res)]<-0
                res[, c("score"):=(score.x + score.y)]
                res[, c("score.x", "score.y"):=NULL]
                print("in merge2")
                print(nrow(res))
        }
        
        setorder(res, -score)
        return(res)
              
     
        #setorder(mres, -score)
        #return(head(mres, 10))
        
}



