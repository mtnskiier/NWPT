#sed "s/\t/\,/g" <FILENAME> > <filename>.csv

CO3getclean <- function(fname) {
        # get and clean the COCA.csv data
        library(data.table)
        
       
        CCtable <- read.table(fname, sep=",", quote="",
                          col.names = c("freq", "t2", "t3", "t4"),
                          stringsAsFactors = FALSE)
        with(CCtable, {
                foo <- cleanr(paste(token1,token2,token3))
                # Throw away the bottom quartile
                triCdf <- data.frame(tokens = foo, freq=as.integer(CCtable$freq))
                cutline <- as.integer(quantile(triCdf$freq, .25))
                triCDT <- data.table(triCdf[as.integer(triCdf$freq) > cutline,])
                dellist <- grep(" n't( |$)", triCDT$tokens)
                triCDT <- triCDT[-dellist]
        })
        
        
        setorder(triCDT, -freq)
        return(triCDT)
        
}

compqt <- function(qt, Cqt) {
        library(data.table)
        NR <- nrow(Cqt)
        uniq <- NULL
        for (i in 1:NR) {
                
                res <- qt[ t1==Cqt$t1[i] & t2 == Cqt$t2[i] & t3 == Cqt$t3[i] & t4 == Cqt$t4[i], ]
                if (nrow(res) == 0) 
                    uniq <- c(uniq, i)
                    
        }
        uniq        
}
