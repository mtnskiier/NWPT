testdrv <- function(sentl) {
        # With a sentence list, extract a random set of sentences, then extract a
        # set of quads, triads, biads. Strip last word. Predict and measure the outcome.
        library(stringr)
        library(quanteda)
        
        nsample <- 1000
        
        sentl <-sample(sentl, nsample, replace=TRUE)
        sentl <- cleanr(unname(unlist(tokens(sentl, what= "sentence"))))
        #sentl <- gsub("[[:punct:]]", " ", tolower(sentl))

        
        wds <- word(sentl, end=3)
        nxt <- word(sentl, start=4, end=4)
        
        
        
        x <- data.frame(words=wds, nxt=nxt, p1=as.character(NA), p2=as.character(NA),
                        p3=as.character(NA), p4=as.character(NA), p5=as.character(NA),
                        score = as.numeric(NA), exact = as.integer(NA),
                        stringsAsFactors = FALSE)
         x <- x[!is.na(x$nxt), ]
        
        for (i in 1:nrow(x)) {
                # select the search algo: Csbo and sbo, isbo
                guess <- sbo(x$words[i])
                x$p1[i]<-guess$t4[1]
                x$p2[i]<-guess$t4[2]
                x$p3[i]<-guess$t4[3]
                x$p4[i]<-guess$t4[4]
                x$p5[i]<-guess$t4[5]
                G <- c(x$p1[i],x$p2[i],x$p3[i], x$p4[i], x$p5[i])
                if (x$nxt[i] %in% G)
                        x$score[i] <- 1
                if (x$nxt[i] == guess$t4[1])
                        x$exact[i] <- 1
                
        }
         
        msg<-paste("Correct predictions: ", sum(x$score, na.rm = TRUE)/nrow(x))
        print(msg)
        msg<-paste("Exact predictions: ", sum(x$exact, na.rm = TRUE)/nrow(x))
        print(msg)
        
        return(x)
        
}