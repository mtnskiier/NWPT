
library(data.table)
library(shiny)
library(wordcloud)
library(RColorBrewer)

load(file = "ngrams.RData")

source("sampleCorpus.R")

otxt <- readselect("otab.txt")
utxt <- readselect("utab.txt")


shinyServer(function(input, output) {
  
  output$resTable <- renderTable({
    
    Nres <- as.integer(input$numwds)
    inp <-input$inptext
    # call predict algorithm
    res <- sbo(inp)
    if (nrow(res) > 0) {
            setnames(res, c("t4"), c("Next Word"))
            head(res, Nres)        
    }
    
    }) 

  
  output$plot <- renderPlot( {
        Nres <- as.integer(input$numwds)
        inp <-input$inptext
        # call predict algorithm
        wcres <- sbo(inp)
      
        wordcloud(words=wcres$t4, freq=wcres$score , scale =c(6,.5), min.freq = 0,
                  max.words = 10, colors=brewer.pal(8, "Dark2"), rot.per=.2)
        
        })
  
  output$usage <- renderText(utxt)
  
  output$overview <- renderText(otxt)


})

sbo <- function(wd) {
        ## INPUT: 
        ## wd - a character string of one or more words to use as a input to the prediction algorithm
        ##
        
        # examine highest-order n-gram for match - if none found
        # look at next lower order n-gram and discount score by .4
        # repeat on next lower order until unigrams
        
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
        res <- ut[order(-freq)][1:10, .(t4, score = (freq/nrow(ut)*lambda))]
        
        if (ntok == 3) { # predict from quadgrams
                res <- qt[t1 == wdlist[1] &
                                  t2 == wdlist[2] &
                                  t3 == wdlist[3], .(t4, score = freq)]
                if (nrow(res) > 0) {
                        lfreq <- tt[t2 == wdlist[1] &
                                            t3 == wdlist[2] &
                                            t4 == wdlist[3]]$freq
                        res[, c("score"):= (score/lfreq*lambda)]
                        #print("found match in qt")
                } else {
                        # decrement the count, shift the tokens and drop thru to trigram search
                        ntok <- 2
                        lambda <- lambda * .4
                        wdlist[1] <- wdlist[2]
                        wdlist[2] <- wdlist[3]
                        
                }
        }
        
        if (ntok == 2) { #search the trigrams
                res <- tt[t2 == wdlist[1] &
                                  t3 == wdlist[2], .(t4, score = freq)]
                if (nrow(res) > 0) {
                        lfreq <- bt[t3 == wdlist[1] & t4 == wdlist[2]]$freq
                        res[, c("score"):= (score/lfreq*lambda)]
                        #print("found match in tt")
                } else {
                        # decrement the count, shift the tokens and drop thru to bigram search
                        ntok <- 1
                        wdlist[1] <- wdlist[2]
                        lambda <- lambda * .4
                }
        }
        
        if (ntok == 1) { #search the bigrams
                res <- bt[t3 == wdlist[1], .(t4, score = freq)]
                if (nrow(res) > 0) {
                        lfreq <- ut[t4 == wdlist[1]]$freq
                        res[, c("score"):= (score/lfreq*lambda)]
                        #print("found match in bt")
                } else {
                        # decrement the count, shift the tokens and drop thru to unigram search
                        ntok <- 0
                        lambda <- lambda * .4
                }
        }
        
        if ((ntok == 0) | (nrow(res) == 0)) {
                res <- ut[order(-freq)][1:10, .(t4, score = (freq/nrow(ut)*lambda))]
        }
        
        setorder(res, -score)
        return(head(res, 10))
        
}
