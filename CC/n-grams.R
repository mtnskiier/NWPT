
# CITE
# Davies, Mark. (2011) N-grams data from the Corpus of Contemporary American English (COCA). Downloaded from http://www.ngrams.info on August 4, 2017. 

dicecorpus <- function() {
        ## Divide corpus into training, testing, and validation
        ## Divide each file set seperately to prevent sample bias on one doc source
        ## 70/20/10 respectively
        
        cfiles <- list.files(pattern = "clean*")
        
        for (j in 1:3) {
                # read in clean data and create a subset sample
                sentences <- readselect(cfiles[j])
                set.seed(7889)
                len <- length(sentences)
                inTrain <- sample(1:len, size = len * .7, replace = FALSE)
                lines <- sentences[inTrain]
                
                fname <-paste0("train", basename(cfiles[j]))
                conn <- file(fname, "w")
                writeLines(lines, con = conn)
                close(conn)
                
                lines <- sentences[-inTrain]
                len <- length(lines)
                inTest <- sample(1:len, size = len * .66, replace = FALSE)
                testing <- lines[inTest]
                
                fname <-paste0("test", basename(cfiles[j]))
                conn <- file(fname, "w")
                writeLines(testing, con = conn)
                close(conn)
                
                testing <- lines[-inTest]
                
                fname <-paste0("valid", basename(cfiles[j]))
                conn <- file(fname, "w")
                writeLines(testing, con = conn)
                close(conn)
        }
        gc(verbose = FALSE)
        
        # Dice up training data for more bite-sized chunks
        cfiles <- list.files(pattern = "trainclean*")
        nchunk <- 5
        for (j in 1:3) {
                sentences <- readselect(cfiles[j])
                len <- length(sentences)
                blk <- len / nchunk
                start <- 1
                for (i in 1:nchunk) {
                        fname <- gsub("trainclean", paste0("train", as.character(i)), cfiles[j])
                        conn <- file(fname, "w")
                        writeLines(sentences[start:(blk*i)], con = conn)
                        start <- (blk * i) + 1
                        close(conn)
                }
                unlink(cfiles[j])
                
        }
                
}

makeNgramFreq <- function (input_text, ng = 1) {
        # INPUT: input_text - A cleaned text vector
        #
        # OUTPUT: A data.table of ngrams and corresponding frequency
        
        library(quanteda)
        library(data.table)
        
        ngramlist <- quanteda::tokenize(char_tolower(input_text), what = "word",
                                        remove_punct = TRUE, remove_symbols = TRUE, 
                                        remove_numbers = TRUE, remove_url = TRUE,
                                        ngrams = ng, 
                                        concatenator = " ", simplify = TRUE)
        
        # make data.table and add frequency
        ngDT <- data.table(tokens = ngramlist)
        ngDT <- ngDT[, .(freq = .N), by = .(tokens)]
        
        return(ngDT) 
}

mergeDT <- function(x, y) {
        # Merge two ngramFreq data.table 
       
        library(data.table)
        z <- merge(x, y, by = "tokens", all=TRUE)
        z[is.na(z)] <- 0
        z <- z[, c("freq"):= (freq.x + freq.y)  ]
        z <- z[, c("freq.x", "freq.y"):=NULL]
        return(z)
}


trainProcess <- function(n = 1) {
        # read all train data, create ngrams and end up with a single ngDT
        # Create the n-gram freq table given n 
        
        ngDT <- NULL

        for (j in list.files(pattern = "train*")) {
                sentences <- readselect(j)
                ng <- makeNgramFreq(sentences, ng = n)
                if (!is.null(ngDT)) {
                        ngDT <- mergeDT(ngDT, ng)
                } else {
                        ngDT <- ng
                }
                
        }
     
       
        setorder(ngDT, -freq)
         
        return(ngDT)
        
}


