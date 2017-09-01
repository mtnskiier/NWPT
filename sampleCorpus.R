## Set of utility routines for getting and cleaning the corpus. Also used to 
## process the input text in the shiny applet.
##

files <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt",  "final/en_US/en_US.twitter.txt")

getclean <- function() {
        charv <- sapply(files, readselect)
        
        # Tokenize into sentences
        
        library(quanteda)
        # too memory/CPU intensive - do one at a time
        
        for (i in 1:3) {
                sentences <- cleanr(unname(unlist(tokens(unlist(charv[i]), what= "sentence"))))
                fname <- paste0("./clean", basename(files[i]))
                conn <- file(fname, "w")
                writeLines(sentences, con = conn)
                close(conn)
        
        }

   
}

makedfm <- function( flist, ng=1 ) {
        ## INPUTS: flist - a list of the files to read into corpus before creating dfm
        ##
        ## OUTPUT: a document-frequency matrix from quanteda
        library(quanteda)
     
        gettxt <- sapply(flist, readselect)
        mycorpus1 <- corpus(unname(unlist(gettxt[1])))
        metadoc(mycorpus1, "docsource") <- c("blog data")
        mycorpus2 <- corpus(unname(unlist(gettxt[2])))
        metadoc(mycorpus2, "docsource") <- c("news data")
        mycorpus3 <- corpus(unname(unlist(gettxt[3])))
        metadoc(mycorpus3, "docsource") <- c("twitter data")
        
        mycorpus <- mycorpus1 + mycorpus2 + mycorpus3
        mydfm <- dfm(mycorpus, ngrams = ng, remove_punct = TRUE, 
                     remove = c("will", stopwords("english")), concatenator=" ")
        mydfm
}



readselect <- function(fname) {
        ## INPUTS: 
        ##      fname - local file to read 
        ##
        ## OUTPUTS:
        ##      Contents of the file
        library(readr)
        conn <- file(fname, "r")
        seldata <- readLines(conn, skipNul = TRUE)
        close(conn)
        return(seldata)
}

cleanr <- function(sentl) {
        # INPUT:  sentl - list of sentences
        # OUTPUT: Cleaned list of sentences
        #
        
        
        sentl <- gsub("\u0092", "'", sentl)
        # Convert to ASCII
        sentl <- iconv(sentl, "UTF-8", "ASCII", "")  # Drop and chars that don't convert
        
        # Things to clean
        # email addresses and URls
        sentl <- gsub("[[:alnum:]]+\\@[[:alpha:]]+\\.(com|org|edu|gov|uk|ca)", "", sentl)
        
        sentl <- gsub("http:", " ", sentl)
        sentl <- gsub("[[:alnum:]]+\\.[[:alnum:]]+\\.(com|org|edu|gov|uk|ca)", "", sentl)
        sentl <- gsub("[[:alnum:]]+\\.(com|org|edu|gov|uk|ca)", "", sentl)
        #sentl <- gsub("http\\S+\\s*", "", sentl) 
        
        # Remove hyphens, numbers, and symbols
        # make meta tags for time and dates and nums
        
        sentl <- gsub("._+.", " ", sentl)
        
        sentl <- gsub("\"", "", sentl)
        sentl <- gsub('[—!?;:…“”\\"()\\{\\}]+', " ", sentl)
        sentl <-  gsub("&|%|@|#|\\(|\\)|\\$|>|!|\\[", " ", sentl)
        sentl <- gsub("/", " or ", sentl)
        sentl <- gsub(" +- +", " ", sentl)
    
        # Delete 1 char sentences
        dellist <- grep("^.$", sentl)
        if (length(dellist)>0)
                sentl <- sentl[-dellist]
        
        # Delete 1 word sentences
        #dellist <- grep("^([a-zA-Z]+).?$", sentl)
        #if (length(dellist)>0)
        #        sentl <- sentl[-dellist]
        
        # Delete double words
        sentl <- gsub(" +([a-zA-Z]+) +\\1+", " ", sentl)
        
        sentl <- tolower(sentl)
        
        # Profanity - subjective - choose most egregious 
        sentl <- gsub("(fuck|shit|damn|bitch|asshole|faggot|nigger|nigga|cocksuck)", " ", sentl)
      
        # Consider using http://www.bannedwordlist.com/lists/swearWords.csv
        
        # Delete contractions that are hanging...
        sentl <- gsub(" (ers|er|ty|y|ing|n't)( |$)", " ", sentl) # from profanity filter
    
        # expand common mispelled contractions
        sentl <- gsub("( |^)im ", " i'm  ", sentl)
        sentl <- gsub("( |^)ive ", " i've ", sentl)
        sentl <- gsub("( |^)isnt ", " isn't ", sentl)
        sentl <- gsub("( |^)youre ", " you're ", sentl)
        sentl <- gsub("( |^)youve ", " you've ", sentl)
        sentl <- gsub("( |^)youd ", " you'd ", sentl)
        sentl <- gsub("( |^)dont ", " don't ", sentl)
        sentl <- gsub("( |^)doesnt ", " doesn't ", sentl)
        sentl <- gsub("( |^)didnt ", " didn't ", sentl)
        sentl <- gsub("( |^)thats ", " that's ", sentl)
        sentl <- gsub("( |^)havent ", " haven't ", sentl)
        sentl <- gsub("( |^)couldnt ", " couldn't ", sentl)
        sentl <- gsub("( |^)wouldnt ", " wouldn't ", sentl)
        sentl <- gsub("( |^)shouldnt ", " shouldn't ", sentl)
        sentl <- gsub("( |^)theyre ", " they're ", sentl)
        sentl <- gsub("( |^)theyll ", " they'll ", sentl)
        sentl <- gsub("( |^)weve ", " we've ", sentl)
        sentl <- gsub("( |^)theres ", " there's ", sentl)
        sentl <- gsub("( |^)wasnt ", " wasn't ", sentl)
        sentl <- gsub("( |^)youll ", " you'll ", sentl)
        sentl <- gsub("( |^)heres ", " here's ", sentl)
        sentl <- gsub("( |^)hadnt ", " hadn't ", sentl)
        sentl <- gsub("( |^)hasnt ", " hasn't ", sentl)
        sentl <- gsub("( |^)werent ", " weren't ", sentl)
        sentl <- gsub("( |^)theyd ", " they'd ", sentl)
        sentl <- gsub("( |^)whos ", " who's ", sentl)
        sentl <- gsub("( |^)itll ", " it'll ", sentl)
        sentl <- gsub("( |^)arent ", " aren't ", sentl)
        sentl <- gsub("( |^)shes ", " she's ", sentl)
        sentl <- gsub("( |^)hes ", " he's ", sentl)
        
        sentl <- gsub("[[:space:]]+", " ", sentl)  # remove multi spaces 
        return(sentl)
}

multisamplesize <- function() {
        ## wrapper function to profile the different samples
        # getclean()
        
        cfiles <- list.files(pattern = "clean*")
        
        for (i in seq(from = .7, to = .8, by= .1)) {
                
                for (j in 1:3) {
                        # read in clean data and create a subset sample
                        sentences <- readselect(cfiles[j])
                        set.seed(23)
                        lines <- sample(sentences, size = (length(sentences) * i), replace = FALSE)
                        
                        fname <- paste0("small", basename(cfiles[j]))
                        conn <- file(fname, "w")
                        writeLines(lines, con = conn)
                        close(conn)
                }
                Rprof(filename = "prof.data", append = TRUE)
              
                smfiles <- list.files(pattern = "small*")
                mydfm <- makedfm(smfiles)
                Rprof(NULL)
                #conn <- file("prof.data", open = "a")
                print(paste("completed pass for: ", as.character(i)))
                outmsg <- topfeatures(mydfm, n = 40)
                print(outmsg)
                outmsg <- paste("sentences = ", mydfm@Dim[1], "unique words = ", mydfm@Dim[2])
                print(outmsg)
                foo <- topfeatures(mydfm, n= mydfm@Dim[2])
                outmsg <- paste("num with > 5 references = ", length(foo[as.integer(foo) > 5]) )
                print(outmsg)
                #close(conn)
                
        }
       
        
}

