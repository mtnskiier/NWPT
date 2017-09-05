# NWPT
Next Word Prediction Tool - JHU Capstone

## Directory CC
This directory contains source files created for the Cleaning and Conditioning of the corpus. 

#### SGTprocess.R - Simple Good Turing smoothing
- Incomplete implementation
- Code not used in submitted solution
- [Pointer to the supporting paper ]( https://www.grsampson.net/AGtf1.html)

#### isbo.R - interpolation using the quad-unigrams
- Incomplete implementation
- No automated process to determine appropriate lambda values
- Interpolation got good reviews

#### mkcorpus.Rmd - Overview of creating the corpus and n-grams

#### mkmodel.R - break n-grams into named tokens

#### n-grams.R - sampling algorithms for creating the training, testing, and validation sets. 
- For complete usage of the functions, see mkcorpus.Rmd


## Directory COCA
This directory includes routines for processing the n-grams from the:  

Davies, Mark. (2011) N-grams data from the Corpus of Contemporary American English (COCA). 
Downloaded from http://www.ngrams.info on August 4, 2017. 

#### COCO_process.R - read and format n-grams

