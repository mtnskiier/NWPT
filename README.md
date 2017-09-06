# NWPT - Next Word Prediction Tool - JHU Capstone
The next word prediction tool is a natural language processing model built to predict the next work in written English. The basic algorithm builds a model from a large corpus of blobs, twitter, and news articles - provided by SwiftKey. The model is build from counting the frequency of occurrence for n-grams (where n is 1-4) and then using a Simple Back-off algorithm to try and predict phrases from the higher order n-grams down through the lowest order n-grams. 

## main branch directory (NWPT)

#### Capstone_overview.Rpres - 5 slide overview of project
#### README.md - this file
#### otab.txt - overview tab content for inclusion in shiny app
#### mkcorpus.Rmd - high-level scripting for creating the model
#### sampleCorpus.R - tools for cleaning inputs and reading files
#### sbo.R - Simple back-off code
#### server.R - server backend for shiny app-let
#### ui.R - UI definition for shiny app-let
#### utab.txt - usage tab content for inclusion in shiny app

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

## Directory Ttools
Test tools for the NWPT

#### testtools.R - simple test scaffold for testing prediction algos

