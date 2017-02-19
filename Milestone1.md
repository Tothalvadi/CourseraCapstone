---
title: "Milestone report 1"
author: "Tom van Dienst"
date: "February 18, 2017"
output: html_document
---


```r
knitr::opts_chunk$set(echo = TRUE)
```


```r
library(R.utils)
library(tm)
library(dplyr)
library(slam)
library(ggplot2)
library(RWeka)
library(parallel)

options(mc.cores=1)
```


# Introduction

For the Data Science capstone project we'll be building a word prediction model based on a set of documents. 
In this report I'll perform the loading of the documents and will perform an exploratory data analysis. 

# Loading the data 

## Downloading

First off we'll need to download the necessary files. 


```r
#Set the URL for the download
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip" 
#To prevent unnecessary downloading, only perform this if the final directory is not available
if(!dir.exists("./Data/final"))
{
    download.file(url, destfile = "./Data/Coursera-Swiftkey.zip", method = 'curl', quiet=TRUE)
    unzip("./Data/Coursera-Swiftkey.zip")
}
```

Let's take a look at what we downloaded: 


```r
writeLines(dir('./Data/final', recursive=TRUE))
```

```
## de_DE/de_DE.blogs.txt
## de_DE/de_DE.news.txt
## de_DE/de_DE.twitter.txt
## en_US/en_US.blogs.txt
## en_US/en_US.news.txt
## en_US/en_US.twitter.txt
## fi_FI/fi_FI.blogs.txt
## fi_FI/fi_FI.news.txt
## fi_FI/fi_FI.twitter.txt
## ru_RU/ru_RU.blogs.txt
## ru_RU/ru_RU.news.txt
## ru_RU/ru_RU.twitter.txt
```

There are a total of twelve different documents divided over four languages: English (EN), German (DE), Finnish (FI) and Russion (RU). For this report we'll only be focusing on the English documents. These range in size between 167 and 225 MB. So how many lines are we talking about here? Let's take the blog file as an example.


```r
countLines('./Data/final/en_US/en_US.blogs.txt')
```

```
## [1] 899288
## attr(,"lastLineHasNewline")
## [1] TRUE
```

Wow, that's a lot. Let's take a look at the first few lines of the English blog, to see what we're dealing with: 


```r
writeLines(readLines('./Data/final/en_US/en_US.blogs.txt')[1:3])
```

```
## In the years thereafter, most of the Oil fields and platforms were named after pagan “gods”.
## We love you Mr. Brown.
## Chad has been awesome with the kids and holding down the fort while I work later than usual! The kids have been busy together playing Skylander on the XBox together, after Kyan cashed in his $$$ from his piggy bank. He wanted that game so bad and used his gift card from his birthday he has been saving and the money to get it (he never taps into that thing either, that is how we know he wanted it so bad). We made him count all of his money to make sure that he had enough! It was very cute to watch his reaction when he realized he did! He also does a very good job of letting Lola feel like she is playing too, by letting her switch out the characters! She loves it almost as much as him.
```

It's clear that every line has no connection to the previous. We can assume that every line in this file is a separate sample and thus a document in its own right. 

## Sampling

Of course a textfile of this size will not fit in the memory of most computers. We'll be making use of sample files to bypass this problem. A 5% sample should be sufficient for analyses purposes. This will still give us a large number of samples per file to work with.
For this purpose I've written two functions. The first one will take a sample of a file and write it at a given location. It could be possible to read the file in chunks, but I figured a 200+ MB file should pose no problems for a computer with 8 GB memory. 


```r
sampleFile <- function(source, destination)
{
    con <- readLines(source) #Read the file in memory
     #Create a sample based on a vector of random binominal values with a 10 percent chance of success. 
    sample <- con[rbinom(length(con), 1, 0.05) > 0 ]
    rm(con) #Remove the file from memory
    writeLines(sample, destination) #Write the sample to its destination
}
```

Next we'll also write a function to trawl through the different documents and call the sampleFile function for each of them. It takes a sourcedir variable with the root folder for all the documents and a destdir folder with the root for where all files should be written to. 



```r
processFiles <- function(sourcedir, destdir)
{
    if(!dir.exists(destdir))
        dir.create(destdir)
    
    #This function section will create the subfolders if they're not present. 
    dirs <- list.dirs(sourcedir) 
    dirs <- dirs[nchar(dirs) > 0]
    dirs <- paste(destdir,dirs, sep='/')
    #Invisible to prevent return values
    invisible(sapply(dirs, function(x){if(!dir.exists(x))dir.create(x)})) 
    
    #Find all text files hidden within the subfolders
    files <- dir(sourcedir, "*.txt", recursive = T) 
    
    for( i in 1:length(files)) #For each found list
    {
        #Create a full source filepath
        source <- paste(sourcedir, files[i], sep='/') 
        #Create a full destination filepath
        destination <- paste(destdir, files[i], sep='/') 
        #Call the sampleFile function to process.
        sampleFile(source, destination)  
    }
    
}
```

Next we set the seed and start with our sampling. 



```r
#We'll set a seed for reproducibility
set.seed(12345)

processFiles('./Data/final', './Data/samples')
```

We'll now have a set of sample files only 5% of their original size. This should be small enough for general analysis purposes.

# Analysis

For our analysis, we'll be focussing on just the English files. We'll create a corpus (a collection of documents) of all the English files (blogs, tweets and news messages). Because each line is a separate entity, we'll be adding each line as a separate document in the corpus. 

## Corpus

Since we want each line to be a separate document, we will first need to split up each file. To prevent any unnecessary strain on the harddrive, we'll process each file in memory before feeding it to the corpus. 
For this purpose a special function is written. I prefer to write everything in functions, it allows for reusability in the future. 


```r
#Parameter is the language folder where the documents are located
createCorpus <- function(sourcedir) 
{
    #Extract the language from the subfolder (Last folder)
    lan <- tail(strsplit(sourcedir, '/')[[1]], 1)
    
    #Assemble a vector with the files and their full pathnames
    sourcedocs <- list.files(sourcedir, full.names = T )
    #Assemble a second vector with just the filenames
    sourcefile <- list.files(sourcedir, full.names = F )
    #Create a dummy variable. We'll combine the corpus when ready. 
    corp <- NULL
    #For each document
    for(i in 1:length(sourcedocs))
    {
        #Read the files into a variable
        corpfile <- readLines(sourcedocs[i], encoding = lan)
        #Create a temporary corpus file based on the corpfile Vector
        corptemp  <- VCorpus(VectorSource(corpfile)
                         , readerControl = list(reader=readPlain, language = lan))
        
        #Add the filename to the metadata
        meta(corptemp, "source") <- sourcefile[i]
        #If it's the first document then just create the corpus
        if(i == 1)
            corp <- corptemp
        #Otherwise combine both of them to one large corpus
        else
            corp <- c(corp, corptemp)
        
    }
    #Return corp value
    corp
}

#Call the function
corp <- createCorpus('./Data/samples/en_US')
#Display basic information
corp
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 1
## Content:  documents: 213710
```

Our corpus shows that it contains over 200,000 documents. Since we added the sourcefile to the metadata during creation, we can even see from which sourcefile the data came from: 


```r
group_by(meta(corp), source) %>% summarise(total_docs = n())
```

```
## # A tibble: 3 × 2
##              source total_docs
##               <chr>      <int>
## 1   en_US.blogs.txt      45216
## 2    en_US.news.txt      50308
## 3 en_US.twitter.txt     118186
```

## Cleaning
A problem with language is that it's not structured. A lot of junk can be inserted. Let's consider the goal of this exercise. We intend to create a body of text to base our predictions on. This means that we don't need: 

*   Numbers
*   Punctuation
*   Twitter tags

But because we want to predict a written language, we can't shorten or remove words. It's tempting to shorten our list of possible words by removing stopwords or "stemming" words (shortening similar words to their common root). 

For now we'll clean up our text with the following actions: 

*   Change the characters :, - and / to space. 
*   Bring all words to lower case
*   Remove all numbers
*   Remove all punctuation

First we'll create a custom function to change certain patterns to a space. This function is not mine but copied from the helpful blog [Eight2Late][1]. The idea is that some people tend to write words like "Therefore I say:Hi!". Without cleaning this up, tm will transform this as "therefore i sayhi". By first replacing the colon with a space, we'll preserve the words in their intended form. 


```r
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
```

So now let's clean up our text: 


```r
corp <- tm_map(corp, toSpace, "-")
corp <- tm_map(corp, toSpace, ":")
corp <- tm_map(corp, toSpace, "/")
```

Next we need to clean up our twitter documents. All those hashtags and handles need to be replaced.  
We'll make two new functions and apply them to our corpus. The regex expression is adapted from this [stackoverflow post][2]


```r
labelHandle <- content_transformer(function(x) {return (gsub("\\S*@(?:\\[[^\\]]+\\]|\\S+)", "twitterhandle", x))})
labelHashtag <- content_transformer(function(x) {return (gsub("\\S*#(?:\\[[^\\]]+\\]|\\S+)", "twitterhashtag", x))})

corp <- tm_map(corp, labelHandle)
corp <- tm_map(corp, labelHashtag)
```

And finally we'll remove numbers, punctuation and bring all our text to lowercase. 


```r
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
```

## Document Term Matrix

It's nice that we have our words cleaned up, but we know very little about what we have without looking at specific samples. To fix this we'll create a DocumentTermMatrix. A sparse matrix with a column for each unique word and a row for each document. 

```r
corpdtm <- DocumentTermMatrix(corp)
corpdtm 
```

```
## <<DocumentTermMatrix (documents: 213710, terms: 121580)>>
## Non-/sparse entries: 3469719/25979392081
## Sparsity           : 100%
## Maximal term length: 322
## Weighting          : term frequency (tf)
```

We got a collection of 213710 documents, which matches our corpus, and we have 121580 terms, in this case distinct words. That's a lot of words, I'm sure there are some words that are rarely used. Let's take a look at what we've got. 


```r
dat <- data.frame(word = colnames(corpdtm), total = col_sums(corpdtm), mean = col_means(corpdtm))
dat <- arrange(dat, desc(total))
```

Our top 5 words are: 


```r
head(dat, 5)
```

```
##   word  total      mean
## 1  the 237116 1.1095222
## 2  and 119352 0.5584764
## 3  for  54846 0.2566375
## 4 that  51919 0.2429414
## 5  you  46798 0.2189790
```

This isn't unexpected. "The"" is the most popular word and appears on average at least once in every document. One thing that's quite clear is that the frequency of the word is rapidly dropping. Let's take a look at how fast this drops, we'll present it in log10. 

























