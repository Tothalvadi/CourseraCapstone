library(tm)

createCorpus <- function(sourcedir)
{
    sourcedocs <- list.files(sourcedir )
    sourcedocs <- paste(sourcedir, sourcedocs, sep='/')
    
    corp <- NULL
    for(i in 1:length(sourcedocs))
    {
        corpfile <- readLines(sourcedocs[i], encoding = "en_US")
        corptemp  <- VCorpus(VectorSource(corpfile)
                         , readerControl = list(reader=readPlain, language = "en_US"))
        if(i == 1)
            corp <- corptemp
        else
            corp <- c(corp, corptemp)
        
    }
    corp
}

corpfile <- readLines("./Data/samples/en_US/en_US.blogs.txt", encoding = "en_US")

corp  <- VCorpus(VectorSource(corpfile)
               , readerControl = list(reader=readPlain, language = "en_US"))

rm(corpfile)

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

corp <- tm_map(corp, toSpace, "-")
corp <- tm_map(corp, toSpace, ":")
corp <- tm_map(corp, toSpace, "/")
corp <- tm_map(corp, toSpace, "  ")
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))


#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

corptdm <- TermDocumentMatrix(corp)

tdmFiltered <- removeSparseTerms(corptdm, 2 / 45321)

temp <- inspect(removeSparseTerms(corptdm, 0.995))
fr <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(fr) <- NULL
