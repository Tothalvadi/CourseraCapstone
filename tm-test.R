library(tm)
tmpfile <- readLines("./Data/samples/en_US/en_US.blogs.txt", encoding = "en_US")

tmp  <- VCorpus(VectorSource(tmpfile)
               , readerControl = list(reader=readPlain, language = "en_US"))

rm(tmpfile)

tmp <- tm_map(tmp, removeNumbers)
tmp <- tm_map(tmp, removePunctuation)
tmp <- tm_map(tmp, content_transformer(tolower))


#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/