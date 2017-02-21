


url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip" 
if(!dir.exists("./Data/final"))
{
    download.file(url, destfile = "./Data/Coursera-Swiftkey.zip", method = 'curl', quiet=TRUE)
    unzip("./Data/Coursera-Swiftkey.zip")
}


sampleFile <- function(source, destination)
{
    con <- readLines(c <- file(source, encoding = "UTF-8"))
    sample <- con[rbinom(length(con), 1, 0.05) > 0 ]
    close(c)
    rm(con)
    writeLines(sample, destination)
}


processFiles <- function(sourcedir, destdir)
{
    files <- dir(sourcedir, "*.txt", recursive = T)
    
    for( i in 1:length(files))
    {
        
        source <- paste(sourcedir, files[i], sep='/')
        destination <- paste(destdir, files[i], sep='/')
        print(paste("Processing file", source))
        sampleFile(source, destination)
    }
    
}
set.seed(12345)
processFiles('./Data/final', './Data/samples')
