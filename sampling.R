set.seed(8008135)

sampleFile <- function(source, destination)
{
    con <- readLines(source)
    sample <- con[rbinom(length(con), 1, 0.01) > 0 ]
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

processFiles('./Data/final', './Data/samples')
