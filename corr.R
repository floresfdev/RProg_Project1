corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    correlations <- numeric()
    
    vectorSulfate <- numeric()
    vectorNitrate <- numeric()
    
    countIterator <- 1
    
    #filenames <- list.files(directory, pattern="*.csv")
    
    for (fileNumber in 1:332) {
        fileName <- paste(directory, "/", sprintf("%03d", fileNumber), ".csv", sep = "")
        data <- read.csv(fileName)
        
        checkingDataFrame <- complete(directory, fileNumber)
        
        allCompleteCases <- complete.cases(data)
        dataToProcess <- data[allCompleteCases, ]

        
        if (checkingDataFrame[, 2] > threshold) {
            #vectorSulfate[countIterator] <- dataToProcess[["sulfate"]]
            #vectorNitrate[countIterator] <- dataToProcess[["nitrate"]]
            
            #vectorSulfate <- c(vectorSulfate, dataToProcess[["sulfate"]])
            #vectorNitrate <- c(vectorNitrate, dataToProcess[["nitrate"]])
            
            correlations <- c(correlations, cor(x = dataToProcess[["sulfate"]], y = dataToProcess[["nitrate"]], use = "pairwise.complete.obs"))
            countIterator <- countIterator + 1
        }
        
        
    }
    
    #if(length(vectorSulfate) >= 1) {
    #    correlations <- cor(x = vectorSulfate, y = vectorNitrate, use = "pairwise.complete.obs")
    #} else {
        correlations
    #}
}
