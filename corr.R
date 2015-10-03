corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    # initialize empty vector for result
    cors = vector("numeric", length=0)    
    
    # first reuse our earlier function complete, being lazy I use the fact that
    # I now the number of stations
    nobs = complete(directory, 1:332)

    # select just the stations that have nobs above treshold
    stations = nobs[nobs$nobs > threshold, ]
    #print(stations$id)
    for (i in stations$id) {
        # construct filename to be opened
        filename <- paste(directory, '/', formatC(i, width=3, flag="0"), '.csv', sep='')

        # read the data and clean it up
        data <- read.csv(filename)
        good <- complete.cases(data)
        data <- data[good, ]

        # determin correlation between sulfate and nitrate
        c <- cor(data[, "sulfate"], data[, "nitrate"])
        # add it to the result vector
        cors <- c(cors, c)
    }
    # evaluate vector to get a result
    cors
}