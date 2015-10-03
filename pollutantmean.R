pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!

    # initialize empty vector of correct type to hold pollutant values
    points <- vector("numeric", length=0)
    for (i in id) {
        # construct filename to be opened
        filename <- paste(directory, '/', formatC(i, width=3, flag="0"), '.csv', sep='')
        data <- read.csv(filename)
        
        # slice the wanted pollutant, determine NA values and strip them
        newdata <- data[[pollutant]]
        bad <- is.na(newdata)
        newdata <- newdata[!bad]
        
        # add new pollutant data to points
        points <- c(points, newdata)
    }
    # determine mean value over all points
    result <- mean(points)
    # evaluate result as return value of function
    result
}