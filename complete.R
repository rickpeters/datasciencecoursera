complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    nobs = vector("logical", length=0)    
    for (i in id) {
        # construct filename to be opened
        filename <- paste(directory, '/', formatC(i, width=3, flag="0"), '.csv', sep='')
        data <- read.csv(filename)
        #print(data)
        good <- complete.cases(data)
        #print(good)
        #print(data$sulfate[good])
        
        nobs <- c(nobs, length(data[[1]][good]))
        
    }
    x <- data.frame(id=id, nobs=nobs)
    x
}