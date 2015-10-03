rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # read the data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # check if parameter outcome is valid
    pos_outcome <- data.frame(outcome=c('heart attack', 'heart failure', 'pneumonia'), col=c(11, 17, 23))
    pos <- match(outcome, pos_outcome$outcome)
    if (is.na(pos)) {
        stop("invalid outcome")
    }
    col = pos_outcome$col[pos]
    
    # determine values for state
    data <- data[data$State == state, ]
    if (nrow(data) == 0) {
        # no data found, invalid state
        stop("invalid state")
    }
    # get just the wanted outcome and convert it to numeric
    data <- data[, c(2, col)]
    data[, 2] <- as.numeric(data[, 2])
    
    # assign new column names to make manipulation easier
    colnames(data) <- c('hname', 'outcome')
    
    # filter out complete cases
    data <- data[complete.cases(data), ]
    
    # to find the best hospital, just sort them in order for wanted outcome
    # and then hospital name (in case of a tie, the hospital are scored in alphabetical order)
    # this took way to much time to find out how this works in R :-( because
    # in my error I tried reverse order )not neccessary, best means lowest rate!
    # newdata <- data[order(data$outcome, data$hname, decreasing=c(T, F)), ]
    newdata <- data[order(data$outcome, data$hname), ]
    
    # until now the same function as best.R, however now comes the logic for
    # getting the rank instead of the "best"
    if (num == 'best') {
        rank <- 1
    } 
    else if (num == 'worst') {
        rank <- nrow(newdata)
    } 
    else if (num <= nrow(newdata)) {
        rank <- num
    }
    else {
        # rank is out of bounds
        return(NA)
    }

    
    # print the best one (the first in the data.frame)
    newdata[rank,1]
}
