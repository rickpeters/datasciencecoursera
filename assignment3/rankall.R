rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    # check if parameter outcome is valid
    pos_outcome <- data.frame(outcome=c('heart attack', 'heart failure', 'pneumonia'), col=c(11, 17, 23))
    pos <- match(outcome, pos_outcome$outcome)
    if (is.na(pos)) {
        stop("invalid outcome")
    }
    col = pos_outcome$col[pos]

    # read data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # determine all states in the dataset
    hospitalinfo <- read.csv("hospital-data.csv", colClasses = "character")
    statelist <- levels(factor(hospitalinfo$State))
    
    # now I need to rank according to outcome and be able to select a state
    # let's sort the complete dataset on outcome and hospitalname
    # get just the wanted columns (hospitalname and outcome) and convert it to numeric
    data <- data[, c(2, 7, col)]
    data[, 3] <- as.numeric(data[, 3])
    
    # assign new column names to make manipulation easier
    colnames(data) <- c('hname', 'State', 'outcome')
    
    # filter out complete cases
    data <- data[complete.cases(data), ]
    
    # to find the best hospital, just sort them in order for wanted outcome
    # and then hospital name (in case of a tie, the hospital are scored in alphabetical order)
    newdata <- data[order(data$outcome, data$hname), ]
    

    # determine ranknumber
    if (num == 'best') {
        num <- 1
    } 

    # now let's loop across the states and determine hospital at rank for each state
    # first create the empty resultset, assume value is NA initially
    result <- data.frame(hospital=NA, state=statelist)
    for (state in statelist) {
        # select values from specific state from data
        statedata <- newdata[newdata$State == state, ]
        #print(head(statedata))
        # determine number of rows in statedata
        numranks <- nrow(statedata)
        if (num == 'worst') {
            # take the last value from statedata if data exists
            if (numranks) {
                result[result$state == state, 1] <- statedata[numranks, 1]
            }
        }
        else if (num <= numranks) {
            result[result$state == state, 1] <- statedata[num, 1]
        }
    }
    result
}