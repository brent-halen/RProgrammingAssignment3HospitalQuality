rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ReadData <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    states <- ReadData[ , 7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ((outcome %in% outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    
    
    ## Initialize a character vector to hold the results of the upcoming 'for' loop. 
    state <- levels(factor(ReadData[, 7]))
    hospital <- vector(mode="character")
    ## For each state, find the hospital of the given rank
    for (i in seq(state)) {
        hospital[i] <- rankhospital(state[i], outcome, num)
        
    }
    ## Return a data frame with the hospital names and the abbreviated state name
    return(data.frame(hospital, state))
}
