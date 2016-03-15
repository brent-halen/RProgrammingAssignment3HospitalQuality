rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ReadData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states <- ReadData[ , 7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ((state %in% states) == FALSE) {
        stop(print("invalid state"))
    }
    else if ((outcome %in% outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    
    ## Subset data with specified state
    new_ReadData <- subset(ReadData, State == state)
    
    ## Subset data with specified outcome
    if (outcome == "heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else {
        outcome_column <- 23
    }
    
    ## Return NA if num > # of hospitals in specified state.
    if (is.numeric(num) == TRUE) {
        if (length(ReadData[,2]) < num) {
            return(NA)
        }
    }
    
    ## Remove NAs from desired outcome column.
    new_ReadData[, outcome_column] <- as.numeric(new_ReadData[,outcome_column])
    bad <- is.na(new_ReadData[, outcome_column])
    Desired_ReadData <- new_ReadData[!bad, ]
    
    ## Rearrange data-frame in descending order of desired outcome. 
    outcome_column_name <- names(Desired_ReadData)[outcome_column]
    hospital_column_name <- names(Desired_ReadData)[2]
    index <- with(Desired_ReadData, order(Desired_ReadData[outcome_column_name], Desired_ReadData[hospital_column_name]))
    Ordered_Desired_ReadData <- Desired_ReadData[index, ]
    
    ## if "num" is either best or worst, re-interpret it to be the appropriate number value.
    if (is.character(num) == TRUE) {
        if (num == "best") {
            num = 1
        }
        else if (num == "worst") {
            num = length(Ordered_Desired_ReadData[, outcome_column])
        }
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate    
    return(Ordered_Desired_ReadData[num, 2])
}