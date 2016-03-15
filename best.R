best <- function(state, outcome) {
    ##outcome_name: "heart attack", "heart failure", "pneumonia"
    
    
    ## Read outcome data
    ReadData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    ## Subset data by states.
    states <- ReadData[ , 7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ((state %in% states) == FALSE) {
        stop(print("invalid state"))
    }
    else if ((outcome %in% outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    
    ## Subset the data by state
    New_Data <- subset(ReadData, State == state)
    
    ## Set the desired column by outcome. 
    if (outcome == "heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else {
        outcome_column <- 23
    }
    
    ## Remove NA values in outcome column
    ## This reduces some of the NAs introduced by coersion, but not all. 
    Required_Data <- as.numeric(New_Data[,outcome_column])
    NA_key <- is.na(Required_Data)
    desired_data <- New_Data[!NA_key, ]

    ## Find hospitals with minimum outcome values
    columns_considered <- as.numeric(desired_data[, outcome_column])
    desired_rows <- which(columns_considered == min(columns_considered))
    desired_hospitals <- desired_data[desired_rows, 2]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    ## If there's more than one entry in the vector of top-ranked hospitals, the hospitals will be ranked
    ## alphabetically, and the earliest one in that order will be displayed.
    if (length(desired_hospitals) > 1) {
        hospitals_sorted <- sort(desired_hospitals)
        hospitals_sorted[1]
    }
    ## Otherwise, the hospital is displayed. 
    else {
        desired_hospitals
    }
}