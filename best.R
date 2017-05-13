# write a function called "best" that takes two arguments: the name of state (2-character
# abbreviation "State") and an outcome name

# function reads the outcome-of-care-measures.csv file and returns a character vector
# that has the best 30-day morality for the specified outcome in that state.
# the hospital name is in the variable "Hospital.Name"

# outcome can be either "heart attack", "heart failure" or "pneumonia"
# Hospitals that do not have data on a particular outcome should be excluded from
# the set of hospitals when deciding the rankings (via na.remove?)

# The function should check the validity of its arguments. If an invalid state value 
# is passed to best, the function should throw an error via the stop function with the 
# exact message “invalid state”.

best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available",
                     stringsAsFactors=FALSE)
    
    ## minimise data frame to columns needed
    HOSPITAL_NAME_COL <- 2
    STATE_COL <- 7
    HEART_ATTACK_COL <- 11
    HEART_FAILURE_COL <- 17
    PNEUMONIA_COL <- 23
    
    
    # the columns 11-23 are "outcome" and we create an outcome index called "column_index"
    #column_index <- 
    
    #    df[,c(HOSPITAL_NAME,STATE,column_index)]
    
    outcome_columns <- c("heart attack"=HEART_ATTACK_COL, "heart failure"=HEART_FAILURE_COL, "pneumonia"=PNEUMONIA_COL)
    outcome_column <- outcome_columns[[outcome]]
    data_new <- data[c(HOSPITAL_NAME_COL, STATE_COL, outcome_column)]
    
    
    #Hint: if you setup a named vector with something like 
    # outcomes <- c(“heart attack”=11, “heart failure”=17, “pneumonia”=23) 
    # then you can use that to both test the function argument and select the column. 
    # Something like df[, c(2,7,outcomes[outcome])]. Also, when you validate the outcome 
    # argument instead of using %in% outcomes you’d use %in% names(outcomes). 
    
    # give variables new names
    names(data_new) <- c("hospital", "state_code", "outcome")
    
    ## Check that state and outcome are valid
    states <- data[,STATE_COL]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% states)) {
        stop(print("invalid state"))
    }
    if (!(outcome %in% outcomes)) {
        stop(print("invalid outcome"))
    }
    
    # filter for state 
    state_filtered <- subset(data_new, state_code == state)
    
    # make variable numeric 
    state_filtered$outcome <-as.numeric(state_filtered$outcome)
    
    # select the row with lowerst outcome
    min_row <-which.min(state_filtered$outcome)
    # select the hospital with lowest outcome row
    hospital <- state_filtered[min_row,1]
    return(hospital)
    
    # ties: If there is a tie for the best hospital for a given outcome (meaning there is 
    # more than one hospital with that rate), 
    # then the hospital names should be sorted in alphabetical order and the first hospital 
    # in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” are tied for best, 
    # then hospital “b” should be returned).
    
    if (length(hospital) > 1) {
        hospitals_ties <- sort(hospital)
        hospitals_ties[1]
    }
    else {
        hospital
    }
}
