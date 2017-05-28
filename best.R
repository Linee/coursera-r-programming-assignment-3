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
    
    ## Create constants based on column position
    HOSPITAL_NAME_COL <- 2
    STATE_COL <- 7
    HEART_ATTACK_COL <- 11
    HEART_FAILURE_COL <- 17
    PNEUMONIA_COL <- 23
    
    # Pick outcome column based on the outcome passed into the function
    outcome_columns <- c("heart attack"=HEART_ATTACK_COL, "heart failure"=HEART_FAILURE_COL, "pneumonia"=PNEUMONIA_COL)
    outcome_column <- outcome_columns[[outcome]]
    
    # Create data_new, a collapsed data frame with 3 columns - hospital name, state, outcome
    data_new <- data[c(HOSPITAL_NAME_COL, STATE_COL, outcome_column)]
    
    # Rename the three columns above
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
    
    # Filter data_new and store only data releveant to state in state_filtered data frame
    state_filtered <- subset(data_new, state_code == state)
    
    # make variable numeric 
    state_filtered$outcome <-as.numeric(state_filtered$outcome)
    
    # select the row with lowerst outcome
    min_row <-which.min(state_filtered$outcome)
    
    # order/rank outcome
    ordered_state_filtered <- state_filtered[order(state_filtered$outcome, state_filtered$hospital), ]
    
    # select the hospital with lowest outcome row
    hospital <- ordered_state_filtered[1,1]
    return(hospital)
}

#best("TX", "heart attack")
# "CYPRESS FAIRBANKS MEDICAL CENTER"
#best("TX", "heart failure")
# "FORT DUNCAN MEDICAL CENTER"
#best("MD", "heart attack")
# "JOHNS HOPKINS HOSPITAL, THE"
#best("MD", "pneumonia")
# "GREATER BALTIMORE MEDICAL CENTER"
#best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
#best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
