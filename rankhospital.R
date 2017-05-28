
rankhospital <- function(state, outcome, num=1) {
    
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
    
    # filter out NAs
    state_filtered <- state_filtered[!is.na(state_filtered$outcome),]
    
    # order/rank outcome
    ordered_state_filtered <- state_filtered[order(state_filtered$outcome, state_filtered$hospital), ]
    
    if (num == "best") {
        # case - num is "best"
        hospital <- ordered_state_filtered[1,1]
    } else if (num == "worst") {
        hospital <- ordered_state_filtered[nrow(ordered_state_filtered),1]
    } else {
        # case - num is a number
        # select the hospital with ranking 
        hospital <- ordered_state_filtered[num,1]
    }
    
    return(hospital)
}

#rankhospital("TX", "heart attack", 2)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("TX", "heart failure", 4)
#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
