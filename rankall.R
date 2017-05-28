
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available",
                     stringsAsFactors=FALSE)
    
    ## Create constants based on column position
    HOSPITAL_NAME_COL <- 2
    STATE_COL <- 7
    HEART_ATTACK_COL <- 11
    HEART_FAILURE_COL <- 17
    PNEUMONIA_COL <- 23
    
    ## Check that state and outcome are valid
    states <- unique(data[,STATE_COL])
    
    states <- states[order(states)]
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% states)) {
        stop(print("invalid state"))
    }
    if (!(outcome %in% outcomes)) {
        stop(print("invalid outcome"))
    }
    
    # Pick outcome column based on the outcome passed into the function
    outcome_columns <- c("heart attack"=HEART_ATTACK_COL, "heart failure"=HEART_FAILURE_COL, "pneumonia"=PNEUMONIA_COL)
    outcome_column <- outcome_columns[[outcome]]
    
    # Create data_new, a collapsed data frame with 3 columns - hospital name, state, outcome
    data_new <- data[c(HOSPITAL_NAME_COL, STATE_COL, outcome_column)]
    
    # Rename the three columns above
    names(data_new) <- c("hospital", "state_code", "outcome")
    hospitals <- NULL
    ## For each state, find the hospital of the given rank
    for(state in states) {
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
        
        # print(hospital)
        rbind(hospitals, c(hospital, state)) -> hospitals

       
    }
    
    names(hospitals) <- c("hospital", "state")
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(hospitals)
    
}

rankall("heart attack", "best")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
