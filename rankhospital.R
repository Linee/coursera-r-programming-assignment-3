# Second function

rankhospital <- function(state, outcome, num = "best") {
   #state <- "MD"
  #outcome <- "heart failure"
  #num <- 5

    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available",
                     stringsAsFactors=FALSE)
    
    ## minimise data frame to columns needed
    HOSPITAL_NAME_COL <- 2
    STATE_COL <- 7
    HEART_ATTACK_COL <- 11
    HEART_FAILURE_COL <- 17
    PNEUMONIA_COL <- 23
    
    # the columns 11-23 are "outcome" and we create an outcome index called "outcome_column"
    outcome_columns <- c("heart attack"=HEART_ATTACK_COL, "heart failure"=HEART_FAILURE_COL, "pneumonia"=PNEUMONIA_COL)
    outcome_column <- outcome_columns[[outcome]]
    data_new <- data[c(HOSPITAL_NAME_COL, STATE_COL, outcome_column)]
    
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
    
    # in this function, we have four variables: hospital, state_code, outcome and num
    # we need to create the rank variable called num
  
    # but first, filter for state  
    state_filtered <- subset(data_new, state_code == state)
    
    # make variable numeric 
    state_filtered$outcome <-as.numeric(state_filtered$outcome)
    
    
    
    
    state_filtered$number_column <- rank(state_filtered$outcome, na.last=TRUE,
                          ties.method="first")
    state_filtered$rank_column <- number_column[[num]]
    
 
    sorted_state_filtered <- state_filtered[order(state_filtered$rank_column),]
    
    # give variables new names
    names(data_new) <- c("hospital", "state_code", "outcome", "num")
 
    # select the row with the outcome defined by "num"
    row_select <- data_new$num

    # select the hospital with the row defined by num
    hospital <- data_new[row_select,1]
    
    return(hospital)
    
    # we create a column index called "number" 
    # The num argument can take values “best”, “worst”, or an integer indicating the ranking 
    # (smaller numbers are better).
    
## Check that state and outcome are valid
## Return hospital name in that state with the given rank 30-day death rate


}

rankhospital("MD", "heart failure", 5)



