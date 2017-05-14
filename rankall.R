# Write a function called rankall that takes two arguments: an outcome name (outcome) and a 
# hospital rank- ing (num). The function reads the outcome-of-care-measures.csv file and returns 
#a 2-column data frame containing the hospital in each state that has the ranking specified in num. 
#For example the function call rankall("heart attack", "best") would return a data frame containing 
#the names of the hospitals that are the best in their respective states for 30-day heart attack 
#death rates. The function should return a value for every state (some may be NA). The first column 
#in the data frame is named hospital, which contains the hospital name, and the second column is named 
#state, which contains the 2-character abbreviation for the state name. Hospitals that do not have 
#data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.


rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available",
                     stringsAsFactors=FALSE)
    
    ## minimise data frame to columns needed
    HOSPITAL_NAME_COL <- 2
    STATE_COL <- 7
    HEART_ATTACK_COL <- 11
    HEART_FAILURE_COL <- 17
    PNEUMONIA_COL <- 23
    
    ## Check that state and outcome are valid
    states <- data[,STATE_COL]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% states)) {
        stop(print("invalid state"))
    }
    if (!(outcome %in% outcomes)) {
        stop(print("invalid outcome"))
    }
    
    ## For each state, find the hospital of the given rank
    
    
    #if num is greater that the number of hospitals in the desired state,
    # return NA
    if (is.numeric(num) == TRUE) {
        if (length(data[,2]) < num) {
            return(NA)
        }
    }
    
    #if nume is either "best" or "worst", then interpret it to the
    #corresponding numerical value
    if (is.character(num) == TRUE) {
        if (num == "best") {
            num = 1
        }
        else if (num == "worst") {
            num = length(ordered_desired_data[, outcome_column])
        }
    }
    
    ## Return a data frame with the hospital names and the state name 
    
    
    
}

