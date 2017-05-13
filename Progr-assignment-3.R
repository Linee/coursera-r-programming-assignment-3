## Coursera A Programming Assignment 3 ##

# set working directory
setwd("~/code/coursera-r-programming-assignment-3")

#load data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")


# explore dataset
head(outcome)
str(outcome)
nrow(outcome)
ncol(outcome)
names(outcome)
dim(outcome)

# create histogram of the 30-day death rates from heart attack

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome [,11])

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
# exact message “invalid state”. If an invalid outcome value is passed to best, the 
# function should throw an error via the stop function with the exact message 
# “invalid outcome”.


best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available",
                     stringsAsFactors=FALSE)
    
    ## Check that state and outcome are valid
    states <- data[,7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if ((state %in% states) == FALSE) {
        stop(print("invalid state"))
    }
    else if ((outcome %in% outcomes)== FALSE) {
        stop(print("invalid outcome"))
    }
    # get the data that contains states
    data_state <- subset(data, State == state)
    
    # get either "heart attack", "heart failure", or "pneumonia" from the outcome
    
    if (outcome=="heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else {
        outcome_column <- 23
    }
    # now find the hospitals with the smallest rate of mortality
    columns_rate <- as.numeric(data_state[,outcome_column])
    selected_rows <- which(columns_rate==min(columns_rate))
    
    ## Return hospital name in that state with lowest 30-day death rate
    hospitals <- data_state[selected_rows, 2]
    
    # ties: If there is a tie for the best hospital for a given outcome (meaning there is 
    # more than one hospital with that rate), 
    # then the hospital names should be sorted in alphabetical order and the first hospital 
    # in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” are tied for best, 
    # then hospital “b” should be returned).
    
    if (length(hospitals) > 1) {
        hospitals_ties <- sort(hospitals)
        hospitals_ties[1]
    }
    else {
        hospitals
    }
}


best("TX", "heart failure")

best("MD", "heart attack")

## second version


best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available",
                     stringsAsFactors=FALSE)
    
    ## minimise data frame to columns needed
    data_new <- data[c(2, 7, 11, 17, 23)]
    HOSPITAL_NAME <- 2
    STATE <- 7
    HEART_ATTACK <- 11
    HEART_FAILURE <- 17
    PNEUMONIA <- 23
    # 2 = hospital name
    # 7 = state
    # 11 = heart attack
    # 17 = heart failure
    # 23 = pneumonia
   
    # the columns 11-23 are "outcome" and we create an outcome index called "column_index"
    column_index <- 
        
        df[,c(HOSPITAL_NAME,STATE,column_index)]
    
    outcomes <- c("heart attack"=HEART_ATTACK, "heart failure"=17, "pneumonia"=23)
    
    Hint: if you setup a named vector with something like 
    outcomes <- c(“heart attack”=11, “heart failure”=17, “pneumonia”=23) 
    then you can use that to both test the function argument and select the column. 
    Something like df[, c(2,7,outcomes[outcome])]. Also, when you validate the outcome 
    argument instead of using %in% outcomes you’d use %in% names(outcomes). 
    
    
    
    
    # give variables new names
    names(data_new) <- c(“hospital”, “state”, “outcome”).
    
    ## Check that state and outcome are valid
    states <- data[,7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if ((state %in% states) == FALSE) {
        stop(print("invalid state"))
    }
    else if ((outcome %in% outcomes)== FALSE) {
        stop(print("invalid outcome"))
    }
   
 
    
    
    
    # get either "heart attack", "heart failure", or "pneumonia" from the outcome
    
    if (outcome=="heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else {
        outcome_column <- 23
    }
    
    
    # now find the hospitals with the smallest rate of mortality
    columns_rate <- as.numeric(data_state[,outcome_column])
    selected_rows <- which(columns_rate==min(columns_rate))
    
    ## Return hospital name in that state with lowest 30-day death rate
    hospitals <- data_state[selected_rows, 2]
    
    # ties: If there is a tie for the best hospital for a given outcome (meaning there is 
    # more than one hospital with that rate), 
    # then the hospital names should be sorted in alphabetical order and the first hospital 
    # in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” are tied for best, 
    # then hospital “b” should be returned).
    
    if (length(hospitals) > 1) {
        hospitals_ties <- sort(hospitals)
        hospitals_ties[1]
    }
    else {
        hospitals
    }
}


best("TX", "heart failure")

best("MD", "heart attack")
