best <- function(state, outcome) {
    
    directory <- "data"
    outcomes_data <- c("heart attack", "heart failure","pneumonia")
    
    
    for (letters in list.files()) {
        if(identical(letters, directory)){
            setwd(directory)
        }
    }
    
    
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    
    if(!(state %in% outcome_data$State)) {
        stop("invalid state")
    }
    
    
    if(!any(sapply(outcomes_data, function(x) identical(x, outcome)))) {
        stop("invalid outcome")
    }
    
    
    outcome_column <- NULL
    
    if(outcome == outcomes_data[1]) {
        outcome_column <- 11 
    } else if(outcome == outcomes_data[2]) {
        outcome_column <- 17
    } else {
        outcome_column <- 23
    }
    
    
    outcome_data_state <- outcome_data[outcome_data[,7] == state,]
    
    min_indexes <- which.min(as.double(outcome_data_state[,outcome_column]))
    
    sort(as.character(outcome_data_state[min_indexes, 2]))[1]
}