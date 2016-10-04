rankall <- function(outcome, num = "best") {
    
    directory <- "data"
    outcomes_data <- c("heart attack", "heart failure", "pneumonia")
    nums_data <<- c("best", "worst")
    
    for (letters in list.files()) {
        if(identical(letters, directory)){
            setwd(directory)
        }
    }
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    # check function args
    
    if(!any(sapply(outcomes_data, function(x) identical(x, outcome)))) {
        stop("invalid outcome")
    }
    
    if(class(num) == "character"){
        if(!(num %in% nums_data)){
            stop("invalid num")
        }
    } else if(class(num) == "numeric"){
        if(num <= 0){
            stop("invalid num")
        }
    } else {
        stop("invalid num")
    }
    
    # select column
    
    outcome_column <- NULL
    
    if(outcome == outcomes_data[1]) {
        outcome_column <- 11 
    } else if(outcome == outcomes_data[2]) {
        outcome_column <- 17
    } else {
        outcome_column <- 23
    }
    
    
    states <- sort(unique(data[!is.na(data[,7]),7]))
    
    hospitals <- character(0)
    
    for(i in seq_along(states)){
        
        if(is.na(states[i])){
            stop("state is NA")
        }
        
        hospitals[i] <- rank_hospital_internal(data, states[i], outcome_column, num)
    }
    
    data.frame(hospital = hospitals, state = states, row.names = states)
}

rank_hospital_internal <- function(data, state, outcome_column, num) {
    
    if(is.na(state)) {
        stop("state is NA")
    }
    
    data_state <- data[data[,7] == state,]
    
    if(class(num) == "numeric"){
        if(length(unique(data_state[,2])) < num){
            return(NA)
        }
    }
    
    data_state_sorted <- data_state[order(as.numeric(data_state[,outcome_column]), data_state[,2], decreasing = FALSE, na.last = NA),] 
    
    row <- NULL
    
    if (num == nums_data[1]) { 
        row = 1
    }
    else if (num == nums_data[2]) {
        row = nrow(data_state_sorted)  
    } else {
        row <- num
    }
    
    data_state_sorted[row, 2]
}



















