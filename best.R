best <- function(state, outcome) {
    
    if(!is.element(state, state.abb)) {
        stop("invalid state")
    }
    
    conditions <- c("heart attack", "heart failure", "pneumonia")
    
    cidx <- match(outcome, conditions)
    if(is.na(cidx)) {
        stop("invalid outcome")
    }
    
    tidx <- c(11, 17, 23)[cidx]
    
    db <- read.csv("data/outcome-of-care-measures.csv")
    
    hospitals <- db[db[,7] == state,] # get just the hospitals in our state
    
    hospitals[,tidx] <- as.numeric(hospitals[,tidx]) # make the rate numeric so . . .
    best_rate <- min(hospitals[,tidx]) # we can find the best rate.
    best_list <- hospitals[hospitals[,tidx] == best_rate,] # make a list of "best" hospitals
    sorted <- best_list[order(best_list[,'Hospital.Name']),] # bug here
    as.character(sorted[1,'Hospital.Name'])
}