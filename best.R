##
## best returns the hospital name for the lowest mortality rate for an outcome in the state
##  - state: the two letter abbreviation for a state (from state.abb)
##  - outcome: must be "heart attack", "heart failure", "pneumonia"
##

best <- function(state, outcome) {
    
    # check for valid state & outcome
    
    if(!is.element(state, state.abb)) {
        stop("invalid state")
    }
    
    conditions <- c("heart attack", "heart failure", "pneumonia")
    
    cidx <- match(outcome, conditions) # cidx identifies which outcome we want
    if(is.na(cidx)) {
        stop("invalid outcome")
    }
    
    tidx <- c(11, 17, 23)[cidx] # tidx is the table column index for the desired outcome
    
    db <- read.csv("data/outcome-of-care-measures.csv")
    
    hospitals <- db[db[,7] == state,] # get just the hospitals in our state
    
    # the data frame columns holding our mortality rates are factors
    # we need to convert them to numeric, silencing the "NAs introduced by coercion" warning,
    # so that . . .
    
    hospitals[,tidx] <- suppressWarnings(as.numeric(as.character(hospitals[,tidx])))
    best_rate <- min(hospitals[,tidx], na.rm = TRUE) # . . . we can find the best rate.
    
    best_list <- hospitals[hospitals[,tidx] == best_rate,] # make a list of "best" hospitals
    sorted <- best_list[order(best_list[,'Hospital.Name']),] # for ties, first in alphabet wins
    as.character(sorted[1,'Hospital.Name'])
}