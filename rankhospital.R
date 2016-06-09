##
## rank hospital returns the hospital name for the num-th ranked mortality rate for a
##  given outcome in the given state
##  - state: the two letter abbreviation for a state (from state.abb)
##  - outcome: must be "heart attack", "heart failure", "pneumonia"
##  - num: the ranking to return, can be an integer, "best" (for 1), or "worst"
##  - Note: returns NA for out of range ranking

rankhospital <- function(state, outcome, num = "best") {
    
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
    # so that we can rank them.
    
    hospitals[,tidx] <- suppressWarnings(as.numeric(as.character(hospitals[,tidx])))

    # sort by desired ranking, then by hospital name
    sorted <- hospitals[order(hospitals[,tidx], hospitals[,'Hospital.Name'], na.last = NA),]
    if (num == "best") {
        num <- 1
    } else if (num == "worst") {
        num <- nrow(sorted)
    }
    as.character(sorted[num,'Hospital.Name'])
}