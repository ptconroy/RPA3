best <- function(state, outcome) {
    
    if(!is.element(state, state.abb)) {
        errmsg <- sprintf("Error in best(\"%s\", \"%s\") : invalid state", state, outcome)
        stop(errmsg)
    }
    
    db <- read.csv("data/outcome-of-care-measures.csv")
    
    hospitals <- db[db[,7] == state,] # get just the hospitals in our state
    
    hospitals[,11] <- as.numeric(hospitals[,11]) # make the rate numeric so . . .
    best_rate <- min(hospitals[,11]) # we can find the best rate.
    best_list <- hospitals[hospitals[,11] == best_rate,] # make a list of "best" hospitals
    sorted <- best_list[order(best_list[,'Hospital.Name']),] # bug here
    as.character(sorted[1,'Hospital.Name'])
}