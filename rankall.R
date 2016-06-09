##
## rankall returns the hospital name for the num-th ranked mortality rate for a
##  given outcome for all states
##  - outcome: must be "heart attack", "heart failure", "pneumonia"
##  - num: the ranking to return, can be an integer, "best" (for 1), or "worst"
##  - Note: returns NA for out of range ranking

rankall <- function(outcome, num = "best") {
    
    conditions <- c("heart attack", "heart failure", "pneumonia")
    
    cidx <- match(outcome, conditions) # cidx identifies which outcome we want
    if(is.na(cidx)) {
        stop("invalid outcome")
    }
    
    tidx <- c(11, 17, 23)[cidx] # tidx is the table column index for the desired outcome
    
    hospitals <- read.csv("data/outcome-of-care-measures.csv")
    
    ## doing this as split/sapply b/c I want to belive that there can be a map-reduce behind the scenes.
    s <- split(hospitals, hospitals$State)
    hitvector <- sapply(s, FUN = nthhospital, colidx = tidx, num = num)
    data.frame(hospital = hitvector[1,], state = hitvector[2,])
}

nthhospital <- function(x, colidx = colidx, num = num) {
    # helper function
    # nthhospital returns a vector with the name of the num-th hospital, ranked by the
    # value of the ranking column (at colidx, lowest first), and the name of the state
    # ties in ranking are resolved by alphabetical order of the hosptial name.
    # sort by desired ranking, then by hospital name
    
    # the data frame columns holding our mortality rates are factors
    # we need to convert them to numeric, silencing the "NAs introduced by coercion" warning,
    # so that we can rank them.
    
    x[,colidx] <- suppressWarnings(as.numeric(as.character(x[,colidx])))
    
    # Now, sort what we have, removing NAs . . .
    sorted <- x[order(x[,colidx], x[,'Hospital.Name'], na.last = NA),]
    
    state <- sorted$State[1] # get it now, so that we can have it even if the num gives an NA
    
    if (num == "best") {
        num <- 1
    } else if (num == "worst") {
        num <- nrow(sorted)
    }
    
    # build the return vector
    
    c(as.character(sorted[num,'Hospital.Name']), as.character(state))
}