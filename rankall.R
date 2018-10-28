

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        out_col <- ""
        if (outcome == "heart attack") {
                out_col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                out_col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                out_col <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome", call. = TRUE)
        }
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        # get outcome column
        out_data <- measures[[out_col]]
        
        # filter not available
        out_avail <- out_data != "Not Available"
        avail_data <- measures[out_avail,c("State", out_col, "Hospital.Name")]
        
        # sort data 
        avail_data[out_col] <- as.numeric(avail_data[[out_col]])
        sorted_index <- order(avail_data[,"State"], 
                              avail_data[,out_col], 
                              avail_data[,"Hospital.Name"])
        sorted_data <- avail_data[sorted_index,]
        
        split_data <- split(sorted_data, sorted_data$State)
        
        # get ranked hospitals
        r <- lapply(split_data, function(s) {
                hospital  <- if (num == "best") {
                        s[1, "Hospital.Name"]
                } else if (num == "worst") {
                        s[length(s[,"State"]), "Hospital.Name"]
                } else if (num > length(sorted_index)) {
                        "NA"
                } else {
                        s[num, "Hospital.Name"]
                }
                result <- data.frame(state=s[1,"State"], hospital)
        } )
        
        do.call('rbind', r)
}