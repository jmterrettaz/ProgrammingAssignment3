
best <- function(state, outcome) {
        ## Read outcome data
        measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!state %in% measures$State) {
                stop("invalid state", call. = TRUE)
        }
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        # split data frame by state
        s <- split(measures, measures$State)
        
        # extract data frame for requested state 
        state_data <- s[state][[1]]
        
        # get outcome column
        out_data <- state_data[[out_col]]
        
        # get minimum (filter out not available rows)
        out_avail <- out_data != "Not Available"
        out_num <- as.numeric(out_data[out_avail])
        out_min <- min(out_num)
        out_min_c <- formatC(out_min,format='f', digits=1)
        
        # get hospital row
        hospital_row <- state_data[out_data == out_min_c,]
        
        # return hospital name
        hospital_row$Hospital.Name
}