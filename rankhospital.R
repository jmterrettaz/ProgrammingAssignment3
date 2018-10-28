
rankhospital <- function(state, outcome, num="best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        # split data frame by state
        s <- split(measures, measures$State)
        
        # extract data frame for requested state 
        state_data <- s[state][[1]]
        
        # get outcome column
        out_data <- state_data[[out_col]]
        

        # filter not available
        out_avail <- out_data != "Not Available"
        avail_data <- state_data[out_avail,c("Hospital.Name", out_col)]

        # sort data 
        avail_data[out_col] <- as.numeric(avail_data[[out_col]])
        sorted_index <- order(avail_data[,out_col], avail_data[,"Hospital.Name"])
        sorted_data <- avail_data[sorted_index,]

        # get hospital row
        if (num == "best") {
                hospital_row <- sorted_data[1,]
        } else if (num == "worst") {
                hospital_row <- sorted_data[length(sorted_index),]
        } else if (num > length(sorted_index)) {
                return(NA)
        } else {
                hospital_row <- sorted_data[num,]
        }
        
        # return hospital name
        hospital_row$Hospital.Name
}