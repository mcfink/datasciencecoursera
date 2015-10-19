corr <- function(directory, threshold=0){
    
    prepare_filename <- function(n){
        if(n<10){
            paste0(directory, "/00", n, ".csv")
        } else if(n<100){
            paste0(directory, "/0", n, ".csv")
        } else {
            paste0(directory, "/", n, ".csv")
        }
    }
    
    
    # load number of complete sets per file for all files
    all_cases <- complete(directory)
    
    # find id numbers for stations with more complete cases than threshold
    relevant_cases <- all_cases[which(all_cases$nobs > threshold),]
    
    # declare a vector equal in length to relevant cases
    correlation_vector <- vector(mode="numeric", length = length(relevant_cases$id))
    
    # get subset of data from env stations with more complete cases than the threshold
    index <- 1
    for(i in relevant_cases$id){
        file <- read.csv(prepare_filename(i))
        complete_cases <- file[which(complete.cases(file)),]
        correlation_vector[[index]] <- cor(complete_cases$nitrate, complete_cases$sulfate)
        index <- index + 1
    }
    
    # run NO3 / SO4 correlation for relevant stations
    
    correlation_vector    
}