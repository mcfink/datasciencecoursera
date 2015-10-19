pollutantmean <- function(directory, pollutant, id=1:332){
    mean_list <- vector(mode="numeric", length=length(id))
    
    prepare_filename <- function(n){
        if(n<10){
            paste0(directory, "/00", n, ".csv")
        } else if(n<100){
            paste0(directory, "/0", n, ".csv")
        } else {
            paste0(directory, "/", n, ".csv")
        }
    }
    
    remove_NA <- function(list){
        bad_list <- is.na(list)
        list[!bad_list]
    }
    
    index <- 1
    total <- 0
    total_readings <- 0
    for(i in id){
        a <-read.csv(prepare_filename(i))
        b <-remove_NA(a[,c(pollutant)])
        total <- total + sum(b)
        total_readings <- total_readings + length(b)
        index <- index + 1
    }
    
    total / total_readings
}