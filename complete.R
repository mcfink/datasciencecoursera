complete <- function(directory, id=1:332){
    
    make_dataframe <- function(id_numbers){
        no_of_obs <- vector(mode="numeric", length = length(id_numbers))
        
        index <- 1
        for(i in id_numbers){
            a <- read.csv(prepare_filename(i))
            no_of_obs[[index]] <- sum(complete.cases(a))
            index <- index + 1
        }
        
        data.frame("id"=id_numbers, "nobs"=no_of_obs)
    }
    
    prepare_filename <- function(n){
        if(n<10){
            paste0(directory, "/00", n, ".csv")
        } else if(n<100){
            paste0(directory, "/0", n, ".csv")
        } else {
            paste0(directory, "/", n, ".csv")
        }
    }
    
    check_for_complete_cases <- function(list){
        complete.cases(list)
    }
    
    
    make_dataframe(id)
    
}