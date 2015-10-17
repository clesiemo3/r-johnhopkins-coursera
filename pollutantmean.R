pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## initialization of variables
    con <- vector(length=0)
    for (i in id) {
        con[i] <- paste(directory,"/",sprintf("%03d",i),".csv",sep="")
    }
    #print(con) ##debug files list
    rd <- function(){
        #allow consistent read options across loop.
        fl <- read.csv(file=con[i],header=TRUE,stringsAsFactors = FALSE)
        return(fl)
    }
    for (i in id) {
        #print(i) ##debug loop count
        if(i==id[[1]]){
            dat <- rd()
        }
        else{
            dat <- rbind(dat,rd())
        }
    }
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    pollMean <- mean(dat[[pollutant]],na.rm=T)
    return(pollMean)
}