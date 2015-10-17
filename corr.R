corr <- function(directory, threshold = 0) {
    source("complete.R")
    print(paste(Sys.time(),"Executing complete(directory)...",sep="-"))
    df <- complete(directory)
    print(paste(Sys.time(),"complete(directory) returned...",sep="-"))
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    print(paste(Sys.time(),"get raw data...",sep="-"))
    con <- vector(length=0)
    for (i in 1:nrow(df)) {
        con[i] <- paste(directory,"/",sprintf("%03d",i),".csv",sep="")
    }
    #print(con) ##debug files list
    rd <- function(){
        #allow consistent read options across loop.
        fl <- read.csv(file=con[i],header=TRUE,stringsAsFactors = FALSE)
        return(fl)
    }
    for (i in 1:nrow(df)) {
        #print(i) ##debug loop count
        if(i==1){
            dat <- rd()
        }
        else{
            dat <- rbind(dat,rd())
        }
    }
    #clean data
    good <- complete.cases(dat[[2]],dat[[3]])
    dat <- dat[good,]
    print(paste(Sys.time(),"raw data processed",sep="-"))
    cr <- vector(length=0)
    for(i in 1:nrow(df)){
        if(!is.na(df$nobs[i]) && df$nobs[i] >= threshold){
            subLog <- dat$ID==i
            subDat <- dat[subLog,]
            crx <- cor(subDat$sulfate,subDat$nitrate)
            cr <- c(cr,crx)
            if(is.na(crx)){print(i)}
        }
    }
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    return(cr)
    
}