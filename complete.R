complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
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
    #clean data
    good <- complete.cases(dat[[2]],dat[[3]])
    dat <- dat[good,]
    
    df <- aggregate(sulfate ~ ID,data=dat,FUN=function(x){NROW(x)})
    names(df)[1] = "id"
    names(df)[2] = "nobs"
    df <- df[match(id,df$id),] #sort in the order we sent in the ids
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    return(df)
}