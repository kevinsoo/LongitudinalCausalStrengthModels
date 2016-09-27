###########################################
########## Rescorla-Wagner model ##########
###########################################

#### calculates associative strength for different causes
# alpha = learning rate, default of .1
# df = data frame of cause(s) and effect (causes should include background cue)
# effectCol = column number of effect
# output includes final row showing final V values for cause(s)

RW <- function(alpha=.1, df, effectCol, final=TRUE) {
    e <- df[,effectCol] # extracts effect
    c <- df[,-effectCol] # extracts cause(s)
    i <- dim(c)[1] # num of observations
    j <- dim(c)[2] # num of causes
    
    # set up vectors for computing V and dV
    V <- createOutputCols("V", c, extra=TRUE)
    dV <- createOutputCols("dV", c, extra=TRUE)

    # create other vectors
    VTotal <- numeric(i) # vector for total V
    error <- c(numeric(i), NA) # vector for error
    
    # compute associative strength
    for (t in 1:i) {
        VTotal[t] <- sum(c[t,]*V[t,]) # calculate VTotal
        error[t] <- e[t]-VTotal[t] # calculate prediction error
        dV[t,] <- as.numeric(alpha*c[t,]*error[t]) # calculate change in V for each cause
        V[t+1,] <- as.numeric(V[t,]+dV[t,]) # update next trial's associative strength
    }
    
    # input end values
    dV[i+1,] <- NA
    VTotal[i+1] <- NA
    error[i+1] <- NA
    df[i+1,] <- NA
    t <- 1:(i+1)
    
    # return associative strengths in data frame WITH original data frame
    rw <- data.frame(t, df, V, VTotal, error, dV)
    if (final==TRUE) { return(rw) } # by default, final row is returned
    else if (final==FALSE) { return(rw[1:i,]) }
}
