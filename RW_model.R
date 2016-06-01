###########################################
########## Rescorla-Wagner model ##########
###########################################

#### calculates associative strength for different causes
# alpha = learning rate, default of .1
# df = data frame of cause(s) and effect (causes should include background cue)
# effectCol = column number of effect

RW <- function(alpha=0.1, df, effectCol) {
    e <- df[,effectCol] # extracts effect
    c <- df[,-effectCol] # extracts cause(s)
    i <- dim(c)[1] # num of observations
    j <- dim(c)[2] # num of causes
    
    # set up vectors for computing V
    vectors <- matrix(nrow=2, ncol=j) # names of all columns
    for (k in 1:j) {
        vectors[1,k] <- paste("V", colnames(c)[k], sep="") # V columns
        vectors[2,k] <- paste("dV", colnames(c)[k], sep="") # deltaV columns
    }
    
    # create vectors
    V <- matrix(0, nrow=i+1, ncol=j) # V vectors for all causes
    colnames(V) <- vectors[1,] # name the columns
    dV <- matrix(0, nrow=i+1, ncol=j) # dV vectors for all causes
    colnames(dV) <- vectors[2,] # name the columns
    VTotal <- numeric(i+1) # vector for total V
    error <- numeric(i+1) # vector for error
    
    # compute associative strength
    for (p in 1:i) {
        VTotal[p] <- sum(c[p,]*V[p,]) # calculate VTotal
        error[p] <- e[p]-VTotal[p] # calculate prediction error
        dV[p,] <- as.numeric(alpha*c[p,]*error[p]) # calculate change in V for each cause
        V[p+1,] <- as.numeric(V[p,]+dV[p,]) # update next trial's associative strength
    }
    
    # return associative strengths in data frame WITH original data frame
    df[i+1,] <- NA
    t <- c(1:(i+1))
    return(data.frame(t, df, V, VTotal, error, dV))
}
